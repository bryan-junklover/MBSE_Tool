# ModelBench schema

This explains `schema.sql`: the conventions, what each table is for, which rules the
database enforces, which rules code must enforce, and how revisions, baselines,
evidence and impact work. `sample_survey_uav.sql` loads the Survey UAV project
shown in the wireframes. Read `BUILD_PLAN.md` first for context.

## 1. Storage

- One SQLite file per project in a workspace folder (`MODELBENCH_WORKSPACE_DIR`
  overrides the default). A project is copied or deleted by copying or deleting
  its file.
- `PRAGMA foreign_keys = ON` and WAL mode on every connection.
- `init_project_db(path)` runs `schema.sql` on creation **and on every open**.
  Every statement is `IF NOT EXISTS`, so running it twice is safe (tested).
  Later schema changes are numbered migrations (§9).
- Expect at most about 150 components, a few hundred requirements and a few
  thousand rows elsewhere. Load whole tables into R where convenient; no paging.

## 2. Conventions

| Convention | Rule |
|---|---|
| IDs | Stable, human-readable `TEXT` primary keys (`SYS-012`, `C-004`, `IF-002`). Allocated from `id_counter` by prefix, never reused, never changed by a rename. Each prefix has one format (`ID_FORMATS` in `R/db_entities.R`); stereotypes use `STY-`, sequence steps `ST-`. A prefix with no counter row starts at 1. |
| `rev` | Revisioned tables carry `rev`. Every write goes through the core write path, which increments `rev` and inserts the new state into `revision` in the same transaction (§6). The revisioned kinds are listed in `R/db_entities.R`: everything that describes the model. Settings and registries (`project_meta`, `id_counter`, `unit`, `glossary`), derived or logged rows and evidence aren't revisioned. A two-column key is addressed as `a\|b` in `revision`. |
| Tiered-coaching columns | `origin` (`human` / `ai`), `review_state` (`reviewed` / `unreviewed`) and `review_note`. AI-originated rows start `unreviewed`. The database refuses `ai` + `reviewed` without a note. |
| Placeholders | An explicit state, never a magic value: `value_state = 'placeholder'`, `range_state = 'placeholder'`, `hold_state = 'placeholder'`, `timing_state = 'placeholder'`. In requirement text a placeholder is any `[bracketed token]`. |
| Who sets values | `property_value.set_by` is `human` or `code`. There is no `ai` option: the AI can't set a value. |
| Timestamps | ISO-8601 UTC text. |
| JSON columns | Suffix `_json`. Canonical JSON (sorted keys, no whitespace) wherever it is hashed. |

## 3. Entity map

```
requirement ──parent_id──▶ requirement
     │  link: satisfied_by ─────────────▶ element (component)
     │  link: verified_by ──────────────▶ test_case | sm_assertion | scenario
     │  requirement_term ───────────────▶ element (term resolution)
element (component | function | external) ──parent_id──▶ element
     │  port ──interface_id──▶ interface ──▶ interface_field
     │  connector: port ──▶ port
     │  property_value (owner_kind = element | connector | interface | requirement)
     │  link: allocated_to (function ──▶ component)
     │  state_machine ──▶ sm_state, sm_event (+ sm_event_scope), sm_transition,
     │                    sm_variable, sm_ignore, sm_assertion ──▶ sm_run
scenario ──▶ seq_lifeline (element), seq_step (interface, field)
         ──▶ act_node (performer element, reads properties), act_edge
revision ◀── baseline_item ──▶ baseline
llm_call ◀── ai_proposal, llm_cache, chat_message, requirement.coach_call_id
```

## 4. Tables by area

**Project.** `project_meta` holds key-value settings (scope, coaching policy,
local-only switch, spend cap, redaction list, coach wait). `id_counter` allocates
ids. `unit` is the unit registry; `si_factor` and `si_offset` let code convert and
compare units. `glossary` holds definitions and aliases. When `element_id` is set,
the term is an alias of that element, so "autopilot" resolves to Autopilot Software.

**Properties.** `stereotype` and `property_def` describe what can be recorded.
`rollup` on a definition (`sum`, `max`, `min`) drives budget roll-ups.
`property_value` holds one value per owner and definition. Because its owner is
polymorphic, core must check that the owner exists, both on write and in an
integrity test. `budget` sets a limit on a rolled-up property at a scope element.
`analysis_result` caches the output of registered R analyses (endurance, for example), and `analysis_limit` holds the limit an analysis is judged against, since `budget` needs a property and an analysis has none. Property values are per unit: a roll-up multiplies them by `multiplicity`.

**Structure.** `element` covers components, functions and external actors. The
functional and physical models use the same tables and are told apart by `kind`.
Functional flows are connectors between ports on function elements, typed by
`interface` rows with `layer = 'functional'`. `interface` has a `flow_type` of
data, energy or matter, which drives the line style in the Flows view.
`interface_field` holds typed fields with a unit and a range in one of three
states. `port` and `connector` are as in System Composer. Interface compatibility
between the two ends of a connector is a gate check in code: both ports must name
the same interface, and their directions must be compatible.

**Requirements.** `requirement` holds the text plus the gate result (`gate_score`,
`gate_pass`, `gate_json`) and the coach state. The database enforces four rules
for a **committed** requirement: it passed the gate, its text has no `[…]`, it is
`reviewed`, and its coach state is `reviewed` or `uncoached`. Drafts may hold
placeholders and can fail the gate. `requirement_term` stores how each term the
gate extracted resolved: to a model element (`model`), as not a model term
(`not_model`), or still open (`unresolved`). `requirement_conflict` records a
conflict only when the coach quoted the other requirement verbatim.
`import_triage` is the queue for imported rows that fail the gate.

**Links.** One `link` table holds trace, allocation, realisation and verification
links. Use `parent_id` for the requirement tree and `derives_from` for derivation
that crosses the tree. A link chosen from AI candidates has `origin = 'ai_candidate'`
and must carry a `rationale` and an `ai_proposal_id`; the database enforces both.
Links made by hand may leave `rationale` empty.

**Evidence.** `test_case` and `verification_record`. A verification record
stores the context it was recorded against (§7).

**Behaviour.**
- A `scenario` can have a sequence view (`seq_lifeline`, `seq_step`), an activity
  view (`act_node`, `act_edge`), or both.
- Sequence steps reference an interface and, optionally, a field. Code checks
  that the field exists; a missing field is drawn amber.
- Activity nodes record which properties they read in `reads_json`. Behavioural
  impact depends on it.
- A state machine belongs to one element.
- `sm_event_scope` lists the states in which an event must be handled. The
  completeness check (`v_sm_unhandled`) reports each in-scope pair that has
  neither a transition nor an `sm_ignore` row. An ignore needs a rationale.
- `sm_assertion` is an executable check tied to a requirement. `sm_run` stores
  each result with the model hash it ran against.

**Views and layout.** `view_def` holds saved views and filters. `layout` holds
diagram positions per view. Neither is revisioned: moving a box is not a model change.

**Revisions and baselines.** See §6.

**AI layer.**
- `provider` holds no keys. Keys come from the OS keyring or environment variables.
- `task_route` maps each AI task to a trigger, a provider, a model and a fallback.
- `llm_call` records every call, including blocked ones: the exact system prompt
  and message sent, the schema, the response, usage, cost, any error, and the
  model hash at the moment of the call.
- `llm_cache` is keyed by task plus the hash of the exact input text and the
  prompt version. Failed calls are never cached.
- `ai_proposal` is the review queue. Keeping a proposal requires `engineer_note`
  and dismissing one requires `dismiss_reason`; the database enforces both.
  `candidates_json` maps the numbered handles shown to the model back to real ids.
  This mapping is never sent to the model.
- `chat_message` and `audit_log`.

## 5. What the database enforces, and what code must enforce

| Rule | Where |
|---|---|
| Committed requirement: gate passed, no placeholder, reviewed | DB `CHECK` + core |
| AI-originated row can't be `reviewed` without a note | DB `CHECK` on every tiered table |
| Link from AI candidates needs a rationale and a proposal | DB `CHECK` |
| Keeping or dismissing a proposal needs a note or reason | DB `CHECK` |
| Ignore needs a rationale | DB `CHECK` |
| Values are set by a person or by code, never the AI | DB `CHECK` (`set_by`) |
| A `set` value has a value; a placeholder has none | DB `CHECK` |
| Polymorphic owners and link ends exist | Core write path + integrity test |
| Connector ends share an interface; directions are compatible | Element gate (core) |
| No cycles in `parent_id` (element, requirement) | Core write path |
| Interface field unit exists in the registry | FK + gate |
| AI payloads contain no numbers | Proposal validator (core) |
| Stale evidence | Core `stale_evidence()` (§7) |
| Baseline refused while blockers exist | Core `create_baseline()` using `v_baseline_blockers` + `stale_evidence()` |

Every DB-enforced rule has a negative test that tries to break it. The checks
used to build this file already pass against SQLite (see BUILD_PLAN, Phase 0).

## 6. Revisions, model hash and baselines

**Write path.** Core exposes one writer per entity (`write_element()`,
`write_requirement()`, …), and each calls `write_entity(kind, id, fields, author,
note)`. Inside one transaction it:

1. validates the change (gate checks for that entity type);
2. updates the row and increments `rev`;
3. serialises the row as canonical JSON (child collections such as interface
   fields are revisioned as their own entities);
4. inserts `revision(entity_kind, entity_id, rev, hash = sha256(json), data_json)`;
5. inserts an `audit_log` row;
6. invalidates the area for the UI (`invalidate_db("model")`, …).

Every revision written by one user action shares a `change_set` id. Swapping the
battery, for example, updates both capacity and mass in one change set. A delete
writes a final revision with `deleted = 1`. Nothing writes to these
tables any other way. A static test greps for `dbExecute` calls outside `R/db_write.R`; `R/db_init.R`, which creates the schema, runs migrations and loads the raw sample, is the one exception. Tables that aren't revisioned are written through `write_plain()` in the same file.

Deleting an entity first writes a final revision for each revisioned row that goes with it (ports, connectors, property values, links and so on), because the database's cascades leave no history. Step 6 is the caller's: the core has no reactivity, so `write_entity()` returns the `area`, and the Shiny shell invalidates it.

**Model hash.** `model_hash()` is the sha256 of the sorted list of
`(entity_kind, entity_id, rev)` for every live, revisioned entity. Every
`llm_call` and `sm_run` stores it, and the UI compares it to show "this result is
from an older model".

**Baseline.** `create_baseline(id, name, note)`:

1. collects blockers: `v_baseline_blockers` plus `stale_evidence()`. If there are
   any, it refuses and returns them; this is the banner on Health;
2. otherwise writes `baseline` and a `baseline_item` for every live entity at its
   current `rev`, excluding requirement drafts and deprecated requirements.

The sample builder is the one caller that skips step 1, through the internal
`.write_baseline_items()`: BL-03 is historical, and six of today's blockers were
already open when it was taken.

**Compare.** Diff two item sets, either two baselines or a baseline and the
working copy, by `(entity_kind, entity_id)`. The working copy's set uses the same
exclusions as a baseline (`baselineable_entities()`), so drafts never show as added:

- *added*: present in B only;
- *removed*: present in A only, or deleted in B;
- *modified*: `rev` differs. A field-level diff comes from the two `data_json` values.

The UI groups these entity changes by the `change_set` of their latest revision.
Each change card is one change set, titled by its first entity.

**Rollback.** First save a safety baseline (`is_safety = 1`), then write each
entity's baselined `data_json` back through the write path as a new revision.
Rollback never rewrites history.

## 7. Evidence staleness

A verification record is current only while everything it depended on is
unchanged. When it is recorded, core builds its context, a list of
`(kind, id, rev)` entries:

- the requirement;
- every element linked to it by `satisfied_by`, and their property values;
- every input of any analysis the requirement's verification relies on (from the
  analysis registry);
- for an `sm_assertion`: the state machine's states, transitions, events and variables.

The context is stored as `context_json` and its hash as `context_hash`.
`stale_evidence()` rebuilds each record's context from the current revisions and
reports any record whose hash no longer matches, along with the entries that
changed. On the sample project TC-007 is stale because `V-0013`
(battery_capacity) went from rev 1 to rev 2.

## 8. Impact traversal (deterministic)

`impact_set(changes, max_hops = 3)` walks an igraph built from the tables. Edge
types, followed in both directions unless marked:

| From | Edge | To |
|---|---|---|
| property_value | owned by | element / connector |
| element | port + connector | neighbouring element (1 hop per connector) |
| element | `satisfied_by` (reverse) | requirement |
| function | `allocated_to` | component |
| requirement | parent / `derives_from` | requirement |
| requirement | `verified_by` | test_case / sm_assertion / scenario |
| property_value | rollup | budget at every ancestor scope |
| property_value | analysis input | analysis_result, then the requirement whose budget or verification uses it |
| interface / interface_field | used by | port, seq_step, sm_event |
| element | lifeline / performer / `reads_json` | scenario |
| element | owns | state_machine |

Each impacted item carries its shortest path (shown as "Path" in the impact table)
and a hop count. The set is complete and repeatable, so tests pin it for the
sample change. The AI only rates the significance of each item. It sees items as
`I1…In` and returns `{n, significance: High|Medium|Low, reason}`. Numbers in
the narrative come from `analysis_result` and the roll-ups, which are passed to
the model as quoted facts.

## 9. Migrations

`project_meta.schema_version` starts at `1`. A migration is a file
`migrations/NNN_description.sql` (or an R function) that runs once, inside a
transaction, when the stored version is lower than NNN, then bumps the version.
`init_project_db()` runs `schema.sql` (whose `IF NOT EXISTS` statements create
new tables) and then any pending migrations. Never edit a released migration.

## 10. Sample project facts (used by tests)

From `sample_survey_uav.sql` (the working copy, after the battery change). Stored gate
scores are what `check_requirement()` computes, and a test keeps them in step:

| Check | Expected |
|---|---|
| `v_placeholders` | 4: return_reserve (C-003), mass (C-010), heading_accuracy (IF-002), hold_timeout hold (SM-001) |
| `v_draft_placeholders` | 2: SYS-031, SYS-007a |
| `v_unreviewed` | 3: C-010, F8, ST-11 |
| `v_sm_unhandled` | 3: battery_low × Hold position, link_lost × Return to home, link_lost × Land |
| `v_unconnected_ports` | 1: Payload Camera `trigger_in` |
| `v_unallocated_requirements` | 1: SYS-021 |
| `v_unallocated_functions` | 1: F8 |
| `v_baseline_blockers` | 7, plus 1 stale evidence (TC-007) from code = 8, as on the Health wireframe |
| Mass roll-up at C-000 | 3.31 kg of 3.60 (Propulsion: 0.30 kg × 4) |
| Power roll-up at C-000 | 412 W of 450 (Propulsion: 95 W × 4) |
| Endurance analysis | 31.0 min (BL-03: 37.0 min) against `analysis_limit` 35 min, so SYS-001 fails |
| Functional flows | 9, each with a `realized_by` link to a physical connector |
| Scenarios | 4: SC-001, SC-002, SC-003 (activity), SC-004 |
| Assertion A-001 (SYS-012) | pass: Survey mission → Return to home 3.0 s after `gnss_lost` |
| BL-03 vs working copy | 3 change sets touching 4 entities: {V-0013, V-0003}, {SYS-002}, {FLD-0005 added} |

The wireframes show requirement counts from a fuller 47-requirement spec. The
sample holds 13 requirements, so tests assert the sample's numbers, not the
wireframe's.
