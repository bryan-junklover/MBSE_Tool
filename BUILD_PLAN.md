# ModelBench: build plan

For the coding agent building the app. Read the documents in this order:

1. **BUILD_PLAN.md** (this file): what to build, in which order, and how each phase is accepted.
2. **SCHEMA.md**, `schema.sql` and `sample_survey_uav.sql`: the data model and the sample project.
3. **HANDOFF_BEHAVIOUR_AND_STYLE.md**: how each screen behaves and looks.
4. The wireframe canvas "MBSE App Wireframes", 15 artboards (named in the handoff, §2).
   The wireframes fix layout, content and behaviour. They are not pixel specs.
5. The Requirements Coach technical overview (the owner has it). Its gate, LLM
   patterns and Shiny lessons are reused here, as named below.

"ModelBench" is a working name.

## 1. What we're building

A single-user model-based systems engineering (MBSE) tool, built in R with Shiny
and bslib, that does a useful subset of what MATLAB System Composer does, plus an
AI layer through ellmer. It is **requirements-first**: the app opens on Write.
Requirements lead into a functional model, then a physical model, then behaviour.
Baselines and change impact tie them together. The AI coaches the engineer rather
than doing their work, under a **tiered coaching** policy.

Target scale: up to 150 components per model and a few hundred requirements, for
one user. Multi-user comes later: keep `created_by` and `changed_by` populated
and avoid global state, but don't build authentication.

### In scope

- Requirements authoring with a deterministic gate and an AI coach.
- Architecture (structure, flows, functional) with interfaces, ports and connectors.
- Allocation and traceability.
- Behaviour: sequence and activity scenarios, and executable state machines.
- Baselines, diffs and change impact (structural and behavioural).
- A Health page, a review queue, a read-only chat, and exports.

### Out of scope for v1

Physics simulation, code generation, AUTOSAR, full SysML diagram fidelity,
authentication, concurrent editing, cloud hosting, and dark mode.

## 2. Principles (each one has a test)

1. **Rigor first, then intelligence.** Each entity type has a deterministic gate.
   The gate decides when the AI runs, and it re-checks anything the AI produces.
2. **Tiered coaching**, enforced in core code and the database, not by the UI:

   | Kind | AI may | Always the engineer's |
   |---|---|---|
   | Requirement wording | ask questions; show example rewrites, folded, as word diffs | every word committed |
   | Values | point out that one is missing | every number (the AI writes `[value]`) |
   | Links | rank numbered candidates, argue as questions | choosing the link, plus a note |
   | Structure | sketch unreviewed drafts | reviewing each item, with a note |
   | Explanations | write freely | decisions; numbers come from code |

3. **Code computes every number.** Roll-ups, analyses, impact sets and Health
   counts are deterministic R. The AI never provides a figure that ends up stored
   or presented as fact.
4. **The model never names an ID.** Candidates are ranked locally and sent to the
   model as `C1…Cn` (impact items as `I1…In`). Answers are mapped back and
   validated. Anything that doesn't resolve is dropped and logged.
5. **Provenance.** Every AI call writes an `llm_call` row. Each feature has one
   prompt builder, shared by the call and the "See the prompt" dialog.
6. **Snapshot at invoke.** Every async AI task captures the project, the model
   hash and its inputs when it starts. Its result is stored against that snapshot.
7. **One write path.** All mutations go through core `write_*()` functions (SCHEMA §6).

## 3. Architecture

```
modelbench/                  R package. No Shiny dependency. All logic, all tests.
  R/db_*.R                   init_project_db, migrations, write path, queries
  R/gate_*.R                 requirement gate (ported) + element/interface/sm gates
  R/terms.R                  term extraction + resolution against the model
  R/graph.R                  igraph build, impact_set(), stale_evidence()
  R/rollup.R, R/analysis.R   budgets; analysis registry (endurance, ...)
  R/sim_*.R                  expression parser, state machine evaluator, assertions
  R/ai_*.R                   providers, routing, prompt builders, schemas,
                             candidates, proposal validators, async wrappers
  R/export_*.R               Markdown, CSV, ReqIF, Quarto ICD, SysML v2 text (stretch)
  inst/sql/schema.sql        this schema;  inst/sql/migrations/
  inst/sample/               sample_survey_uav.sql
  R/sample.R                 build_sample_project() (tools/ holds the thin script)
  tests/testthat/
app/                         Shiny shell (thin): app.R, R/mod_*.R, www/
widgets/mbdiagram/           htmlwidget package: block, flow, functional, state-machine,
                             activity editing (JointJS + elkjs)
widgets/seqview/             htmlwidget: sequence diagram renderer (SVG) with click events
tools/                       build_sample_project.R, llm_eval.R
```

Why the split: the Requirements Coach found that Shiny's reactive traps took up
much of its debugging. Keeping all logic in a package with no reactivity makes it
testable without a session, and leaves room for an API layer when multi-user
arrives.

### Stack

| Need | Choice |
|---|---|
| UI | shiny, bslib (Bootstrap 5), reactable for tables, shinychat for chat |
| Diagrams | custom htmlwidgets: **JointJS** (`@joint/core`, MPL-2.0) for ports and links, plus **elkjs** for port-aware auto-layout. Decided by a Phase 2 spike; the fallback is Cytoscape.js |
| Storage | DBI + RSQLite, WAL |
| Graph | igraph |
| LLM | ellmer (Anthropic, OpenAI, and OpenAI-compatible for LM Studio), with structured output via `type_object()` / `type_enum()` |
| Async | `ExtendedTask` + promises (+ mirai) |
| Hashing / JSON | digest or openssl (sha256), jsonlite with canonical output |
| Secrets | keyring, falling back to environment variables |
| Documents | quarto (ICD), xml2 (ReqIF), readxl / openxlsx2 (import, export) |
| Tests | testthat, `testServer()`, shinytest2 for browser checks |
| Reproducibility | renv |

Pin versions with renv when you start. For JS libraries, vendor exact versions
into the widget's `inst/htmlwidgets/lib`; don't load them from a CDN at runtime.

## 4. Phases

Each phase ends at an **exit gate**. Don't start the next phase until it passes.
Wireframe artboards are listed by title (handoff §2).

### Phase 0: Foundations

**Build**
- Package skeleton, renv, test runner script, and a lint step.
- A test guard that replaces ellmer's chat constructors with functions that throw,
  so no test can reach the network.
- `init_project_db()` loads `schema.sql` (idempotent) and runs migrations.
- ID allocator; `write_entity()` with canonical JSON, sha256, `revision` and `audit_log`; `model_hash()`.
- Sample loader for `sample_survey_uav.sql`, and `tools/build_sample_project.R`.
  The script builds the project through the write path in BL-03 form (battery
  5000 mAh, Power System 1.47 kg, SYS-002 at 5 km, no heading_accuracy), creates
  BL-03 without the blocker check (SCHEMA §6), then applies the three changes.

**Exit gate**
- `schema.sql` runs twice without error.
- Every database rule in SCHEMA §5 has a negative test.
- The sample loads with no foreign-key violations, and the SCHEMA §10 view counts match.
- A revision round-trip test passes (write → revise → read back by rev).
- No test reaches the network.

### Phase 1: Write workspace, no AI

Artboards: *0 · Write*, *3 · Write › Specification*.

**Build**
- Port the gate from the Coach's `R/logic_checks.R` (commit `79a11d9`) together
  with its tests and the `sample_requirements.txt` fixture (10 pass, 9 fail).
  Replace the "actor not in glossary" check with **term resolution**: the actor
  and noun phrases are matched against element names and glossary aliases.
  Matching is case-insensitive and ignores articles and plurals.
- Write screen:
  - text area and EARS builder;
  - parsed sentence;
  - gate checklist, refreshed on a 300 ms debounce;
  - Terms panel with "Add as property…", "Add interface field…" and "Not a model term";
  - Commit.
- Specification screen: outline with parent/child order, table (reactable),
  inspector with details saved on change, and import (CSV, xlsx, txt) with a
  sequential triage queue.
- Project settings: scope text, glossary, ID prefixes.
- Status strip, at first showing only placeholders and unreviewed counts.

**Exit gate**
- The gate fixture is locked.
- Commit is refused for gate failures and placeholders at the UI, in core, and in the database.
- Term resolution is tested on the sample (e.g. "power system" → C-003, "autopilot" → C-009).
- `testServer()` tests cover Write and Specification.

### Phase 2: Model core

Artboards: *1 · Architecture*, *13 · Interface dictionary*, *0 · Analyze › Health*.

**Build**
- Spike first, about two days: render the sample plus a synthetic model of 150
  components and 600 ports in JointJS with elkjs. Test drag, port-to-port connect,
  drill-down into a sub-architecture, and selection events to Shiny. Record the
  decision in `docs/decisions/diagram.md`.
- Built-in `view_def` rows, seeded by `init_project_db()`.
- Model tree and inspector editing: elements, stereotypes, property values
  (including placeholders), ports, connectors.
- Interface dictionary: interfaces, fields, units and ranges, plus a "where used" panel.
- Element gates: connector compatibility, unknown units, missing interface on a
  port, and `parent_id` cycles.
- `mbdiagram` Structure view:
  - tools: Select, Component, Port, Connect, Group into subsystem;
  - double-click drills down;
  - undo and redo (a command stack in core);
  - layout saved per view; auto-layout.
- Roll-ups and budgets; the analysis registry with `endurance`.
- Health page, computed with no AI calls.

**Exit gate**
- The 150-component model renders in under 1 s and drags smoothly.
- Edits made on the diagram and in the tree reach the same database rows.
- Health shows the SCHEMA §10 numbers, except stale evidence (Phase 4) and the
  state-machine count (Phase 6).
- Roll-ups give 3.31 kg and 412 W, and endurance gives 31.0 min.

### Phase 3: AI foundation and requirement coach

Artboards: *0 · Write* (coach), *5 · Analyze › Review queue*,
*7 · Project: providers, routing & coaching policy*.

**Build**
- Default `provider` and `task_route` rows, seeded by `init_project_db()`.
- Provider layer: `new_llm_chat_for(task)` is the only place chat clients are
  built. It covers task routing with fallback, the local-only switch, redaction of
  listed terms before cloud calls, the spend cap, and plain-word errors (Coach §6
  list plus `spend_cap` and `local_only`).
- Usage logging on every call, shown in the status strip.
- Prompt builders that return `list(system, message, schema)`, and a "See the
  prompt" dialog for each AI panel. The authoring rules inside prompts are
  generated from the gate's constants.
- Requirement coach on Write: two clocks, the current-assessment rule, light
  check on every edit (local model), deep check on demand. The example rewrite is
  re-gated and shown folded as a word diff. Scope gaps run after a commit.
- `ai_proposal` store and the Review queue screen. Kept and dismissed items require a note.
- Validators:
  - numbered candidates;
  - "no numbers in sketches": any digit in a sketch payload, outside names copied
    from the model, is replaced with `[value]`, and the proposal is flagged and
    counted under "values the model tried to invent";
  - schema validation of every response.

**Exit gate**
- Each prompt builder has a mocked test showing the dialog text equals the text sent.
- `tools/llm_eval.R`, a billed run made deliberately and never in CI, reports: gate
  pass rate of example rewrites, invented-number rate after validation (must be 0),
  and latency per provider.
- A manual browser check covers debounce timing and commit enablement.

### Phase 4: Traceability, functional model, allocation, flows, evidence

Artboards: *4 · Write › Traceability*, *12 · Functional architecture*,
*11 · Flow view*.

**Build**
- Links with the rationale rules.
- Trace matrix (requirements × components), with drafts included and shaded.
- Trace coach and allocation coach using numbered candidates. The Link button
  stays disabled until a rationale is entered.
- Functional view (functions as elements, functional interfaces), allocation, and
  a realisation check that every functional flow has a physical connector.
- Flows view: line encoding by flow type, and quantities from connector property values.
- Test cases, verification records with context hashes, and `stale_evidence()`.

**Exit gate**
- A link chosen from candidates can't be saved without a note.
- On the sample, TC-007 is reported stale after the battery change.
- The realisation check passes on the sample's functional flows.

### Phase 5: Baselines, change impact, stakeholder view

Artboards: *6 · Analyze › Baselines & impact*, *2 · Architecture (stakeholder view)*.

**Build**
- `create_baseline()` with the blocker check, compare, and rollback via a safety baseline.
- `impact_set()` (SCHEMA §8) and the impact table showing path and hops.
- AI significance rating over `I1…In`, and a narrative with Technical and Stakeholder tabs.
- Stakeholder view:
  - plain summaries (`element.plain_summary`);
  - status chips;
  - "Are we on track?" meters from budgets and analyses;
  - open questions in plain words;
  - export as a PDF brief.

**Exit gate**
- On the sample, BL-03 compared with the working copy shows exactly 3 change sets,
  touching 4 entities.
- The battery impact set matches a pinned fixture.
- Creating BL-04 is refused with 8 blockers listed.

### Phase 6: Behaviour

Artboards: *8 · Behavior*, *9 · Behavioral impact*, *10 · Executable state machine*.

**Build**
- Sequence scenarios: a structured step editor, plus `seqview` rendering with
  click-to-select. A step whose interface field doesn't exist is drawn amber.
- Activity diagrams: nodes and edges in `mbdiagram`, with ELK layered layout and
  `reads_json` editing.
- State machine editor in `mbdiagram`; the guard and effect expression parser
  (handoff §5.8), which never uses `eval()`; the simulator; the completeness check;
  assertions and runs.
- AI behaviour sketch (every step `unreviewed`, timings and guards as
  placeholders) and behaviour analysis questions.
- Behavioural impact: scenarios reached by `impact_set()`, with changed-input
  steps highlighted, and before/after/side-by-side views.

**Exit gate**
- A-001 passes on the sample.
- `v_sm_unhandled` returns exactly 3 pairs.
- Property tests on the evaluator pass (deterministic, terminates, rejects unknown identifiers).
- Behavioural impact for the battery change lists 4 scenarios.

### Phase 7: Chat, exports, polish

Artboards: *11* (chat panel), plus export buttons wherever they appear.

**Build**
- Chat panel (shinychat), docked on the right. It reads the model only through
  tools such as `get_element`, `get_interface`, `rollup`, `impact_set` and
  `list_requirements`. It can't write; "Attach to proposal" adds its answer to an
  existing `ai_proposal`.
- Exports: ICD through a Quarto template, ReqIF with links as `SPEC-RELATIONS`,
  CSV, Markdown, and the requirements PDF. SysML v2 textual export is a stretch goal.
- Accessibility pass, user guide, and bundled fonts.

**Exit gate**
- The ICD renders for the sample.
- The ReqIF export opens in a validating parser.
- A browser checklist (handoff §8) passes.

## 5. Cross-cutting rules (lessons from Requirements Coach)

- **Never enable commit because "some assessment exists."** Record the text each
  review belongs to (`assessed_text`) and require that it equals the current text.
- **Trigger the coach observer on the draft text only,** never on gate output. The
  gate depends on database state, and triggering on its output caused an infinite
  loop in the Coach.
- **`debounce()` doesn't dedupe by value.** Keep a `last_processed` guard keyed
  on project and text.
- **No key, no lockout.** Without a configured coach, commit runs on the gate
  alone and marks the requirement `uncoached`.
- **Per-area invalidation:** `db_dep(area)` and `invalidate_db(area)` with areas
  `spec`, `model`, `behavior`, `ai`, `baseline`, `settings` and `layout`.
- **Pure renderers:** `render_*()` functions take data and return HTML, so they
  can be tested without a session.
- **Shiny traps the Coach found:**
  - force `display: block` on outputs;
  - call `outputOptions()` only after the output is defined;
  - popover inputs don't exist until the popover first opens;
  - fingerprint the theme URL;
  - reset the fill on outline task buttons.
- **Failures never crash the session.** Wrap async calls so that synchronous
  throws become rejected promises.
- **Dismissals persist.** A backfill from the cache must check for dismissed rows,
  so dismissed items don't come back.

## 6. Decisions made (change only with the owner)

| Decision | Choice |
|---|---|
| Entry point | Requirements-first: the app opens on Write |
| Coaching policy | Tiered (Strict and Assistive exist as settings but are v2) |
| AI triggers | Light requirement check on every edit with a local model; everything else on demand |
| State machines | Executable; requirement assertions run against them |
| Block diagram views | Structure, Flows (data / energy / matter), Functional |
| Diagram editing | On the diagram and in the tree; both write through the same core functions |
| Providers | Anthropic, OpenAI, LM Studio (OpenAI-compatible), routed per task with a fallback |
| System of record | This app owns requirements and model; ReqIF is export only |

## 7. Open questions for the owner

1. The diagram library is confirmed only after the Phase 2 spike.
2. Should the Strict and Assistive coaching policies ship in v1, or stay hidden?
3. Which ICD template and section order should the export follow? A sample
   document would help.
4. Should ReqIF import (round-trip) happen in v1, or stay export only?
