# ModelBench: behaviour and styling handoff

How each screen behaves and how it should look. Read with `BUILD_PLAN.md` (phases,
architecture) and `SCHEMA.md` (data). Section numbers here are referenced from
both of those documents.

## 1. Using the wireframes

The canvas "MBSE App Wireframes" holds 15 interactive artboards, and they are the
source for layout, content hierarchy and behaviour.

- **What they fix:** which panels exist, what each contains, the order of
  information, and which actions are offered.
- **What they don't fix:** exact pixel positions. Build with bslib layouts and the
  tokens in §6, not by copying absolute positions.
- **Their data is the Survey UAV sample** (`sample_survey_uav.sql`). Model-level
  numbers match the sample. Requirement counts on Specification and Health come
  from a fuller 47-requirement spec; tests use the sample's numbers (SCHEMA §10).
  Two wireframe details give way to the sample: artboard 9's "fixed 1250 mAh"
  reserve is `[value]` in the sample, and Propulsion's mass and power are shown per
  unit (0.30 kg, 95 W, × 4) with the 1.20 kg and 380 W totals in roll-ups.
- **Interactive parts:** Play works on the Impact tabs (Technical / Stakeholder)
  and on the state machine simulator, which is a working reference for the
  simulator semantics in §5.8.
- **One deliberate departure:** interactive control borders use `--line-control`
  (#85888E), not the wireframes' #9A9DA3, to meet the 3:1 contrast rule for UI
  components.

## 2. Screen inventory

| Artboard | Workspace › tab | Purpose | Phase |
|---|---|---|---|
| 0 · Write: requirements-first landing | Write › Draft | Author one requirement: gate, terms, coach, commit | 1, 3 |
| 3 · Write › Specification | Write › Specification | Outline, table, inspector, deep check, import | 1, 3 |
| 4 · Write › Traceability | Write › Traceability | Requirements × components matrix, allocation questions | 4 |
| 1 · Architecture (engineer view) | Model › Structure | Block diagram editing, tree, inspector | 2 |
| 2 · Architecture (stakeholder view) | Model › Structure, audience = Stakeholder | Plain summaries, status, on-track meters, open questions | 5 |
| 11 · Flow view + chat panel | Model › Flows | Data/energy/matter flows with quantities; chat docked | 4, 7 |
| 12 · Functional architecture & allocation | Model › Functional | Functions, functional flows, allocation | 4 |
| 13 · Interface dictionary | Model › Interfaces | Interfaces, fields, units, ranges, where used | 2 |
| 8 · Behavior: scenario diagrams | Behavior › Sequence | Scenario list, sequence editor, consistency, AI questions | 6 |
| 9 · Behavioral impact | Behavior › Activity (impact mode) | Affected scenarios, activity diagram, changed inputs | 6 |
| 10 · Executable state machine | Behavior › State machine | Editor, simulator, assertions, completeness | 6 |
| 0 · Analyze › Health | Analyze › Health | Baseline readiness and model health, no AI | 2 → 6 |
| 5 · Analyze › Review queue | Analyze › Review queue | Structural gaps and AI proposals | 3 |
| 6 · Analyze › Baselines & impact | Analyze › Baselines & impact | Diff, impact table, narrative, create baseline | 5 |
| 7 · Project: providers, routing & coaching policy | Project | Providers, task routing, coaching policy, data protection | 3 |

The Project workspace also holds scope, glossary, ID prefixes and import settings
(Phase 1). They aren't drawn; use the Settings artboard's layout pattern.

## 3. App shell

**Top bar** (ink background):
- product name, then project and baseline context in mono: "Survey UAV · working copy · based on BL-03";
- workspace navigation: **Project, Write, Model, Behavior, Analyze**;
- an "Ask the model" button that opens the chat drawer.

The app opens on Write. The last workspace and tab are remembered per project in
`localStorage`, wrapped in try/catch.

**Status strip** (directly under the top bar, `--status-bg`, mono 12px). Left to right:
1. Coach state per provider kind ("local ready · cloud ready"; "local down" in amber).
2. The policy in force ("Policy: tiered").
3. Blocker counts, as a link to Health: "3 unreviewed AI drafts · 4 placeholders".
4. Pushed right: today's AI calls and cost.

The strip refreshes from `db_dep("ai")` and `db_dep("model")`.

**Workspace tab bar** (white): segmented tabs on the left, the screen's own
actions on the right. The Model workspace adds the view switcher Structure,
Flows, Functional, Interfaces, and the audience toggle Engineer / Stakeholder.

**Body:** a three-column flex layout:
- left navigator, 220–260px;
- main area, flexible, `min-width: 0`;
- right inspector or panel, 300–380px.

Use `flex-wrap: wrap`, so that below about 1100px the side columns stack under
the main column. Wide tables scroll inside their own box.

**Chat drawer:** right-docked, 360px. It pushes the content on wide screens and
overlays on narrow ones. It stays open across navigation.

## 4. Copy and voice

- **The AI asks; it doesn't tell.** Every AI finding, gap and allocation prompt
  is phrased as a question about the system. For example: "What brings the
  aircraft down if the Flight Controller fails?", not "Add a flight termination unit."
- **Show the source of every claim:** "code" or "no AI", "local", or "cloud",
  with the provider and prompt version in mono at the foot of AI panels, next to
  "See the prompt".
- **Placeholders are literal and visible:** `[value]`, `[min] … [max]`, `[X]`.
  Never hide them in grey, and never prefill a plausible number.
- **Name buttons by what they do to the model:** "Add as unreviewed draft",
  "Link to Flight Controller", "Ignore, with my note", "Rewrite it in Write".
  Never "Accept", "Apply" or "Accept all".
- **Use plain words in the stakeholder view and stakeholder narratives:** no
  IDs, no interface names, no stereotypes.
- Sentence case for all labels.

## 5. Behaviour

### 5.1 Write: the requirement pipeline

States: **empty → gate fail → gate pass → coach pending → reviewed (current) → committed.**

| Clock | Trigger | Runs |
|---|---|---|
| Gate | text input, 300 ms debounce | `check_requirement()`: score, ledger, hard rules, term extraction and resolution |
| Coach | draft text, `coach_wait_ms` debounce (default 2000) | Only if the gate passes and the text isn't cached: `req_light_check`, routed to the local model |

The rules:
- The coach observer depends on the draft text only.
- A `last_processed` guard keyed on (project, text) stops repeated lookups.
- Commit is enabled when the gate passes **and either** no coach is configured
  (commit then marks the requirement `uncoached`) **or** `assessed_text == current_text`.
- Commit re-runs the gate in core. The database refuses placeholders.
- After the first commit in a session, `scope_gaps` runs once in the background.
- **Deep check (cloud)** runs `req_deep_check` on demand, with up to 40 related
  requirements selected locally plus the scope.
- **Snapshot:** each coach task stores (project id, text, model hash). If the text
  has changed when the result arrives, the result is cached but not shown as current.

**Parsed sentence** underlines each part with a distinct line style, so colour
isn't needed to tell them apart:
- condition: 2px dotted;
- actor: 3px solid, weight 500;
- "shall": bold;
- response: 1px solid;
- constraint: 2px dashed.

A legend sits under the sentence.

**Gate checklist:** mono, one row per ledger item (`+40 exactly one "shall"`),
with the score and PASS / FAIL on the right. Hard-rule failures are listed first,
in amber.

**Terms panel:** one row per extracted term.
- *In the model*: shows the element name, stereotype and "in the model".
- *Not in the model*: amber text with a question and the actions "Add as
  property…", "Add interface field…" or "Add component…" (whichever fit the
  term's role), and "Not a model term". The last one adds a glossary entry with
  no element.
- Anything added this way is created with its value as a placeholder.

**EARS builder:** pick a pattern (ubiquitous, event-driven, state-driven,
unwanted behaviour, optional feature). Fields assemble the sentence, the actor
is a dropdown of model elements, and the result goes into the same text area,
so the same pipeline runs.

### 5.2 Coach panel (any AI panel that reviews engineer text)

The panel is fixed in this order:

1. Header: the AI badge, the title, and the trigger and source ("local · after
   the gate passed").
2. Criteria grid: singular, verifiable, design-independent, conflicts (N related
   checked), fits the scope. Values are `pass`, `question` or `none`. `type_enum()`
   enforces them.
3. **Questions**, one card each, at body size 14–15px. This is the main content.
4. A folded `<details>`: "Show an example rewrite (word diff)". Deleted words are
   struck through in amber; inserted words are underlined in AI blue. Below it:
   "An example only."
5. Actions: "Rewrite it in Write" or "Deep check (cloud)", plus "Dismiss with
   reason". **There is never a button that applies the rewrite.**
6. Footer: provider, prompt version, duration, whether the result came from the
   cache, and "See the prompt".

The example rewrite is re-gated. If it fails the gate, it is hidden and replaced
with "The example didn't pass the gate: <reasons>". It is never repaired
automatically. Numbers in an example that aren't in the draft are replaced with `[value]`, and
that placeholder doesn't count as a gate failure for the example, since the wireframes show it.

A conflict appears only if the model's quoted text matches a committed
requirement verbatim. A paraphrased conflict is shown in the panel but not recorded.

### 5.3 Tiered coaching: proposals and review

All AI output that could change the model becomes an `ai_proposal`.

| Proposal kind | Rendered as | Engineer actions | Effect |
|---|---|---|---|
| `question` | A question with optional context | Act on it elsewhere; "Dismiss with reason" | None until the engineer acts |
| `gap` (source `structural`) | A plain statement with "STRUCTURAL", from code | "Show on diagram", "Link existing", "Ask AI to draft…" | None |
| `candidate_link` | A question naming `Cn` candidates in plain words, plus a rationale field | One button per candidate: "Link to <name>"; enabled once the rationale isn't blank | Creates a `link` with `origin = 'ai_candidate'` |
| `sketch_element` / `sketch_function` / `sketch_step` | A labelled sketch ("Sketch · structure only, no values"): name, stereotype, ports | A required "why" note, then "Add as unreviewed draft" | Creates rows with `origin = 'ai'` and `review_state = 'unreviewed'`; values are placeholders |
| `sketch_requirement` | A starting draft, gate-checked | "Start from this in Write" | Opens Write with the text as a draft |
| `example_rewrite` | Folded word diff (§5.2) | None | None |

**Unreviewed items** appear in their normal views with the AI sketch treatment
(§6.4): a 2px dashed AI-blue border and an AI tint. They list on Health under
"unreviewed AI drafts". Each needs a "Review" action that takes a note in the
engineer's words. After that the item looks like any other model content, but
keeps `origin = 'ai'` and its note for provenance.

**Payload validation** runs before a proposal is stored:
- it must match the JSON schema;
- every referenced handle must resolve;
- any number not copied from the model becomes `[value]` and counts as "values
  the model tried to invent";
- the title must end with "?" for `question` and `candidate_link` kinds. If it
  doesn't, the item is regenerated once, then stored with a warning.

**Numbered candidates:** `rank_candidates(target, kind, k = 10)` scores model
elements locally:
- rare shared words, weighted by inverse document frequency over element names,
  descriptions and linked requirement text;
- shared interfaces;
- same parent;
- an existing allocation of related functions.

The prompt lists them as `C1 Flight Controller — holds NavSolution, commands
Propulsion`. The response refers to `C2`. The map from handle to id stays in
`llm_call.candidates_json`.

### 5.4 Model editing: tree, inspector, diagram

- **One selection model.** Selecting in the tree, on the diagram or in a table
  selects the same entity everywhere. The inspector shows the selected entity.
- **Inspector:** name, stereotype, domain and id (mono), then properties as a
  label/value grid (placeholders amber `[value]`; optional properties that aren't
  set read "not set"), then ports, then linked requirements, then AI questions
  about this entity.
- **Diagram tools** (Structure view):
  - Select;
  - Component (click the canvas);
  - Port (click a component edge; pick direction and interface);
  - Connect (drag port to port; a compatibility check runs on drop, and a
    rejected drop shows why in a toast);
  - Group into subsystem (wraps the selection in a new parent);
  - Undo / Redo (the core command stack, so tree edits undo too).
- **Drill down:** double-click a component to open its sub-architecture. A
  breadcrumb shows the path. Ports of the parent appear as boundary ports.
- **Layout:** positions persist per `view_def`. "Auto-layout" runs ELK layered
  with port constraints, keeps manual positions of pinned nodes, and is undoable.
- **Saved views:** a filter query (`filter_json`):
  `{ "kinds": [...], "stereotypes": [...], "property": {"name":"trl","op":"<","value":6}, "allocated_to": "C-003" }`.
  Elements that don't match are hidden. Connectors to hidden elements are drawn
  as stubs to the view's edge.
- **Gap rendering on the diagram:** an element or port with an open structural
  gap gets the amber gap treatment (§6.4) and a small amber note callout, as on
  the Payload Camera in the wireframe.

### 5.5 Flows and Functional views

**Flows view:** the same elements and connectors as Structure, with the line
style set by the interface's `flow_type`:

| Flow type | Line |
|---|---|
| Data | 2px solid, `--ink-2` |
| Energy | 4px solid, `--ink` |
| Matter | 2px dashed (6 4), `--line-strong` |

Connector labels show quantities from connector property values (`28 V · 380 W`,
`50 Hz`). Checkboxes toggle each flow type. A dashed band at the bottom holds the
environment boundary for external actors.

**Functional view:** function boxes show `F#`, the name, and "→ <allocated
component>", or "→ unallocated" in amber. Flow labels are prefixed `D:`, `E:` or
`M:`. A code panel reports the realisation check. AI allocation questions follow
§5.3, with candidates and a required rationale.

### 5.6 Roll-ups and analyses

- `rollup(property, scope)` sums (or takes the max or min, as defined) every
  `set` value over descendants, multiplied by `multiplicity`. Values are per unit. The UI shows
  "3.31 / 3.60 kg".
- Placeholders are reported in the result ("excludes 1 placeholder") rather than
  treated as zero.
- **Analysis registry:** `register_analysis(name, inputs, fun, unit)`. Inputs are
  declared as `(owner, property)` pairs, so staleness and impact know what an
  analysis depends on. Endurance:
  `capacity_mAh / 1000 * pack_V * usable_fraction * 60 / cruise_W` →
  31.0 min on the sample.
- If any input is a placeholder, the status is `missing_input` and the UI shows
  "can't compute: return_reserve is [value]".
- An analysis's limit comes from `analysis_limit` ("31 / 35 min").

### 5.7 Behaviour scenarios

**Sequence:** lifelines are model elements. Each step is a row in a structured
editor: kind, from, to, interface, field, label, guard, timing. `seqview` renders
the SVG:
- lifeline boxes with dashed lifelines;
- message arrows;
- `opt` / `alt` / `loop` frames;
- notes.

Clicking an element selects its row. Toolbar buttons (Lifeline, Message,
Fragment, Note) add rows. Drag-reordering rows reorders steps.

**Step states:**
- `field_name` not in the interface → amber label and amber arrow, plus
  "Add field to interface" in the consistency panel;
- unreviewed → dashed AI-blue arrow and label;
- timing `placeholder` → `[value] s` shown in the label.

**Consistency panel** (code): "N of M messages map to defined interfaces", plus
each problem.

**Activity:** nodes and edges in `mbdiagram` with ELK layered layout. Actions
are rounded boxes (radius 16), decisions are diamonds (clip-path, filled, no
border), and initial and final are filled circles. Guards label edges. In impact
mode, nodes whose `reads_json` touches a changed property get the amber gap
treatment, plus a callout naming the changed input.

**"Sketch an unreviewed draft" from a description:**
- the AI may use only existing elements and interfaces, referenced as handles;
- every created step or node is unreviewed;
- timings and guards are placeholders;
- if the description needs an element that doesn't exist, it becomes a
  `question` proposal, not a new element.

### 5.8 State machines: language, simulator, assertions

**Guard language** (parsed by a small recursive-descent parser in R; never `eval()`):

```
expr    := or
or      := and ("or" and)*
and     := not ("and" not)*
not     := "not" not | cmp
cmp     := sum (("==" | "!=" | "<" | "<=" | ">" | ">=") sum)?
sum     := term (("+" | "-") term)*
term    := atom (("*" | "/") atom)*
atom    := NUMBER | "true" | "false" | IDENT | "(" expr ")"
IDENT   := an sm_variable name, or prop(Element Name.property) for a model property
effect  := assign (";" assign)*
assign  := IDENT ":=" expr
```

An unknown identifier is a gate error at edit time. Types are checked:
numbers with numbers, booleans with booleans.

**Simulator semantics** (deterministic):
- The state has the current state, a clock in seconds, and the variables. It
  starts in `initial_state_id` with the initial variable values, or at a `start` state.
- Firing an event:
  1. find the transitions from the current state on that event;
  2. evaluate the guards against the variables, with the event's `set` values applied first;
  3. take the highest-priority true one (ties are a gate error);
  4. if it has a hold of `hold_s`, advance the clock by `hold_s`, then fire;
  5. apply the effect;
  6. enter the target state and apply its entry effect.
- If no transition matches, log "<event> ignored in <state>" and leave the state
  unchanged. If the pair is in scope and has no `sm_ignore`, the log line is amber.
- If a transition has a placeholder hold, the run stops with `error`: "hold_timeout
  needs a value".
- Every step appends `{t, from, to, event, note}` to the trace.

**UI** (matches the wireframe's working simulator):
- a run bar: clock, current state, live variables;
- the diagram highlights the current state (ink fill, white text);
- Fire-event buttons are dark for events valid in the current state and muted
  for the rest (muted ones are still clickable, and get logged as ignored);
- Reset;
- the trace, newest first;
- requirement checks update live.

**Assertions:** `script_json` is a list of steps:
`{"start":"S-MIS"}`, `{"event":"gnss_lost","set":{"gnss_valid":false},"hold_s":3}`,
`{"advance_s":5}`.
`expect_json` gives `{"state":"S-RTH","within_s":3.0}` and/or `{"vars":{...}}`.
A script's `hold_s` restates the transition's hold for the reader and must equal it
(a mismatch is an `error`); the clock advances once, by the transition's value.
A run records `pass`, `fail` or `error` with the trace and the model hash. The
Requirement checks panel shows "PASS · entered Return to home 3.0 s after GNSS
loss", or the failure with the step that broke it.

**Completeness:** `v_sm_unhandled` drives the "Completeness · code · every edit"
panel. Each pair offers "Add transition" or "Ignore, with my note". AI coach
questions about these pairs come after the code panel and never replace it.

### 5.9 Baselines and change impact

- The **Compare** selector defaults to the latest baseline → Working copy.
- **Left column:** one card per change (added, removed, modified), with the field
  diff in mono and "N elements impacted · M high". Selecting a card drives the main column.
- **Impact table** (code): impacted element, path (mono, using →), hops,
  significance (AI). High is amber and weight 600; Medium is weight 500; Low is
  normal weight. Stale evidence appears as an amber sub-line under the test case
  ("evidence recorded against 5000 mAh, no longer current").
- **Narrative:** Technical and Stakeholder tabs. Technical names requirements and
  tests and gives the numbers from analyses. Stakeholder uses plain words and
  ends with "Decision needed: …" when a budget or requirement fails. Footer:
  "numbers from deterministic roll-up, not the model".
- **"Create baseline BL-04…"** opens a dialog. If blockers exist, the dialog lists
  them, grouped the same way as the Health banner, and the Create button stays
  disabled.
- **Rollback** asks for confirmation, names the safety baseline it will create,
  and is undoable by rolling back to that safety baseline.
- **Behavioural impact:** "N scenarios affected" links to artboard 9. Scenarios
  are ranked by AI significance. Before, After and Side by side render the
  scenario as of the baseline and as of the working copy.

### 5.10 Health

No AI calls on this page, and the page says so in the tab bar.

**Banner:** "BL-04 can't be created yet · N blockers". It appears only when
blockers exist; when there are none it reads "Ready to baseline", in ink. It has
three groups:
1. placeholders (`v_placeholders`);
2. unreviewed AI drafts (`v_unreviewed`);
3. stale evidence (`stale_evidence()`).

Each item links to where it can be fixed. A note under placeholders counts
requirement drafts with placeholders, which don't block a baseline.

**Cards:**
- Requirements: committed, passing the gate, failing, committed without a coach
  review, committed but unallocated, no verification method.
- Architecture: components, unconnected ports, components with no requirements,
  interfaces, fields failing the unit check.
- Functional: functions, unallocated, flows with a physical path.
- Behavior: scenarios, messages using a missing field, unhandled pairs, last
  assertion results.
- Budgets: roll-ups and analyses against limits, with a failing one in amber
  ("31 min · FAIL").
- AI contributions: kept with a note, dismissed with a reason, unreviewed, values
  the model tried to invent (blocked).

Problem rows are amber links. Neutral rows are plain.

### 5.11 Chat ("Ask the model")

- Read-only. Tools:
  - `get_element(handle)`, `get_interface(handle)`, `list_requirements(filter)`;
  - `rollup(property, scope)`, `analysis(name)`;
  - `impact_set(change)`, `find(text)`.
- Tools return handles plus plain data. They never return ids the model could
  then use to write.
- Each answer shows the tool calls it made as small AI-bordered mono chips above
  the text.
- Actions under an answer:
  - "Highlight on diagram" selects the referenced entities;
  - "Attach to proposal" appends the answer to an open proposal's body.
- The footer shows the routed model. A note says the chat can't edit the model.

### 5.12 Project: providers, routing, policy, data protection

- **Providers:** Anthropic and OpenAI, with the key fields writing to keyring and
  a "Connected" chip after a test call. LM Studio (and any OpenAI-compatible
  server) takes a base URL; "Reachable · N models loaded" comes from listing the
  server's models.
- **Task routing table:** task, Runs (every edit, on demand or when asked;
  editable only for the requirement checks), provider, model, fallback, and what
  the task sends (a plain description).
- **Coaching policy:** the table from BUILD_PLAN §2, plus a policy selector. In
  v1, Tiered is the only option enabled.
- **Data protection:**
  - **Local only:** routes every task to the local provider. Cloud calls are
    blocked and logged with status `blocked` and error kind `local_only`.
  - **Redaction:** before a cloud call, each listed term is replaced with
    `[REDACTED-n]`, and the response is mapped back.
  - **Audit logging:** every prompt and response goes to `llm_call`. This is on by
    default and can't be turned off in v1.
  - **Spend cap:** once today's cost reaches the cap, cloud calls are blocked
    with error kind `spend_cap`.

### 5.13 Async and errors

- Every AI call is an `ExtendedTask` wrapped by `safely_async()`, so a
  synchronous throw becomes a rejected promise.
- While a call runs, its panel shows a quiet "Checking…" line, not a spinner over
  content. The rest of the UI stays usable.
- Errors render inside the panel that asked, in plain words, with the next step.
  For example: "LM Studio isn't running at localhost:1234. Start it, or switch
  this task to a cloud provider in Project." Kinds are listed in `llm_call.error_kind`.
- **Toasts** are only for edits refused by a gate ("Can't connect: MotorCmd out
  to PowerBus28V in"). They stay 6 s and can be dismissed.

## 6. Styling

### 6.1 Direction

Utilitarian engineering software: calm, dense, typographic. The structure comes
from type, rules and spacing, not colour. Colour carries meaning, and there are
exactly two meaningful hues:

- **AI blue:** anything the AI produced or does.
- **Amber:** gaps, problems, placeholders and stale or failing items.

Everything else is ink and greys. No gradients, no emoji, no coloured left-border
cards, no decorative icons.

### 6.2 Tokens

```css
:root {
  /* ground and surfaces */
  --ground:        #F4F4F2;  /* app background */
  --surface:       #FFFFFF;  /* panels, cards, tables */
  --surface-muted: #ECEDEF;  /* table headers, selected rows, bar tracks */
  --canvas:        #FAFAF9;  /* diagram background, with a 16px dot grid in --line */

  /* ink */
  --ink:      #1C1D1F;  /* text, top bar, primary buttons, selected state */
  --ink-2:    #45484D;  /* secondary text, data-flow lines */
  --ink-3:    #5B5E63;  /* captions, labels, muted values */
  --ink-on-dark:       #C9CCD1;  /* inactive nav on the top bar */
  --ink-on-dark-muted: #B9BCC2;  /* project context on the top bar */

  /* lines */
  --line:         #D4D5D8;  /* card borders, table rules (decorative) */
  --line-strong:  #9A9DA3;  /* diagram block borders, matter flows */
  --line-control: #85888E;  /* input, select, secondary-button borders: 3.55:1 on white */
  --edge:         #6B6E74;  /* default diagram connector */

  /* chrome */
  --topbar-bg:     #1C1D1F;
  --topbar-active: #3A3C40;
  --status-bg:     #2A2C30;

  /* AI */
  --ai:        #2F5BD3;  /* badge, primary AI buttons, links */
  --ai-text:   #1E3F9E;  /* AI text on the tint */
  --ai-tint:   #EAF0FD;  /* AI panels, sketch fill */
  --ai-border: #B8C9F2;

  /* gap / problem / placeholder */
  --gap-text:   #8A4500;
  --gap-tint:   #FFF3E3;
  --gap-border: #E8B77A;
  --gap-stroke: #B35A00;  /* dashed gap outlines, failing meters */
  --gap-strong: #6E3600;  /* text on --gap-fill */
  --gap-fill:   #FFDDB0;  /* changed decision nodes */

  /* shape and space */
  --radius: 4px; --radius-inner: 3px; --radius-chip: 10px;
  --radius-state: 20px; --radius-action: 16px;
  --space-1: 4px; --space-2: 8px; --space-3: 12px; --space-4: 16px; --space-5: 24px;
}
```

Measured contrast (WCAG 2.x), all text pairs ≥ 4.5:1:

| Pair | Ratio |
|---|---|
| ink on ground | 15.3 |
| ink-3 on white | 6.5 |
| ink-3 on surface-muted | 5.6 |
| ink-on-dark on topbar | 10.5 |
| white on AI | 5.9 |
| AI on AI tint | 5.2 |
| ai-text on AI tint | 8.1 |
| gap-text on gap tint | 6.5 |
| gap-strong on gap-fill | 7.4 |
| ink-2 on canvas | 8.8 |

Non-text: `--line-control` on white is 3.55:1 and `--gap-stroke` on white is
4.8:1. `--line` (1.5:1) is decorative only and never the sole boundary of a control.

### 6.3 Type

- **IBM Plex Sans** (400, 500, 600) for the UI. **IBM Plex Mono** (400, 500) for
  ids, interface and field names, numbers with units in data contexts, the gate
  ledger, paths, the status strip, and section labels.
- **Bundle the font files** with `bslib::font_face()` pointing at `www/fonts`; don't
  load them from Google at runtime. Fall back to `system-ui` and `ui-monospace`.

| Role | Size / weight |
|---|---|
| Page title (h1) | 20–22 / 600 |
| Section (h2) | 15–18 / 600 |
| Body | 14 / 400, line-height 1.45–1.55 |
| Dense UI, tables | 13 |
| Caption, legend | 12 |
| Section label | Mono 11, uppercase, letter-spacing 0.06em, `--ink-3` |
| Coach question | 15 (14 for secondary questions) |
| Parsed sentence | 16, line-height 2.1 |

### 6.4 Components

| Component | Spec |
|---|---|
| AI badge | Mono 11 / 500, "AI", white on `--ai`, padding 2px 6px, radius 3. Precedes every AI panel title. |
| AI panel | `--ai-tint` background, 1px `--ai-border`, radius 4, padding 12–14. Inner cards are white with an `--ai-border` border. |
| Code panel | White, 1px `--line`, with a title plus a caption "code · every edit" or "no AI". |
| Question card | White inside an AI panel, 15px text, then a 12px context line, then actions. |
| Folded diff | `<details>` with a 500-weight summary. `del` is `--gap-text` with strikethrough; `ins` is `--ai-text` with an underline. |
| Placeholder | Inline text `[value]` in `--gap-text`. In inputs, it shows as the placeholder state, never as a value. |
| Gap treatment | Element: 2px dashed `--gap-stroke`. Callout: `--gap-tint` with a 1px `--gap-border`, 12px text, a bold lead ("Gap:"). Rows: `--gap-tint` background. |
| AI sketch treatment (unreviewed) | 2px dashed `--ai` border, `--ai-tint` fill. Lines are dashed (6 6) in `--ai`. Labels in `--ai-text`. |
| Selected | 2px solid `--ink` (outline offset -2 on rows). Tree item: `--surface-muted` background. |
| Status chips (stakeholder) | Designed: ink fill, white text. In progress: `--surface-muted`. Decision needed and Open question: `--gap-tint` with `--gap-border` and `--gap-text`. Not started: white with a 1px dashed `--line-strong`. Radius 10, 12px. |
| Buttons | Primary model action: ink fill, white text. Primary AI action: `--ai` fill, white text. Secondary: white, 1px `--line-control`, ink text. AI secondary: `--ai-tint` fill, `--ai` border, `--ai-text`. Padding 6px 12px, 13px, radius 4. Touch targets at least 32px tall in dense toolbars, 44px elsewhere. |
| Segmented control | Joined buttons in a 1px `--line-control` frame; selected is ink fill. |
| Tables | reactable: header `--surface-muted`, 13px, weight 500; 1px `--line` row rules; IDs in mono; no zebra striping. |
| Meters | 10px track `--surface-muted`, radius 5; fill `--ink-2`, or `--gap-stroke` when failing. Label and value row above; a 12px status line below. |
| Diagram block | White, 1px `--line-strong`, radius 4; stereotype `«HW»` mono 11 `--ink-3` above a bold name. Port: 10px square, white with a 2px ink border (gap: `--gap-tint` fill with a `--gap-stroke` border). |
| Diagram connector | 2px `--edge` with a filled arrowhead. Label: mono 12 `--ink-2` on a `--canvas` background pad. |
| State node | Radius 20, 1px `--ink-2`; current state is ink fill with white text and weight 600. |
| Activity | Action radius 16; decision is a diamond filled `#DADBDE` (changed decision: `--gap-fill` with `--gap-strong` text); initial and final are filled ink circles. |

### 6.5 bslib theme

```r
theme <- bslib::bs_theme(
  version = 5,
  bg = "#F4F4F2", fg = "#1C1D1F",
  primary = "#1C1D1F", secondary = "#5B5E63",
  info = "#2F5BD3", warning = "#B35A00",
  base_font = bslib::font_face("IBM Plex Sans", src = "fonts/IBMPlexSans-*.woff2"),  # one call per weight
  code_font = bslib::font_face("IBM Plex Mono", src = "fonts/IBMPlexMono-*.woff2"),
  "border-radius" = "4px", "border-color" = "#D4D5D8",
  "font-size-base" = "0.875rem",
  "link-color" = "#2F5BD3"
)
# Add the §6.2 tokens as a CSS file in www/; component rules use var(--…).
```

Fingerprint the compiled theme URL (a Coach lesson), and force
`display: block` on Shiny outputs.

## 7. Accessibility

- Use real `<button>`, `<a href>` and labelled `<input>` elements. Every
  icon-only button has an `aria-label`.
- Diagrams expose a parallel accessible list: the tree plus a "connections" table.
- Keyboard: Tab order follows the visual order. In diagrams, the arrow keys move
  the selection between connected nodes and Enter opens the inspector.
- Colour is never the only signal:
  - AI items carry the "AI" badge or dashed outlines;
  - gaps carry text ("Gap:", "missing", "unallocated");
  - flow types differ by line width and dash.
- Visible focus: a 2px `--ai` outline with a 2px offset.

## 8. Browser acceptance checklist (shinytest2 plus a manual pass)

1. App opens on Write, and the status strip shows "3 unreviewed AI drafts · 4 placeholders" on the sample.
2. Typing a draft updates the gate within about 300 ms. The coach result appears
   about 2 s after typing stops, and only when the gate passes.
3. Editing the text after a coach review disables Commit until the next review is current.
4. Typing `[value]` keeps Commit disabled with the placeholder named. The database
   rejects a forced commit.
5. The example rewrite is folded by default, and no button applies it.
6. In the Review queue, "Add as unreviewed draft" stays disabled until the note
   has text. The new component appears dashed blue on the diagram and on Health.
7. On Traceability, the Link buttons stay disabled until a rationale is entered.
8. On the diagram, connecting MotorCmd out to a PowerBus28V in port is refused
   with a toast. A valid connection persists after reload.
9. Mass shows 3.31 / 3.60 kg on Structure, Stakeholder and Health. Endurance
   shows 31 / 35 min and FAIL.
10. The simulator, starting in Survey mission, fires `gnss_lost`, reaches Return to home
    at T+3 s, and SYS-012 shows PASS.
11. Completeness lists exactly three pairs. "Ignore, with my note" with a blank
    note is refused.
12. BL-03 → Working copy shows 3 changes. TC-007 shows stale evidence. "Create
    baseline" lists 8 blockers and stays disabled.
13. Impact narrative: the Technical and Stakeholder tabs switch. The Stakeholder
    text contains no IDs.
14. With Local only on, a cloud task is blocked and the panel says why.
15. With LM Studio stopped, the coach panel shows the plain-word error. The rest
    of the app keeps working.
16. At 900px wide, the side columns stack, and tables scroll inside their boxes
    with no horizontal scroll on the page.
