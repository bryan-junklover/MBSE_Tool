# ModelBench user guide

ModelBench keeps a system's requirements and its model in one project file. You write the
requirements, then build the structure, interfaces and behaviour that satisfy them, and check
how the system fails. Diagrams, tables and documents are all views of that one model: change
something in one place and every view shows it.

An AI coach can ask questions and sketch drafts along the way. It never decides anything:
every value, link, response and commit is yours, and the code, not the prompt, enforces that.

For a step-by-step walk through a new project, see the **How-to** tab.

## Contents

1. Starting the app
2. Finding your way round
3. Project: settings, scope and glossary
4. Write: requirements
5. Model: structure, flows, functions and interfaces
6. Behavior: scenarios, state machines and failure modes
7. Analyze: health, review, baselines and documents
8. Asking the model
9. AI rules and data protection
10. Keyboard and accessibility
11. Troubleshooting

## 1. Starting the app

Open `MBSE_Tool.Rproj` in RStudio, open `app/app.R` and click **Run App**.

- **Where projects live:** each project is one `.sqlite` file in your workspace folder. That's
  R's user data folder for `modelbench`, unless `MODELBENCH_WORKSPACE_DIR` points somewhere else.
- **The sample:** if the workspace is empty, the app creates the Survey UAV sample, a small
  quadcopter model with requirements, structure, behaviour and failure modes. It's a good place
  to look around before starting your own.
- **Upgrades:** an older project is upgraded the first time it opens. Its data is kept, and every
  link is checked before the upgrade is saved.
- **Exports:** the interface control document and the stakeholder documents need Quarto.
  RStudio includes it; otherwise install it or set `QUARTO_PATH`.

## 2. Finding your way round

The top bar has the project name, the five workspaces (**Project**, **Write**, **Model**,
**Behavior**, **Analyze**), **Help**, and **Ask the model**. Under it, a status strip shows the
coach, the AI policy, how many unreviewed AI drafts and placeholders the project holds, and
today's AI calls and spend. Click the drafts count to open them.

Each screen has one bar at the top: the workspace's tabs on the left, that screen's actions on
the right. The app remembers which screen you were on in each project.

**Columns.** Drag a side column's edge to widen it, or click the small arrow on the edge to hide
it. A hidden column leaves a narrow rail with its name; click the rail to bring it back. You can
also focus the edge and use ← and → (⇧ for bigger steps), or Home and End. Widths are remembered.
In a narrow window, columns open over the page from their rails instead, and Esc closes them.

**Diagrams** open fitted to the canvas.

- The buttons at the bottom right fit the diagram, zoom out, show it at 100% and zoom in.
- On the canvas, `+`, `-`, `0` (fit) and `1` (100%) do the same.
- Ctrl or ⌘ with the scroll wheel zooms where the pointer is, and dragging empty canvas moves the
  view.
- ⌘Z and ⇧⌘Z (Ctrl on Windows and Linux) undo and redo while the canvas has focus.
- **Auto-layout** rearranges a diagram. Blocks marked **Keep this position on auto-layout** stay
  put.
- Connector labels hide when the diagram is too small to read them.

**Undo.** The **Undo** and **Redo** buttons are on the Model and Behavior diagram screens. They
undo model edits made anywhere in the session, most recent first: structure, interfaces, links,
behaviour, failure modes, and the properties and budgets in Project. The stack starts afresh when
you switch project or roll back.

Some changes can't be undone yet:
- committing or saving a requirement, and edits in the Specification inspector;
- test cases and recorded results;
- imports;
- dismissing an AI item;
- the glossary, units, ID prefixes and scope.

Anything that deletes asks first, and says what goes with it.

**Jumping and coming back.** Links on Health, the Review queue and elsewhere take you to the
thing they're about. A bar then offers **Back to …** the screen you came from.

## 3. Project: settings, scope and glossary

**General**

- **Open project** switches projects. **New project name** and **Create project** start an
  empty one.
- **Scope:** a few sentences on what the system is. It's saved as you type. The AI is given it
  for context, and documents quote it.
- **ID prefixes:** the prefixes for new requirements and components, such as `SYS` and `C`.
  Existing ids never change.

**Units & properties**

- **Properties** are what a component can hold, such as Mass in kg or Supplier as text. A
  number property can roll up: sum, largest or smallest, up the tree and times how many of each
  part.
- **Budgets** limit a rolled-up property over part of the system, at most or at least, and can
  name the requirement they come from. They're checked on every edit; Health and the Structure
  inspector show any that fail.
- **Units:** every project has the standard units. Add your own with its symbol, what it
  measures, its SI unit, and its factor (plus an offset, as for °C). A quantity already listed
  keeps its SI unit, so values convert between units.

**Glossary.** Terms with definitions. A term tied to an element is an alias of it, so the gate
recognises "battery" as Power System. A term with no element isn't a model term.

**AI**

- **AI providers:** Anthropic and OpenAI in the cloud, or LM Studio on your machine. A cloud key
  goes to your operating system's keyring, never into the project file. **Test the connection**
  says in plain words whether it works.
- **Task routing:** for each AI task, choose its provider, model and fallback, and whether it
  runs on every edit or on demand.
- **Daily cloud spend cap** and **Coach wait after typing stops**.
- **Coaching policy:** a table of what the AI may do, what's always yours, and how the code
  enforces each rule.

**Data protection.** **Local only** keeps every call on your machine. **Redact before cloud
calls** takes one term per line; each is replaced before anything is sent.

## 4. Write: requirements

### Draft

Write one requirement at a time.

- **Free text** or the **EARS builder** (pattern, trigger, actor, response, bound, value and unit,
  then **Put the sentence in the draft**).
- **The gate** scores the draft as you type. It's code with no AI involved. A good requirement:
  - has exactly one "shall", with a named system or component before it;
  - is a complete sentence;
  - states a measurable bound (a number with a unit, or a phrase such as "only in");
  - states one concept, in the active voice;
  - avoids vague words such as "fast" or "user-friendly".
- **Terms in this draft** shows the words the gate matched against the model. An unknown term
  can be added to the model as a component, a property of a component, or a field of an
  interface. Its value starts as a placeholder.
- **The coach** reviews a draft once it passes the gate, and asks questions. Commit is enabled
  only when the coach's review matches the text on screen. A question you disagree with can be
  dismissed, with a reason.
- **Parent**, **Level** (stakeholder, system, subsystem), **Type**, **Verify by** and
  **Short title** go with the draft.
- `[value]` marks a number still to be decided. A draft holding one can be saved
  (**Save draft**) but not committed.
- **Commit to specification** adds it. **New requirement** clears the page.

### Specification

The outline and table of every requirement, sortable by gate score (**Lowest score first**).

- Select a requirement to edit its details in the inspector.
- **Rewrite it in Write** reopens committed text in Draft, and the gate checks it again.
- **Verification:** add a test case (name and method), then **Record result** with pass, fail or
  error, an evidence reference and a note. Evidence recorded against an older version of the
  model is marked stale.
- **Import** from CSV, Excel or text. Each row is gate-checked, and you either add it as a draft
  or skip it.
- **Export** as ReqIF, CSV, Markdown or PDF.

### Traceability

The requirements × components matrix, plus functions × components.

- Click a cell to link a requirement to the component that satisfies it, or to unlink. A
  **Rationale** is optional for your own links.
- **Ask AI about this** ranks candidate components and argues for them as questions. Choosing one
  needs your rationale.
- Rows with nothing allocated are marked as gaps.

## 5. Model: structure, flows, functions and interfaces

### Structure

The block diagram of components, their ports and connectors. The model tree is on the left and
the inspector on the right.

- **Tools:** **Select**, **Component** (click the canvas to add one), **Port** (click a block) and
  **Connect** (drag from port to port). In Select you can also drag from port to port.
- **The first component** in a project is the whole system. The diagram opens inside it, and
  later components are its parts.
- **Opening a block:** double-click a component to open the level inside it. The breadcrumb
  takes you back up.
- **Group into subsystem** wraps the selected components in a new parent.
- **Saved view** picks a filtered view: All components, Power & mass budget, Software only, or
  TRL below 6.
- **The inspector**, for a selected component:
  - name, stereotype, domain (hardware, software, hardware and software, other), status, how
    many, and description;
  - properties, with units. **Add a property** offers the project's property definitions.
  - ports, and the requirements linked to it.
  - **Runs on** (software components only): the hardware it runs on. Hardware lists **Software
    running here**.
  - **Keep this position on auto-layout**, and delete.
- **With nothing selected**, the inspector shows the level's roll-ups (mass, power and budgets
  against their limits) and its open gaps.
- **Gaps** are dashed amber with a note: a port with no connector, or software with nothing
  recorded to run on.
- **Connector checks:** a connector must join ports with the same interface and compatible
  directions. A refused connection says why.

### Flows

The same structure, coloured by what moves along each connector: data, energy or matter.
**Show** picks the flow types, and **Show flow quantities** adds rates.

Flows also shows **outside equipment**: things beyond the system boundary that it exchanges
with, such as an operator or a base station. The **Outside equipment** tool adds one, and
**Port** and **Connect** work as on Structure.

### Functional

The functions and the flows between them, each function allocated to the component that
performs it.

- **Function** adds one, and ports and connections work as on Structure.
- **Allocate to** a component in the inspector, or **Ask AI about this**.
- **Carried by** names the physical connector that carries a functional flow. The
  **Consistency** panel lists flows with no physical path.

### Interfaces

The interface dictionary.

- Each interface has a flow type, a rate, a transport, and fields.
- Each field has a type, a unit, and a range, which can be "to be decided" or "not applicable".
- **Where it's used** lists the ports, scenarios, state machine events and requirements that
  refer to the interface.
- **ICD (HTML)** and **ICD (PDF)** export the interface control document.

### Stakeholder

A plain-words view for people outside engineering. It has the main parts in a sentence each,
**Are we on track?** meters for budgets and analyses, and open questions.
**Write the summary with AI** drafts the sentences; **Export as PDF brief** makes a handout.

## 6. Behavior: scenarios, state machines and failure modes

### Scenarios

Sequence or activity diagrams, edited as a table of steps.

- **New scenario…** picks a name and the kind of diagram.
- In a sequence diagram:
  - **+ Lifeline** adds a participant.
  - **+ Message** adds a step, with its interface, field and guard in the step inspector.
  - **+ Fragment** and **+ Note** add the rest.
  - Drag rows to reorder them, or use a row's ⋯ menu.
  - Drag the bar under the diagram to share the height between diagram and table.
- **Sketch with AI…** drafts a scenario from your description. Its steps arrive as unreviewed
  drafts.
- **Highlighting:** amber means a step names a field its interface doesn't have. Dashed blue is
  an AI draft that needs your review.

### State machines

**New state machine…** creates one for a component, starting in one state. **State machine**
chooses which one to edit. States, events and variables each have an **Add…** button, and you
drag between a state's handles to add a transition. The inspector edits the selected state or
transition.

- **Guards and effects** use a small language: `gnss_valid == false`, `soc := soc - 5`,
  `prop(Power System.battery_capacity) > 4000`. A transition can wait for its condition to hold
  for a time.
- **Completeness** lists each event/state pair that needs a transition, or an **Ignore, with my
  note**. It counts only the pairs you've put in scope: select a state, and tick the events it
  **Must handle**. A new event isn't in scope anywhere until you tick it.
- **Checks** holds assertions. **Add assertion…** builds one from a start state, a list of events
  and the state it should end in, optionally tied to a requirement. **Run and record** runs them
  all, and a requirement's checks run live.
- **The simulator** runs under the canvas. Choose where to start, fire events (those that can fire
  from here are listed first), and open the trace. **Reset** starts again.
- **Ask about this state machine** asks the AI about gaps in it.

### Failure modes

What can fail, why, what happens next, and how the system responds.

- **Left:** the failure modes. Those with an open response are listed first.
- **Centre:** the editor. You set:
  - **What fails:** a component, an interface (optionally one field), a port, or a state.
  - **Cause:** lost, stale, out of range, wrong value, degraded, failed, open circuit, short
    circuit, over-temperature, mechanical or other, plus a cause in your own words.
  - **Effects:** the effect nearby and the effect on the system.
  - **Response:** not decided, handled, accepted or unhandled, with a note. "Accepted" needs a
    reason.
  - **Detected by:** a state machine event, a requirement or a test case.
  - **Handled by:** a transition, a requirement or a component.
- **Effect chain**, under the editor, traces what the failure leads to through connectors, parts
  and "runs on" links:
  - **→** something stops working;
  - **✓** a loss is handled or accepted where it lands;
  - **⚠** a loss nobody has decided about.

  Losing power stops the receiver, and the chain goes on. Losing data with nothing recorded is
  flagged, and the chain stops there.
- **Right: the checks.**
  - **Open responses:** a response that isn't decided, a failure marked handled with nothing
    handling it, or one handled by a requirement nothing verifies.
  - **Cases nobody has recorded:** software receiving a signal from hardware with no "lost" or
    "stale" case, a power feed with no "lost" case, a part that draws power with no
    over-temperature case, and a field with a range but no out-of-range case.
  - **Add … failure** starts a failure mode from a check. **Doesn't apply…** sets a check aside
    with your reason, and **Check again** brings it back.
- **+ Failure mode for…** adds one for anything in the model.
- **Suggest with AI** asks the AI for failures nobody has recorded. Each suggestion arrives as a
  card:
  1. **Add as unreviewed draft**, with your note, makes it an AI failure mode.
  2. **Review, with my note** makes it part of the model.
  3. Only then can you set its response.

### Behavioral impact

For a change since the last baseline, the scenarios it reaches, with the affected steps
highlighted before and after.

## 7. Analyze: health, review, baselines and documents

### Health

Everything that blocks the next baseline, with a link to fix each one. Below that are cards for:
- Requirements: gate failures, missing coach reviews, nothing allocated, no verification method.
- Architecture: unconnected ports, components with no requirements, software with nothing to run
  on, gate findings.
- Functional: unallocated functions, and flows with no physical path.
- Behavior: missing fields and unhandled event/state pairs.
- Failures: failure modes, open responses, and cases not covered.
- Budgets and AI contributions.

**Recompute** runs the analyses again.

### Review queue

The AI's questions and sketches, plus structural gaps the code finds.

- **Ask AI about gaps** asks for more.
- **Keep:** a sketch joins the model only when you add it with a note in your own words, and it
  then stays an unreviewed draft until you review it.
- **Dismiss with reason** closes an item.
- **Show on diagram** goes to what an item is about. **See the prompt** shows exactly what was
  sent.

### Baselines & impact

- **Compare** a baseline with the working copy, or with another baseline. Changes are grouped by
  where they start.
- Select a change to see what it reaches. **Rate and explain with AI** adds a narrative.
- **Roll back…** puts the model back as it was at a baseline. It saves a safety baseline first,
  so a rollback can itself be undone. Requirements committed since the baseline are removed,
  even ones that were drafts then; the safety baseline keeps them.
- **Create baseline** is enabled once nothing blocks it. Its button names the blockers until then.

### Documents

Read-only documents for each group of readers, generated from the model.

| Document | For | Covers |
|---|---|---|
| **Hardware** | Hardware engineers | Physical breakdown (with the software each part runs), interfaces, budgets and properties, physical failures and their effect chains, requirements |
| **Software** | Software engineers | Where each piece of software runs, signals with their ranges, state and transition tables, software failures and their effect chains, requirements |
| **Systems** | Systems engineering and reporting | Everything |

Each ends with the open items in its part of the model. **From** builds a document from a
baseline instead of the working copy; the working copy isn't touched. **Export HTML** and
**Export PDF** save it.

## 8. Asking the model

**Ask the model** opens a chat that reads the model through tools, which show as small chips
above each answer. It can't change anything.

- **Highlight on diagram** selects the parts an answer is about.
- **Attach to proposal** adds the answer to an open question in the Review queue.
- A number the tools didn't give shows as `[value]`, so the chat can't slip in figures of its own.

## 9. AI rules and data protection

- **The AI asks and sketches.** Values, links, responses and commits come from you.
- **AI drafts are marked.** They're unreviewed until you review them with a note, and they block
  a baseline until then.
- **The AI never sees ids.** Model content is sent as numbered names such as C5, and the app maps
  them back, so the AI can't invent a link to something that doesn't exist.
- **The AI can't invent numbers.** Any number it writes becomes `[value]`.
- **Every call is logged** with its prompt, model and cost. **See the prompt** shows what was sent.
- **When something's down,** such as a local server or a missing cloud key, the panel says so in
  plain words, and the rest of the app keeps working.
- **Spending:** cloud calls stop at the daily spend cap. **Local only** keeps every call on your
  machine.

## 10. Keyboard and accessibility

- **Keyboard:** everything works from the keyboard, and focus always shows as a blue outline.
- **Diagrams** have the model tree and tables beside them. The arrow keys move between connected
  blocks.
- **Colour is never the only signal:** gaps say "Gap", and AI items carry the AI badge or a dashed
  outline.

## 11. Troubleshooting

- **A diagram has no zoom buttons, or straight connectors.** Your R session holds an old copy of
  the diagram widget. The app reloads it at start-up; if that isn't possible, it asks you to
  restart R.
- **Export says it needs Quarto.** Install Quarto, or set `QUARTO_PATH` to it.
- **An edit is refused.** The message says why: for example, a connector between incompatible
  ports, or "accepted" without a reason. Nothing is changed.
- **The coach or AI says it can't run.** Check **Project › AI**. The provider may need a key, the
  local server may be off, or the daily cap may be reached.
