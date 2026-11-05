# ModelBench user guide

ModelBench keeps a system's requirements and its model in one project file. You write
the requirements first, then build the structure, interfaces and behaviour that satisfy
them. An AI coach asks questions along the way, but it never decides anything: every
value, link and commit is yours.

## Starting

Open `MBSE_Tool.Rproj` in RStudio, then open `app/app.R` and click **Run App**. The app
opens your workspace folder. If it's empty, it creates the Survey UAV sample, which is a
good place to look around.

Projects live in **Project**. That's also where you choose AI providers, route each
task to a model, set a daily spend cap, and turn on **Local only**, which keeps every
call on your own machine.

## Finding your way round a screen

Each screen has one bar at the top: the workspace's tabs on the left, the screen's own
actions on the right. Side columns can be widened by dragging their edge, and hidden with
the small arrow on that edge; a hidden column leaves a narrow rail with its name, which
brings it back. Widths and hidden columns are remembered. On a narrow window the columns
open over the page from their rails instead, and Esc closes them.

Diagrams open fitted to the canvas. The buttons at the bottom right fit the diagram, zoom
out, show it at 100% and zoom in; on the canvas, `+`, `-`, `0` (fit) and `1` (100%) do the
same, Ctrl or ⌘ with the scroll wheel zooms where the pointer is, and dragging empty
canvas moves the view. ⌘Z and ⇧⌘Z (Ctrl on other systems) undo and redo while the canvas
has focus. Edge labels hide when the diagram is too small to read them.

Anything that deletes asks first, and says what goes with it.

## The five workspaces

**Write** is for requirements.

- **Draft:** type a requirement. The gate scores it as you type, with no AI involved.
  Once it passes, the coach reviews it and asks questions. Commit is enabled only when
  the review matches the text on screen. A requirement with `[value]` in it can't be
  committed: that's a number still to be decided.
- **Specification:** the outline and table of every requirement. You can import from
  CSV, Excel or text. Export as ReqIF, CSV, Markdown or PDF.
- **Traceability:** the requirements × components matrix. Click a cell to link or
  unlink. AI suggestions need your rationale before any Link button works.

**Model** is for structure.

- **Structure:** the block diagram. Drag between ports to connect them, double-click a
  block to open it, and use the arrow keys to move between connected blocks. With nothing
  selected, the inspector shows the level's roll-ups (mass, power, endurance) and open gaps.
- **Flows** and **Functional:** the same model seen by flow type, and as functions
  allocated to components.
- **Interfaces:** each interface's fields and where it's used. Export the interface
  control document (ICD) as HTML or PDF from here. The ICD needs Quarto, which RStudio
  includes.
- **Stakeholder:** a plain-words view for people outside engineering, with "Are we on
  track?" meters and a PDF brief.

**Behavior** is for what the system does.

- **Scenarios:** sequence diagrams edited as a table of steps (drag rows to reorder, or
  use a row's ⋯ menu), with the interface, field and guard in the step inspector; drag
  the bar under the diagram to share the height. **Sketch with AI…** drafts a new one.
  Activity diagrams too. Amber means a step names a field its interface doesn't have.
  Dashed blue means an AI draft that needs your review.
- **State machines:** edit states and transitions, then run the simulator under the
  canvas. Choose where to start, fire events (those that can fire from here are listed
  first), and open the trace. Completeness and Checks are tabs beside the inspector, and
  **Add assertion…** builds a check from a start state, events and an expected state. Guards and effects use a small
  language: `gnss_valid == false`, `soc := soc - 5`, `prop(Power System.battery_capacity) > 4000`.
  The Completeness panel lists event/state pairs that need a transition, or an
  "ignore" with your note. Requirement checks run every assertion live.
- **Behavioral impact:** for a change since the last baseline, the scenarios it
  reaches, with the affected steps highlighted, before and after.

**Analyze** is for checking.

- **Health:** everything that blocks the next baseline, with links to fix each one.
- **Review queue:** the AI's questions and sketches. Nothing joins the model until you
  add it with a note in your own words, and even then it stays an unreviewed draft
  until you review it.
- **Baselines & impact:** compare a baseline with the working copy, see what each
  change reaches, roll back (which first saves a safety baseline), and create the next
  baseline once nothing blocks it.

## Asking the model

**Ask the model**, at the top right, opens a chat that reads the model through tools,
which show as small chips above each answer. It can't change anything. **Highlight on
diagram** selects the parts an answer is about, and **Attach to proposal** adds the
answer to an open question in the Review queue. A number the tools didn't give shows as
`[value]`, so the chat can't slip in figures of its own.

## AI rules worth knowing

- The AI asks questions and sketches drafts. Values, links and commits come from you.
- Every call is logged with its prompt, model and cost. **See the prompt** shows what was sent.
- When a local server is down or a cloud key is missing, the panel says so in plain
  words, and the rest of the app keeps working.

## Keyboard and accessibility

Everything works from the keyboard. Focus always shows as a blue outline. Diagrams
have the tree and tables beside them, and colour is never the only signal: gaps say
"Gap", and AI items carry the AI badge or a dashed outline.
