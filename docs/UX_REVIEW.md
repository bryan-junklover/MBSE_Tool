# ModelBench UX/UI review

**Date:** 2026-11-05 · **Branch:** `phase-7-chat-exports` (202390c) · **Sample:** Survey UAV
**Method:** I read `HANDOFF_BEHAVIOUR_AND_STYLE.md`, `docs/USER_GUIDE.md`, `app/R/*.R`, `app/www/*.css|js` and the `mbdiagram` widget. Then I ran the app (`.claude/launch.json` → `modelbench`) and went through every workspace at 1920×1080, 1440×900, 1280×800, 1024×768 and 900×800.

This document is for the coding agent. Each item gives the **evidence**, the **cause** where I found one, and a **fix** with a done-when check. Priorities:

- **P0:** broken or misleading. Fix first.
- **P1:** a large usability cost on a core workflow.
- **P2:** polish and consistency.

The handoff's direction still applies: calm and dense, structure from type rather than colour, two hues only. None of the fixes below should add colour, icons or decoration.

---

## Summary

The visual language is consistent and close to the handoff. Typography, tokens, the AI-blue/amber semantics and the copy voice all work. Health and Stakeholder are the strongest screens.

The main problem is the **layout model**:

1. The shell is a single document that scrolls as a whole.
2. Every diagram is a fixed-height box (480–620px) with no zoom and no fit-to-view.
3. The three columns use fixed flex bases that don't respond to what each screen needs.

As a result:

- the canvas is too small on large screens and loses its context on small ones;
- the inspector, the place where edits happen, is pushed off-screen when the chat opens or below about 1100px;
- side panels are either crammed (scenario steps, state machine sidebar) or empty (Draft coach column, Specification inspector before a selection).

The highest-leverage change is **§1: make the shell a fixed-height app frame with independently scrolling panes and a canvas that fills the available space.** Many of the later items get smaller or go away once it's done.

---

## P0: bugs found while testing

### 0.1 Button labels disappear after a click (all `actionButton`s) — **Fixed** (branch `ux-p0-fixes`)
- **Evidence:** I clicked *Auto-layout* on Model › Structure. The button then showed as a blank grey box. Computed style while focused: `color: rgb(244,244,242)` on `background: rgb(236,237,239)`, so the label is invisible. It stays that way until focus moves.
- **Cause:** `shiny::actionButton(..., class = "mb-btn")` keeps Bootstrap's `.btn .btn-default` classes. The bslib theme's `:focus`/`:active` state for `.btn-default` sets near-white text, and `.mb-btn` only overrides `:hover`.
- **Fix:** in `app.css`, give `.mb-btn` explicit `:focus`, `:active` and `.active` rules that keep `color: var(--ink)` and the `--surface`/`--surface-muted` background. Do the same for `.mb-btn-primary` and `.mb-btn-ai`. A cleaner option is a small `mb_button()` helper that emits `class = "action-button mb-btn"` without `btn btn-default`.
- **Done when:** clicking any toolbar button leaves its label readable, checked in both the Structure and Scenarios toolbars.

### 0.2 Empty options in the scenario step "Kind" and "Fragment kind" dropdowns — **Fixed** (branch `ux-p0-fixes`)
- **Evidence:** in Behavior › Scenarios, the first column of the step table shows an empty select. Its options are `<option value="message" selected></option>…`, all without a label.
- **Cause:** `select_tag()` (`app/R/mod_model.R:111`) labels options with `names(options)`. `mod_behavior.R:42` and `:44` pass unnamed vectors, so every label is `NULL`.
- **Fix:** in `select_tag()`, default the names: `if (is.null(names(choices))) names(choices) <- choices`. Even better, pass human labels: `c("Message" = "message", "Self call" = "self", "Note" = "note", "Fragment start" = "fragment_open", "Else" = "fragment_else", "Fragment end" = "fragment_close")`.
- **Done when:** every select in the step table shows its current value. Add a test in `test-mod_behavior.R` that each `<option>` has text.

### 0.3 The simulator's current state is often off-screen — **Fixed** (branch `ux-p0-fixes`)
- **Evidence:** in Behavior › State machines, I fired `arm`. The run bar said "Armed", but the Armed node wasn't visible. The canvas is 1070×480 and its content is 1070×1438, so about two thirds of the machine is hidden. The handoff's main simulator interaction (§5.8, checklist item 10) can't be watched.
- **Fix:** after each step, scroll the active state into view, centred, or fit the whole machine (see §1.3). Briefly pulse the transition that was taken.
- **Done when:** firing `gnss_lost` from Survey mission leaves Return to home visible without any manual scrolling, at 1440×900.

### 0.4 Side-by-side impact hides the change it exists to show — **Fixed** (branch `ux-p0-fixes`)
- **Evidence:** in Behavior › Behavioral impact › Side by side, each pane is about 300px wide. The changed nodes ("Compute energy needed…", "Below RTH reserve?") are cut off at the right edge of both panes. Panes are 620px tall and scroll separately.
- **Fix:**
  - fit each pane to its content (§1.3);
  - sync scroll and zoom between Before and After;
  - on open, frame the bounding box of the changed nodes.
  - Also hide port handles in this read-only view (`tool = "select_only"` should suppress the small square handles).
- **Done when:** both panes show the full activity at 1440 wide with changed nodes visible, and there are no editing handles.

### 0.5 The opening diagram viewport cuts off a block — **Fixed** (branch `ux-p0-fixes`)
- **Evidence:** Model › Structure at 1440 opened with `scrollLeft = 260`. Flight Termination Unit showed as a clipped "Unit" sliver at the left edge, and Power System was clipped the same way on Flows. At 1024 wide the same diagram showed it in full.
- **Fix:** on render and on view switch, either reset scroll to the content's top-left or fit to content. Never keep a scroll offset from a previous layout.

---

### 0.6 Widget script is cached across changes (new, found while fixing 0.3–0.5)
- **Evidence:** after `tools/install_widgets.R`, the browser kept running the old `mbdiagram.js`. The server sent the new file, but the URL (`mbdiagram-binding-0.0.0.9000/mbdiagram.js`) hadn't changed, so a normal reload didn't pick it up.
- **Fix:** bump the `mbdiagram` package `Version` whenever the widget changes (the dependency URL includes it), or add a version stamp to the `script:` entry in `mbdiagram.yaml`. This is the same lesson as `asset_url()` in `app_ui.R`.
- **Status:** open.

## 1. Layout model (P1, structural)

### 1.1 Make the shell an app frame, not a scrolling document
- **Evidence:**
  - `app_ui.R` wraps everything in `div(style = "display:block;overflow:auto")` inside `page_fillable`, which cancels bslib's fill behaviour.
  - `.mb-body` uses `min-height: 70vh`.
  - The top bar, status strip and tab bar scroll off with the content. On Draft at 900px tall, Commit is below the fold, and scrolling to reach it loses the workspace navigation.
  - On large screens the body stops at 70vh, leaving a band of `--ground` (visible on Interfaces and Draft).
- **Fix:**
  - The frame is `100vh` with `display: grid; grid-template-rows: auto auto auto 1fr`: top bar, status strip, tab bar, body.
  - `.mb-body` gets `min-height: 0; height: 100%`.
  - `.mb-left`, `.mb-main` and `.mb-right` each get `overflow: auto`, so each column scrolls on its own and the chrome stays put.
  - Remove the inline `display:block;overflow:auto` wrapper, or replace it with the grid container.
  - Long-form pages (Project, Health, Stakeholder) can keep a single scrolling main pane inside the frame.
- **Done when:** on every workspace the top bar and tab bar stay visible while content scrolls, and there's no band of ground under the body at 1920×1080.

### 1.2 Make diagram canvases fill the space they're given
- **Evidence:** heights are hard-coded: Structure/Flows/Functional `560px` (`mod_model.R:44`), state machine `480px` (`mod_behavior.R:447`), activity `540px` (`:111`), impact `620px` (`:853–855`). At 1920×1080 the Structure canvas is 1250×560 with about 350px of empty page below it. At 1440×900 it's 771×560 for 1070×1018 of content, so it needs two scrollbars.
- **Fix:**
  - In a fixed frame (§1.1), make `.mb-main` on diagram screens a flex column.
  - The diagram wrapper gets `flex: 1 1 auto; min-height: 360px`. Call `mbdiagramOutput(height = "100%")`.
  - The widget already has `resize: function () { if (data) fit(); }`, so it only needs a parent with a real height.
  - Move the facts line and the folded "Connections (n)" table into a footer strip, or a collapsible bottom drawer, so they don't compete for height.
- **Done when:** resizing the window from 900 to 1080 tall grows and shrinks the canvas, with no fixed `px` heights left in `mbdiagramOutput()` calls.

### 1.3 Add zoom, fit-to-view and pan to `mbdiagram`
- **Evidence:** `fit()` in `mbdiagram.js:291` only grows the paper to the content size. There's no `paper.scale`, so large models can only be scrolled. That causes 0.3, 0.4 and 0.5.
- **Fix:**
  - Add a small overlay control (bottom-right of the canvas, mono 12, secondary-button style) with **Fit**, **−**, **100%** and **+**.
  - Implement with JointJS `paper.transformToFitContent({ padding: 24, maxScale: 1, minScale: 0.4 })` and `paper.scale()`.
  - Ctrl or ⌘ plus the wheel zooms. Dragging empty canvas pans in Select mode.
  - Default to **Fit** on first render and after Auto-layout.
  - Keep the "Keep this position" pinning working under scale: convert coordinates with `paper.clientToLocalPoint`.
  - Add `aria-label`s to the controls and support the `+`, `-` and `0` keys when the canvas has focus.
- **Done when:** the Survey UAV structure diagram fits entirely in the canvas at 1440×900, and zooming in and out keeps the selection.

### 1.4 Make side columns resizable and collapsible
- **Evidence:** the left column is fixed at 240–280px and the right at 340–400px, on every screen. Editing screens need a wide inspector, and reading screens need a wide main column.
- **Fix:**
  - Add a drag handle between columns, with the width saved in `localStorage` under the existing try/catch.
  - Add a collapse control ("Hide tree", "Hide inspector") that leaves a 28px rail with a vertical label.
  - Set per-screen defaults (see §3).
- **Done when:** the user can widen the inspector on Specification and collapse the tree on Structure, and both survive a reload.

### 1.5 Opening the chat must not push the inspector off-screen
- **Evidence:** at 1440×900 with the chat open, `.mb-body` gets `margin-right: 420px`. The inspector then wraps under the main column, to `y = 938`, below the fold. This is the "Highlight on diagram" workflow from the guide, so the user loses the inspector exactly when the chat asks them to look at an element.
- **Fix:**
  - Opening the chat should first auto-collapse the left column (§1.4), and then let the chat overlay rather than push below about 1600px.
  - Or dock the chat inside the right column as a tab ("Inspector | Ask"), which fits the 300–380px column the handoff already defines.
  - Also make `.mb-left` and `.mb-right` `flex-shrink` instead of wrapping when the main column would fall under its `560px` basis.
- **Done when:** with the chat open at 1440×900 on Structure, the canvas, inspector and chat are all visible together.

### 1.6 Responsive stacking puts the inspector two screens down
- **Evidence:** at ≤1100px, the media query moves `.mb-main` first and stacks the tree and inspector beneath it. At 900×800 the inspector starts at `y = 1478`. Selecting a block gives no visible feedback that anything changed.
- **Fix:** below 1100px:
  - show the tree as a slide-in drawer, toggled by a "Model tree" button in the toolbar;
  - show the inspector as a bottom sheet or right overlay that opens when something is selected, with Esc to close;
  - keep the canvas at the top.
- **Done when:** at 900×800, selecting a block shows its inspector without scrolling. Handoff checklist item 16 still passes (no horizontal page scroll).

---

## 2. Space versus usability, screen by screen

### Write › Draft
- **2.1 (P1) Empty right column; crowded centre.** Before the gate passes, the Coach column holds one sentence and a button in about 25% of the width. Meanwhile the centre stacks text, metadata, parsed sentence, gate ledger, terms and actions, which pushes **Commit** below the fold at 900px tall.
  - **Fix:** move the **gate ledger** into the right column above the Coach panel, so the right column becomes "checks": gate (no AI) then coach (AI).
  - Keep the parsed sentence and terms under the textarea.
  - Pin the action row (Commit, Save draft, status text) to the bottom of the main pane with `position: sticky; bottom: 0`.
- **2.2 (P2) The gate ledger reads as failing at 100.** "score 100 · FAIL" sits next to rows of `0` with no pass/fail mark. A 0 row could mean "failed" or "not applicable".
  - **Fix:** add a mark column (✓ or —, or the words "pass"/"n/a"), put the hard-rule failure first in the ledger as well as above it, and label the score "100 (blocked by 1 rule)".
- **2.3 (P2) Outline text crammed into the left column.** Entries such as "STK-001 Map an area in one flight" and "SYS-031 Battery temperature · 100" wrap onto two lines at 240px, so the outline is about twice as tall as it needs to be.
  - **Fix:**
    - single-line rows with `text-overflow: ellipsis` and a `title` for the full name;
    - the ID in a fixed mono gutter;
    - the score right-aligned in its own column;
    - indent 12px per level rather than 8px plus a different text colour.
  - Combined with §1.4 the user can widen the column.
- **2.4 (P2) Draft uses selectize while inspectors use native selects.** "Parent / Level / Type / Verify by" use selectize, and the Model inspector uses native `<select>`. They differ in height and style.
  - **Fix:** pick one: native `selectInput(selectize = FALSE)` styled with tokens is lighter and matches the dense UI.

### Write › Specification
- **2.5 (P1) Requirement text wraps three or four lines, with the inspector column empty.** With nothing selected, about 25% of the width says "Select a requirement to see its details", while the Requirement column wraps heavily.
  - **Fix:** collapse the inspector until a row is selected (§1.4), or give the table the full width and open the inspector as an overlay.
  - Shrink Type, Verify by, Status and Score to fit their content (`white-space: nowrap`), and give Requirement the remaining width.
- **2.6 (P2) Import and export dominate the toolbar.** The file input is the widest control and visually louder than search.
  - **Fix:** use an **Import…** button that opens the file picker, and an **Export ▾** menu with ReqIF, CSV, Markdown and PDF.
  - Search goes left, filters next to it, then "+ Requirement" as the primary action.
- **2.7 (P2) The inspector is a long single-column form.** Short title, Parent, Type and Verify by each take a full row, and Quality findings and Verification fall below the fold.
  - **Fix:** use a two-column field grid at inspector widths of 360px and up. Fold Rationale until it's used.

### Write › Traceability
- **2.8 (P2)** At about 800px the matrix columns wrap component names over two lines and the requirement column wraps titles.
  - **Fix:** rotate column headers 45° or wrap them to a fixed height.
  - Make the first column and the header row sticky (`position: sticky`) so both stay visible while scrolling.
  - Show a row and column hover crosshair to help read across.

### Model › Structure, Flows, Functional
- **2.9 (P1) Connectors run through blocks and labels overlap.** "PowerBus28V" sits on top of the Payload Camera block, and on Functional the labels collide ("E: electrical D: mission_pla D:"). Several edges cross straight through blocks.
  - **Fix:** use ELK orthogonal routing (`elk.edgeRouting: ORTHOGONAL`) when running Auto-layout, and place label backgrounds with collision offsets.
  - Optionally hide edge labels below 70% zoom (once §1.3 is in).
- **2.10 (P1) Auto-layout gives no feedback.** After the click nothing visibly moved (the label also vanished, see 0.1).
  - **Fix:** animate to the new positions, then **Fit**. If nothing moved, show a caption such as "Layout unchanged: N blocks are pinned".
- **2.11 (P2) The toolbar wraps at 1280.** Select, Component, Port, Connect, Group, Undo, Redo and Auto-layout spill onto a second line.
  - **Fix:** group the tools into a segmented control, then a divider, then Undo and Redo as compact buttons with `aria-label`s, then Group and Auto-layout in an overflow ⋯ menu below 1300px.
- **2.12 (P2) The selection outline replaces the gap outline.** When Payload Camera was selected, its amber dashed border was replaced by the solid ink selection border, so the gap was shown only by the caption.
  - **Fix:** draw selection as an outer 2px ink outline, offset 3px, so the gap and sketch treatments stay visible underneath.
- **2.13 (P2) Flows has no legend on the canvas.** Line weight and dash encode data, energy and matter, but the "Show" checkboxes have no line samples.
  - **Fix:** draw a 24px line sample next to each checkbox label.
- **2.14 (P2) The Functional inspector shows the last component from Structure.** On Functional the right column showed "C-001 Navigation Sensors" component fields, carried over from Structure.
  - **Fix:** clear the selection when switching to a view that doesn't contain it, or label it "Selected in Structure".
- **2.15 (P2) Mixed counts.** Structure says "8 components" and Health says "Components 10".
  - **Fix:** say "8 components on this level" on Structure.

### Model › Interfaces
- **2.16 (P2)** The layout is fine. *Delete interface* sits directly under the form at the same weight as other buttons.
  - **Fix:** move it into the inspector footer and add a confirm step that names the number of ports that use it.

### Behavior › Scenarios
- **2.17 (P1) The step table is crammed.** It has ten columns in about 640px: Kind, From, To, Interface, Field, Label, Guard, Timing, and the ↑ ↓ × buttons. Selects truncate ("Ground Control Softv", "CommandMsg: start_"), and each row is a mass of borders.
  - **Fix:**
    - Let the sequence diagram and the table share space through a vertical splitter (§1.4), or a "Diagram | Steps" toggle below about 1300px.
    - In the table, show only Kind, From → To, Label and Timing.
    - Expand a row, or use the right inspector, for Interface, Field and Guard.
    - Drop the ↑ ↓ buttons (drag already reorders) in favour of a row ⋯ menu with Move up, Move down and Delete, so keyboard users still have them.
- **2.18 (P1) The AI sketch form is crammed into the scenario list column.** Name, a "What happens" textarea, As and Sketch are squeezed into a 280px sidebar under the list, and push the list's own content.
  - **Fix:** turn "New scenario…" into a split button: "Blank" or "Sketch with AI…". The second opens a dialog or a right-column panel with room for a proper description.
- **2.19 (P2) The sequence diagram leaves space unused.** It uses the left 60% of its box and has a fixed lifeline spacing.
  - **Fix:** spread lifelines across the available width, with a minimum of 140px each.
  - Also make the "Lifeline / Message / Fragment / Note" add-buttons read as additions: "+ Lifeline" and so on.

### Behavior › State machines
- **2.20 (P1) The left column holds four panels.** State machine picker, Completeness (with buttons per pair), AI questions, Requirement checks, Add assertion and Events & variables are all stacked in 320px. That's the screen's main verification content, crammed into a sidebar.
  - **Fix:** make the left column a list and picker only: machine, states, events and variables. Move **Completeness** and **Requirement checks** into a right column with tabs: "Inspector | Completeness | Checks".
- **2.21 (P1) Simulator controls are split around the canvas, and the inspector is below everything.** The run bar is above the canvas, the event buttons and trace are below it, and the state or transition editor is further below ("Select a state or transition to edit it.").
  - **Fix:**
    - group run bar, events and trace as one simulator strip under the canvas, with the trace collapsible and its height capped;
    - put the state or transition editor in the right column (above);
    - mark the valid next events (`.valid`) with a text cue too ("can fire"), not just fill.
- **2.22 (P2) "Add assertion…" uses raw JSON textareas.** "Script (JSON)" and "Expected (JSON)" are hard to use in a narrow column.
  - **Fix:** use a small builder (start state, then a list of events, then expected state), with "Edit as JSON" as an advanced fold. Put it in a dialog.

### Behavior › Behavioral impact
- **2.23 (P2)** Covered by 0.4. Also, the "Change" select is 360px wide and still truncates long change names.
  - **Fix:** let it grow, or use the same card list as Baselines.

### Analyze › Health
- **2.24 (P2)** This screen works well: a clear banner, grouped blockers with links, and cards. It caps at 1320px and centres, which is right for reading.
  - **Fix:** link every count in the cards when it's above 0. "Messages using a missing field 2" and "Unhandled event/state pairs 3" aren't links yet, while their neighbours are.

### Analyze › Review queue
- **2.25 (P1) Long, repetitive cards; both side columns mostly empty.** Four structural gaps of the same kind each take a full card with the same three buttons. The left filter column uses about 15% of the width for five controls, and the right column holds a short paragraph and one button.
  - **Fix:**
    - group cards by kind ("No linked requirements (3)"), each with one compact row: name, then the actions on the right;
    - move Source and Status filters into a horizontal filter bar above the list;
    - put "Ask AI about gaps" in the tab bar actions, which is the shell's pattern;
    - drop both side columns on this screen.
- **2.26 (P2) No way back after "Show on diagram".** It jumps to Structure with the element selected, but there's no return to the queue item.
  - **Fix:** a dismissible caption bar on the target screen, "From Review queue: Payload Camera port trigger_in has no connector · Back to queue".

### Analyze › Baselines & impact
- **2.27 (P1) The impact table buries the narrative.** 28 rows come before the narrative, and every Path repeats the root ("Power System · Battery capacity → …").
  - **Fix:**
    - put the narrative (Technical and Stakeholder tabs) **above** the table, or in the right column;
    - remove the common path prefix and show it once in the table caption;
    - group rows by hop count, folding 3+ hops by default;
    - sort by significance, then hops.
- **2.28 (P2) The Create baseline dialog lists blockers as plain text.** It's the same list as Health, but there it has links.
  - **Fix:** reuse the Health blocker renderer so each item links to its fix, and close the dialog when a link is followed.
  - Also consider showing the "8 blockers" state on the button itself ("Create baseline BL-04 · 8 blockers"). It's currently a solid ink primary button for an action that can't succeed.

### Project
- **2.29 (P1) One very long page with no tab bar.** Project, Scope, Glossary, ID prefixes, AI providers, Task routing, Coaching policy and Data protection are stacked in one column capped at 1200px and aligned left. It's the only workspace without a tab bar.
  - **Fix:** add the standard tab bar: **General** (project, scope, prefixes), **Glossary**, **AI** (providers, routing, policy), **Data protection**. Or keep one page and add a sticky left section nav. Centre the 1200px column the way Health does.
- **2.30 (P2) Task routing has truncated inputs.** Model name inputs are about 90px ("claude-sonnet-5-5" fits only just), and the "Local only…" checkbox label wraps in a narrow cell.
  - **Fix:** make model inputs `min-width: 180px`, and set the table to `table-layout: auto` inside `.mb-table` (it already scrolls). Give Data protection the full width.

---

## 3. Recommended per-screen column defaults (after §1.4)

| Screen | Left | Main | Right |
|---|---|---|---|
| Draft | outline 260 (collapsible) | editor | gate + coach 380 |
| Specification | outline 260 | table | inspector 400, **collapsed until selection** |
| Traceability | — | matrix (sticky header and first column) | candidate panel on selection |
| Structure / Flows / Functional | tree 240 (collapsible) | canvas, fill | inspector 340 |
| Scenarios | list 260 | diagram / steps splitter | step inspector 340 on selection |
| State machines | machine, events, vars 260 | canvas + simulator strip | Inspector / Completeness / Checks 360 |
| Behavioral impact | affected list 280 | before/after, fit and synced | — |
| Review queue | — | filter bar + grouped list (max 1000, centred) | — |
| Baselines | change cards 300 | narrative + table | — |
| Project, Health, Stakeholder | — | centred reading column (max 1200–1320) | — |

---

## 4. Cross-cutting polish (P2)

- **4.1 The status strip dims during recalculation.** After "Show on diagram" the strip faded (Shiny's `.recalculating` opacity).
  - **Fix:** `#status_strip.recalculating { opacity: 1; }`, since it refreshes often and shouldn't flicker.
- **4.2 Destructive buttons look like every other button.** "Delete from the model", "Remove" and "Delete interface" use the secondary style.
  - **Fix:** keep them secondary (no new hue), but separate them: push them to the end of their group with a rule above, word them by effect ("Delete Payload Camera…"), and always confirm.
- **4.3 Toolbars repeat Undo and Redo.**
  - **Fix:** wire ⌘Z and ⇧⌘Z (Ctrl on other platforms) when the canvas has focus, and show the shortcut in the button `title`.
- **4.4 Empty states.** "Select a block or connector to see its details." sits in an otherwise empty 340px column.
  - **Fix:** when the column is visible and empty, show useful summary content instead: on Structure, the level's roll-ups and open gaps, which are currently in the facts line under the canvas.
- **4.5 Tab bars use the space unevenly.** Workspace tab bars show only the segmented control and leave the right side empty on most screens. The handoff (§3) puts "the screen's own actions on the right".
  - **Fix:** move screen-level actions there consistently: Draft's "New requirement", Review queue's "Ask AI about gaps", Health's "Recompute", the diagram's Auto-layout and Fit.
- **4.6 Keep page-level horizontal scroll at zero.** At 900 wide, `scrollWidth` was 885 (OK). Re-check after the column changes. Add a shinytest2 check of `document.documentElement.scrollWidth <= innerWidth` at 900px on every tab.

---

## 5. Suggested order of work

1. **P0 bugs 0.1 and 0.2.** Small CSS and helper fixes, with immediate benefit.
2. **§1.1 app frame and §1.2 fill canvases.** One change to the shell and CSS, plus removing the fixed heights.
3. **§1.3 zoom and fit in `mbdiagram`.** Fixes 0.3, 0.4 and 0.5 along the way. Rebuild with `Rscript tools/install_widgets.R`.
4. **§1.4–1.6 resizable and collapsible columns, chat docking, and narrow-width drawers.**
5. **P1 screen items:** 2.1, 2.5, 2.17, 2.18, 2.20, 2.21, 2.25, 2.27, 2.29.
6. **P2 polish.**

After each step, run `Rscript tools/run_tests.R` and the handoff §8 browser checklist. Items 9, 10 and 16 are the most likely to be affected by layout changes.
