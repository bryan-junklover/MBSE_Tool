# ModelBench UX re-review

**Date:** 2026-11-05 · **Reviewed:** `ux-review-fixes` @ 3dc2a81, against [UX_REVIEW.md](UX_REVIEW.md)
**Method:**
- Installed the branch's `mbdiagram` (0.1.3) and ran the app.
- Checked every screen at 1440×900, and the layout at 1920×1080, 1024×768 and 900×800.
- Ran `tools/run_tests.R` and `tools/lint.R`, and stress-tested the diagram with a synthetic 150-component model.

## Verdict

The branch delivers the structural change the first review asked for, and most screens are clearly better.

- The app is now a fixed frame, and every diagram fills its space and has Fit and zoom.
- Columns resize and collapse, and opening the chat keeps the canvas, inspector and chat all on screen.
- There's no page-level horizontal scroll on any of the 15 screens at 900px.
- Tests and lint pass.

**Three regressions need fixing before merge** (R1–R3 below). R1 affects every dropdown in the app.

## What passed

| Check | Result |
|---|---|
| §1.1 app frame | Document height equals the viewport (900 = 900); the chrome stays put and panes scroll on their own |
| §1.2 canvases fill | Structure fills the main column at every size; no fixed `px` heights remain |
| §1.3 zoom and fit | Fit / − / % / + controls, Ctrl or ⌘ + wheel, and the `+ - 0 1` keys; the sample opens fitted (69%) |
| §1.4 columns | Drag to resize, hide to a labelled rail, and both are remembered |
| §1.5 chat | At 1440×900 the tree folds to a rail, and canvas, inspector and chat are all visible |
| §1.6 narrow widths | At 900×800, selecting a block opens the inspector as an overlay |
| 0.3 simulator (handoff §8 item 10) | Survey mission → `gnss_lost` reaches Return to home at 3.0 s, still in view; SYS-012 PASS |
| 0.4 side by side | Both panes fit, scroll together and show no handles |
| 2.1 Draft | Gate and coach share the right column; the Commit bar stays pinned at the bottom |
| 2.12 selection | The gap's dashed outline survives selection |
| 2.17 / 2.18 Scenarios | Splitter between diagram and steps; compact step rows with a ⋯ menu; "Sketch with AI…" moved into the toolbar |
| 2.20 / 2.21 State machines | Inspector, Completeness and Checks tabs; one simulator strip; "Can fire now" given in words |
| 2.25 / 2.26 Review queue | Grouped list with a filter bar; Show on diagram leads to a "From Review queue … Back to review queue" bar |
| 2.27 / 2.28 Baselines | Narrative above the table; rows grouped by where they start; button reads "Create baseline BL-04 · 8 blockers" |
| 2.29 Project | Tabs: General / Glossary / AI / Data protection, in a centred column |
| Handoff §8 items 12 and 16 | 3 changes listed and 8 blockers named; no horizontal scroll at 900 |
| Performance (BUILD_PLAN: 150 components < 1 s) | 150 blocks and 200 connectors with manhattan routing render in 613 ms; a drag step takes 3.3 ms |

The two small core changes are sound and tested: `requirement_evidence()` with zero test cases, and the transition id now recorded in the simulator trace.

## Regressions: fix before merge

### R1 (P0) Every native dropdown has lost its arrow — **Fixed**: `background-color` on the three select rules; static check in `test-render.R`. All 28 visible selects across 14 screens show an arrow.
- **Evidence:** in Draft (Parent, Level, Type, Verify by), the Specification inspector, Baselines (Compare) and others, selects look like text boxes. Computed `background-image: none` on `#spec-d_type`.
- **Cause:** the 2.4 change moved 39 inputs to `selectize = FALSE`, so they're now `.form-select`. `app.css:76` sets `background: var(--surface) !important` on `.form-select`, and that shorthand also wipes out Bootstrap's chevron `background-image`.
- **Fix:** change that rule to `background-color: var(--surface) !important` (keep `background-image`), or split `.form-select` into its own rule. Check that `.mb-inspector select` and `.mb-table-input` still show an arrow.
- **Done when:** every `<select>` shows a chevron. Add a static check in `test-static_checks.R` that `app.css` never sets the `background` shorthand on `.form-select`.

### R2 (P1) Traceability column headers are cut off — **Fixed**: the cause was each opaque sticky head painting over the name leaning in from its left. Heads now stack left over right (inline `z-index`), with 120px room after the table. Every name reads in full at 1440 and 1024, and the head still covers rows when scrolled.
- **Evidence:** the 45° headers read "Navigat", "Flight Co", "Power Sy", "Propulsi", "Comms l". The last header ("Flight Termination Unit") runs out above the header row, and the clipped end of "Ground Control Software" shows on top of it. The columns are about 24px wide and the matrix uses less than half the main column.
- **Fix:**
  - Give the header row a height worked out from the longest name, about `0.71 × name length × 7px + 16`, or cap names at roughly 22 characters with a `title`.
  - Rotate the inner `<span>`, anchored bottom-left, not the cell, and let it overflow into the header area.
  - Give columns a minimum of 32px, so cells also meet the 32px target rule.
- **Done when:** every component name can be read in full (or ends in "…" with a tooltip) at 1440 and 1024 wide.

### R3 (P1) Fit makes state machines unreadable — **Fixed**: Fit stops at 70% in editable behaviour views and keeps the running state in view. State-machine Auto-layout runs left to right, wrapping to the view's shape: the sample fits at 70% with every label shown. `mbdiagram` is 0.1.6.
- **Evidence:** State machines opens at 48% and drops to 40% once the simulator starts. State nodes are 28px tall, labels about 5px, and a vertical scrollbar remains. The ELK layout is `DOWN`, which turns this machine into a tall thin chain using about a third of the canvas width.
- **Fix:**
  - For `state_machine` (and activity) layouts, choose the direction from the canvas shape (`RIGHT` when the canvas is wider than it is tall), or use `elk.aspectRatio` set to the canvas's width ÷ height.
  - Give Fit a readability floor for behaviour views of about 0.65, and scroll beyond that. Fit should also follow the current state, which 0.3 already does.
- **Done when:** at 1440×900 the Flight modes machine is readable (scale ≥ 0.65) with the current state in view.

## Smaller issues (P2)

All nine were fixed after R1–R3; `mbdiagram` is 0.1.7.

1. **The narrow inspector overlay hides the selection.** At 900×800, selecting Ground Control Software opened the overlay right on top of it. When the overlay opens, scroll the canvas so the selected element sits in the part still visible, or overlay from the side the selection isn't on. — **Fixed**: an overlay opening fires `mb:overlay`, and the widget scrolls the selection into the part of the canvas left open (widening a fitted paper if needed). At 900×800 Ground Control Software sits beside the overlay.
2. **The column splitter can't be used from the keyboard.** `.mb-split` has `role="separator"` but no `tabindex`, `aria-valuenow`, `aria-valuemin` or `aria-valuemax`, and no arrow-key handling. The handoff (§7) says everything works from the keyboard. Its hide button (‹ ›) is 19×26px, under the 32px target, and invisible until hover. Show it at 40% opacity at rest. — **Fixed**: the splitter is focusable with `aria-valuenow/min/max`; ← → resize by 16px (⇧ 64px), Home/End go to the limits. The hide button is 24×32px and shows at 40% at rest.
3. **Specification table overflows by 10px when the inspector is open.** The table is 779px in a 769px box, so the Score column, which the table is sorted by, is clipped and needs a horizontal scroll. Let the table recalculate its column widths when its box changes size (call `Reactable` resize on the column's `resize` event), or give Score a fixed narrow width. — **Fixed**: Requirement's minimum width is 180px, so the columns need 739px; the table fits its 769px box.
4. **Draft outline titles truncate too early at 240px.** Examples: "Map an area in one fli…", "Battery temp…", "Ground sample dist…". The nested ID gutter takes about 40% of the row. Either indent the title rather than the ID, or default this column to 280px, or allow two lines with `-webkit-line-clamp: 2`. — **Fixed**: titles wrap to two lines, the depth indent is 8px a level and the column defaults to 280px. No title in the sample is cut.
5. **The gate's blocking message appears three times:** in the gate header, in the ledger's "blocks" row and in the Commit bar. Keep the ledger row and the Commit bar, and drop the `.mb-hard` line, since the header verdict already says "blocked by 1 rule". — **Fixed**: the `.mb-hard` line is gone; the ledger row and the Commit bar remain.
6. **The top bar wraps to two rows below about 1000px:** 103px tall at 900 wide, where "Ask the model" drops to its own line. Truncate `.mb-context` with an ellipsis (`min-width: 0; overflow: hidden; text-overflow: ellipsis`) before letting the bar wrap. — **Fixed**: the bar no longer wraps and `.mb-context` ends in an ellipsis; 57px tall at 900 wide.
7. **Parallel connectors share one channel.** On Structure, several PowerBus28V routes run on top of each other, and two "PowerBus28V" labels stack. The manhattan router gives every link the same `padding: 16`. Offset links that leave the same side of the same block (padding 16 + 8n), or label only one link per interface in a bundle. — **Fixed**: links leaving the same side of a block take padding 16 + 8n (n ≤ 4). On Structure no two routed links share a segment and no labels overlap.
8. **Baselines "Compare" select truncates** to "BL-03 · Baselin". Let it size to its content (`width: auto`), or show only the id with the name in its `title`. — **Fixed**: both selects size to their content ("BL-03 · Baseline 3" shows in full).
9. **4.6 is still open:** there's no shinytest2 check for horizontal scroll at 900px. My manual sweep found none on all 15 screens, but nothing guards it in CI. — **Fixed**: `app/tests/testthat/test-layout.R` drives the app in headless Chrome (shinytest2), clicks through all 15 screens at 900×800 and fails on any sideways scroll. `MODELBENCH_SKIP_BROWSER_TESTS=true` leaves it out.

## Third pass: the P2 fixes (d02ec9d, 1d6fddc)

**Method:**
- Ran `mbdiagram` 0.1.16, checking that the browser loaded `mbdiagram-binding-0.1.16`.
- Checked each item at 1440×900, 1024×768 and 900×800.
- Ran `tools/run_tests.R`, which includes the new `test-layout.R` sweep, and `tools/lint.R`.
- Rebuilt the 150-block, 200-link stress model in the page.

**Verdict:** all nine P2 items pass their "done when". Tests and lint pass. There's one new P2 problem (N1), plus a few small notes.

| Item | Result |
|---|---|
| P2.1 overlay | At 900×800, selecting Ground Control Software opens the inspector with the block in the free part of the canvas |
| P2.2 splitter | Focusable; `aria-valuenow/min/max` are set. → +16, ⇧→ +64, Home 180, End 680 all work. The button is 24×32 and shows at 40% at rest; there's a blue bar on focus |
| P2.3 Specification | With the inspector open, the table is 769px in a 769px box with no sideways scroll; Score shows in full |
| P2.4 outline | The column is 280px; none of the 13 titles is cut |
| P2.5 gate | No `.mb-hard` line; the header verdict, the ledger rows and the Commit bar remain |
| P2.6 top bar | 57px tall at both 1024 and 900, and the context ends in "…" |
| P2.7 connectors | On Structure, no link crosses a block and no labels overlap. The only shared run is N4 |
| P2.8 Baselines | "BL-03 · Baseline 3" shows in full (157px) |
| P2.9 layout test | Passes (17 expectations) |
| R3 still holds | State machines fits at 0.70 |
| Performance | 150 blocks and 200 links render in 713 ms, under 1 s (613 ms before). A drag step takes a median of 0.4 ms. Each drop re-routes every link, which takes about 230 ms (N3) |

### N1 (P2) Closing the narrow overlay leaves the canvas shifted — **Fixed**: `apply()` now sends `mb:overlay-closed`. The widget keeps the view from before the first reveal, shrinks the paper back, then re-fits or restores the scroll. At 900×800 the fitted view returns exactly (810px paper, scroll 0, 58%), and so does a zoomed one (84%, scroll 0/60). One self-removing listener per widget also covers N5. `mbdiagram` is 0.1.18.
- **Evidence:** at 900×800, select Ground Control Software, then click empty canvas.
  - The overlay closes, but the paper stays widened to 1040px and scrolled 230px right.
  - Power System and Flight Termination Unit are cut off on the left, and there's empty space on the right.
  - The zoom still says 58%, and only Fit brings the diagram back.
- **Cause:** `reveal()` widens the paper and scrolls it when the overlay opens. Nothing undoes that on close, and a closing overlay doesn't resize the holder, so `resize` → `fitAll` never runs.
- **Fix:** when `apply()` closes an overlay, send `mb:overlay-closed`. Have the widget save its scroll position (and whether it was fitted) when it handles `mb:overlay`, then restore it on close: `fitAll(false)` if it was fitted, otherwise put the scroll and paper width back.
- **Done when:** at 900×800, opening and closing the inspector leaves the diagram exactly as it was.

### Smaller notes (P3)
- **N2 Specification's Requirement column is now the squeeze point.**
  - At 1440 with the inspector open, Requirement is 199px and most rows wrap to 3–4 lines, so about 9 rows fit on screen.
  - Status (90px), Type (100px) and Allocated to (121px) have room to spare.
  - Narrow those instead: Type 84, Status 76, Allocated to with `minWidth = 90`. Then put Requirement back to about 220px.
- **N3 Each drop re-routes every link.**
  - `element:pointerup` → `separateChannels()` clears and re-routes all `mbCached` links. That takes about 230 ms at 200 links, which shows as a short hitch on release.
  - Fine at the sample's size. If larger models stutter, re-route only links whose bounding box meets the moved block's old or new box, plus their channel mates.
- **N4 Two boundary stubs share one run.** On Structure, `thrust` (K-0011) and `air` (K-0012) leave Propulsion and meet the same boundary point (650, 908), so the last 40px looks like one line. Give stubs to the same boundary point a small x offset.
- **N5 The `mb:overlay` listener is never removed.** — **Fixed with N1.**
  - Each widget instance adds a listener on `document`. The `el.isConnected` guard keeps it harmless, but listeners pile up as the diagram outputs re-render.
  - Remove it when the element goes, or use one shared listener that walks `mbdiagramDebug`.
- **N6 The splitter's End key always goes to 680px.**
  - At 1024 wide, End on both side columns would leave the canvas very narrow.
  - Cap the maximum at what keeps the main column at least ~400px, and set `aria-valuemax` to match.
- **N7 The mod_ai tests print an unhandled promise error.** It reads "Can't access reactive `ExtendedTask$private$status`; its module session has been destroyed" and appears twice. The tests still pass, but the noise hides real failures. Wait for or cancel the task before the test session ends.

## Suggested order

R1 → R2 → R3, then P2 items 1–3 (behaviour and accessibility), then 4–9. After R3, re-run handoff §8 items 9, 10 and 16. Each widget change needs a bump of the `mbdiagram` version.
