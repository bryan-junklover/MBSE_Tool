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

1. **The narrow inspector overlay hides the selection.** At 900×800, selecting Ground Control Software opened the overlay right on top of it. When the overlay opens, scroll the canvas so the selected element sits in the part still visible, or overlay from the side the selection isn't on.
2. **The column splitter can't be used from the keyboard.** `.mb-split` has `role="separator"` but no `tabindex`, `aria-valuenow`, `aria-valuemin` or `aria-valuemax`, and no arrow-key handling. The handoff (§7) says everything works from the keyboard. Its hide button (‹ ›) is 19×26px, under the 32px target, and invisible until hover. Show it at 40% opacity at rest.
3. **Specification table overflows by 10px when the inspector is open.** The table is 779px in a 769px box, so the Score column, which the table is sorted by, is clipped and needs a horizontal scroll. Let the table recalculate its column widths when its box changes size (call `Reactable` resize on the column's `resize` event), or give Score a fixed narrow width.
4. **Draft outline titles truncate too early at 240px.** Examples: "Map an area in one fli…", "Battery temp…", "Ground sample dist…". The nested ID gutter takes about 40% of the row. Either indent the title rather than the ID, or default this column to 280px, or allow two lines with `-webkit-line-clamp: 2`.
5. **The gate's blocking message appears three times:** in the gate header, in the ledger's "blocks" row and in the Commit bar. Keep the ledger row and the Commit bar, and drop the `.mb-hard` line, since the header verdict already says "blocked by 1 rule".
6. **The top bar wraps to two rows below about 1000px:** 103px tall at 900 wide, where "Ask the model" drops to its own line. Truncate `.mb-context` with an ellipsis (`min-width: 0; overflow: hidden; text-overflow: ellipsis`) before letting the bar wrap.
7. **Parallel connectors share one channel.** On Structure, several PowerBus28V routes run on top of each other, and two "PowerBus28V" labels stack. The manhattan router gives every link the same `padding: 16`. Offset links that leave the same side of the same block (padding 16 + 8n), or label only one link per interface in a bundle.
8. **Baselines "Compare" select truncates** to "BL-03 · Baselin". Let it size to its content (`width: auto`), or show only the id with the name in its `title`.
9. **4.6 is still open:** there's no shinytest2 check for horizontal scroll at 900px. My manual sweep found none on all 15 screens, but nothing guards it in CI.

## Suggested order

R1 → R2 → R3, then P2 items 1–3 (behaviour and accessibility), then 4–9. After R3, re-run handoff §8 items 9, 10 and 16. Each widget change needs a bump of the `mbdiagram` version.
