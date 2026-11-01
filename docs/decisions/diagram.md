# Diagram library: JointJS with elkjs

**Decision:** use `@joint/core` 4.1.3 (MPL-2.0) and `elkjs` 0.11.0 (EPL-2.0), vendored into
`widgets/mbdiagram`. The Cytoscape.js fallback isn't needed.

Measured on 2026-11-01 with `spike/diagram_spike.html`: a synthetic model of 150 components,
600 ports and 295 links, in the app's browser pane on the owner's Mac. The pane was hidden during
the timed run, so treat frame times as indicative; the final numbers come from the exit gate run.

| Check | Result | Target |
|---|---|---|
| ELK layered layout with port sides | 176 ms | |
| Render, to paint | 94 ms | under 1 s |
| Drag one block, 60 frames | median 17 ms, p95 17 ms | smooth |
| Drill in (12 blocks) / back out (150) | 22 ms / 83 ms | |
| Selection and double-click to the app | work with real pointer input | |
| Drag to move a block | works with real pointer input | |
| Port-to-port connect | works, with a drop handler that can refuse it | |

Lessons carried into the widget:

- Links must render below blocks. In the spike a link drawn over a port took the pointer, so a
  real drag from that port did nothing.
- Validate on the server, on drop. The client only requires that a link ends on a port.
- `elk.portConstraints = FIXED_SIDE` honours each port's side, which keeps the layout close to
  how an engineer places ports.
- Don't use `sorting: APPROX`. In the real widget it took about 600 ms of an 800 ms render at 150
  components. Inserting links before blocks gives the same layering with `sorting: NONE`.
- Don't repaint every cell's selection state on each build; each `attr` call redraws its links.
