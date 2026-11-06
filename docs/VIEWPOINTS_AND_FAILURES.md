# Stakeholder viewpoints and failure modes

**Status:** agreed, 2026-11-06; steps 1–4 done · **Owner:** the systems engineer

## Why

The model is the data, and diagrams are only one view of it. Today every screen shows one
combined picture. Two stakeholder groups need different things from it:

- **Hardware engineers** ask what each part weighs and draws, what plugs into what, and whether it
  fits. The software parts of the model confuse them.
- **Software engineers** ask what can go wrong, what state the system is in, and what happens with
  bad or missing input. The model has nowhere to record error conditions.

Safety, test and operations will follow with needs of their own.

## Decisions so far

| Question | Decision |
|---|---|
| Who edits the model? | Only the systems engineer. Viewpoints are read-only. |
| How do stakeholders get their view? | As exported documents, tailored to a viewpoint or combined for reporting. There's no in-app lens for now. |
| Formats | HTML and PDF. Word isn't needed. |
| Live model or baseline? | The live model by default; a baseline can be chosen instead. |
| Physical failures in the Hardware document? | Yes: connector, power, thermal and other physical failures, as well as where software failures reach hardware. |
| Are viewpoints code or data? | Data: one row per viewpoint. New stakeholder groups need no new code. |
| How much failure analysis? | Effects and responses only. No severity, occurrence or detection scores (no RPN). |
| AI's role in failure analysis | A sense check that suggests failure modes nobody recorded. Its suggestions go to the review queue; it never marks anything handled. |

## What the model already holds

Most of the split is already in the data. The views just don't use it.

- `element.domain` is `hw`, `sw`, `hw_sw` or `other`.
- `interface.flow_type` is data, energy or matter, and `interface.layer` is physical or functional.
- `realized_by` links a functional connector to the physical connector that carries it.
- Interface fields have types, units and ranges; properties have units, roll-ups and budgets.
- `sm_event` can name the interface and field that raises it (`gnss_lost`). `sm_event_scope` and
  `sm_ignore` already require each event to be handled or knowingly ignored in each state.
- `view_def.filter_json` holds saved filters, but only for Structure diagrams.
- The ICD export (`icd_markdown`, `render_icd`) already renders Markdown through Quarto to HTML or PDF.

## 1. Which hardware each piece of software runs on

**New link type `hosted_on`:** a software element links to the hardware element it runs on.

- It lets the hardware documents fold software into its host ("Flight computer, running Navigation
  and Flight control").
- It lets failures cross the boundary: when a host fails, the software on it stops.
- **Check:** a `sw` component with no `hosted_on` link is a gap. `hw_sw` elements are exempt.
- The `link_type` CHECK constraint changes, so this needs a schema migration (SQLite rebuilds the
  table). Bump `schema_version`.

## 2. Viewpoints and their documents

**New table `viewpoint`:**

| Column | Meaning |
|---|---|
| `id` | `VP-hardware`, `VP-software`, `VP-systems`, then `VP-0001`… |
| `name`, `stakeholder`, `concerns` | What the document says it's for, and who reads it |
| `rules_json` | Which domains, flow types and interface layers to include, and which sections to render, in order |
| `is_builtin` | Built-in viewpoints are seeded on every open, like `view_def` |

A viewpoint's document is a list of **sections**. Each section is an R function that takes the
connection and the viewpoint's filters and returns Markdown, like `icd_markdown` does today.
Quarto renders the result to HTML or PDF.

| Section | Hardware | Software | Systems (all) |
|---|---|---|---|
| Summary: scope, model hash, baseline, counts | ✓ | ✓ | ✓ |
| Physical breakdown, with hosted software listed under each host | ✓ | | ✓ |
| Interface control: physical interfaces, connectors, pins, rates (today's ICD) | ✓ | | ✓ |
| Budgets and roll-ups: mass, power, margins | ✓ | | ✓ |
| Software components, their hosts and their functions | | ✓ | ✓ |
| Signal list: data interfaces, fields, types, units, ranges | | ✓ | ✓ |
| States and transitions, as a table per state machine | | ✓ | ✓ |
| Failure modes and responses (§3) | physical failures: hardware elements and energy, matter and physical-layer interfaces | software elements, data interfaces and states | ✓ |
| Effect chains (§4) | crossing into hardware | ✓ | ✓ |
| Requirements allocated to what's shown, with verification status | ✓ | ✓ | ✓ |
| Open items: placeholders, gaps and unhandled failures within this scope | ✓ | ✓ | ✓ |

- **Combined reports** use the Systems viewpoint, or any viewpoint whose sections span domains.
- **Live model by default:** a document is rendered from the working copy unless a baseline is
  chosen. The summary names which one it came from, and the model hash.
- **In the app:** a Documents screen lists the viewpoints. Each gets a preview, an Export button
  and a short "what this reader gets" line. Editing a viewpoint's sections in the app can wait;
  until then new viewpoints are seeded rows.
- **Retire `view_def`?** Not yet. Diagram filters and documents are different jobs. Revisit once
  viewpoints exist.

## 3. Failure modes

**New table `failure_mode`** (`FM-%04d`):

| Column | Meaning |
|---|---|
| `subject_kind`, `subject_id`, `field_name` | What fails: an element, an interface (optionally one field), a port or a state |
| `cause_kind` | `lost`, `stale`, `out_of_range`, `erroneous`, `degraded` or `failed`; for physical failures also `open_circuit`, `short_circuit`, `over_temperature` or `mechanical`; else `other` |
| `cause` | The cause in the engineer's words |
| `local_effect`, `system_effect` | What happens next to it, and to the system |
| `response_state` | `handled`, `accepted` (with a rationale, like `sm_ignore`), `unhandled` or `placeholder` |
| `origin`, `review_state`, `review_note` | As for every other entity: AI drafts arrive unreviewed and need a note to accept |

**New link types**, using the existing `link` table with `failure_mode` added as a link kind:

- `detected_by`: failure mode → `sm_event`, requirement or test case. Linking to an `sm_event`
  reuses the existing check that each event is handled or ignored in every state it applies to.
- `handled_by`: failure mode → `sm_transition`, requirement or element.

**Checks** (shown in the Systems view and in each document's Open items):

1. A failure mode whose response is `unhandled` or `placeholder`.
2. A failure mode marked `handled` with no `handled_by` link.
3. A data interface that a `sw` element receives from a `hw` element, with no `lost` or `stale`
   failure mode.
4. A power feed (an `energy` interface) that an element receives, with no `lost` failure mode.
   A hardware element that dissipates power (a set `power` property) with no `over_temperature`
   failure mode.
5. An interface field with a set range on a receiving port, with no `out_of_range` failure mode.
6. A `handled_by` requirement that has no verification.

Rules 3–5 can be noisy. Each can be set to "not applicable" with a reason, as `sm_ignore` does.

## 4. Effect chains

Effects are worked out from the model rather than typed in:

- A `failed` element breaks every interface it sends: whoever receives them sees `lost`.
- A `failed` hardware host fails every element `hosted_on` it.
- At each receiver, follow that receiver's own failure mode for the cause. The chain stops at a
  failure mode marked `handled` or `accepted`, and is flagged where none exists.

Example: PowerBus28V `failed` → Flight computer `failed` → Navigation and Flight control stop →
the Telemetry radio sees attitude data `lost` → handled by the `link_lost` transition to Return
to home.

Chains are computed when needed and never stored. They appear under each failure mode on the
Failure modes screen and in the Effect chains section of each document.

As built:

- **Parts:** a failed component also fails its own parts. So the flight controller failing
  reaches FC Hardware, and from there the software that runs on it.
- **What stops a chain:** losing power fails the receiver and the chain goes on. Losing data
  or matter with nothing recorded is flagged, and the chain stops there.
- **Outside the system:** where a chain reaches external equipment, it notes the loss and
  stops.
- **No cut to scope:** each document shows the chains for its failure modes in full, rather
  than cut to the viewpoint's scope. A power failure that stops software is exactly what the
  hardware reader needs to see.

## 5. AI sense check

- **New task route `failure_suggest`.** It is given the element types, interfaces, mission text
  and existing failure modes, and asks what's missing. Examples: "GNSS can be degraded as well as
  lost"; "nothing records an intermittent radio link".
- Suggestions arrive as unreviewed `failure_mode` rows through `ai_proposal`, with `response_state =
  'placeholder'`. The AI can draft a cause and effect, but never a response and never `handled`.
- **The checks in §3 find gaps in what was recorded; the AI looks for what nobody thought of.**
  Both show in the review queue, labelled so they can be told apart.

## Order of work

Each step is useful by itself.

1. **`hosted_on` links**, with the migration and the check. *Done in ac45f1e.*
2. **Viewpoint documents** for Hardware, Software and Systems, built from data that already exists. *Done: Analyze › Documents.*
   This pays off before failure modes exist.
3. *Done: Behavior › Failure modes, a Failures card on Health, and a section in each document.*
   **Failure modes:** table, links, inspector editing in the Systems view, checks, and the
   document section.
4. **Effect chains.** *Done: under each failure mode, and an Effect chains section in each document.*
5. **AI suggestions.**

## Out of scope for now

- Editing in a stakeholder viewpoint, or an in-app lens per viewpoint.
- FMEA scoring (severity, occurrence, detection, RPN).
- Fault trees and probabilistic analysis.
- Editing a viewpoint's sections from the app.

## Open questions

None at present. The three earlier questions are answered in *Decisions so far*: HTML and PDF
only, the live model by default, and physical failures listed in the Hardware document.
