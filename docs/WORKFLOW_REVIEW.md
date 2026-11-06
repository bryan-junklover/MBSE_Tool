# ModelBench workflow review

**Date:** 2026-11-06 · **Reviewed:** `ux-review-fixes` @ efef5a0 · **Status:** the doc claims, C2, A1–A7, C1 and D2 are fixed; the rest is open.

**Method**

- Four tracers each followed one area's workflows end to end, through the screens and the core:
  requirements, structure, behaviour and failures, and governance.
- They ran each claim against the sample project or a fresh one, and marked anything they
  couldn't confirm.
- I re-ran the most serious findings myself, and walked the how-to's path through a fresh
  project.
- No code was changed.

**What this review looks for:** places where a user following a normal process gets stuck, loses
work, or sees the model disagree with itself. It doesn't cover visual polish.

## Verdict

Creating things works well. Everything after creation is weaker: correcting, removing, renaming
and taking things back. Three patterns account for most of the findings.

1. **Deleting fails, or loses things silently.** Many references between tables have no rule for
   what happens when the thing they point at goes. So deleting an ordinary component, an
   interface or a requirement usually fails with a raw "FOREIGN KEY constraint failed". In the
   sample, components C-000 to C-007 and C-009 can't be deleted. Elsewhere a delete removes things
   that Undo can't bring back.
2. **Many things can be added but never changed or removed.** Requirements can't be deprecated or
   deleted. State machines, scenarios, assertions, test cases, events, ports, property
   definitions and parent links can't be edited or removed afterwards. A mistake becomes
   permanent, and some mistakes become permanent baseline blockers.
3. **The model's own checks can be fooled.** Evidence goes stale when nothing has changed, for
   example after an undo. A budget can pass while it's missing values. Completeness reports
   nothing missing for a state machine nobody has scoped. Renaming a field breaks the things
   that name it.

The how-to's path works from end to end on a fresh project. But the documents overstate two
things: Undo, and Completeness (see D1 and C3).

| | P1 | P2 | P3 |
|---|---|---|---|
| Count | 9 | 28 | 22 |

**P1** blocks or corrupts a normal workflow. **P2** is a dead end or a confusing result, with a
workaround. **P3** is minor.

## A. Deleting and references

**A1 (P1) Components that anything refers to can't be deleted.** *Confirmed.* — **Fixed.** A delete now takes the component's parts, lifelines, budgets and analysis limits with it. It clears actor and performer references and glossary aliases. The confirm says what goes and what's only unlinked, and Undo restores all of it. Every sample component now deletes. A delete that would still break a constraint is refused in plain words.
- **Why:** these references have no ON DELETE rule, and `direct_dependents` doesn't handle them:
  - `requirement.actor_element_id` (schema.sql:261), which the gate fills in automatically;
  - `seq_lifeline.element_id` (:399);
  - `act_node.performer_element_id` (:436);
  - `requirement_term` (a CHECK needs `element_id` when the term resolves to the model, :289);
  - `element.parent_id` (RESTRICT).
- **What the user sees:** the confirm dialog promises "It goes from the model… Undo brings it
  back", then the toast says "FOREIGN KEY constraint failed".
- **Fix:** before deleting, clear actor and term references and remove lifelines with their steps.
  Null the performer. Ask what to do with children. Refuse in plain words, naming anything that
  still holds the component.

**A2 (P1) Rollback fails when kept items point at something added after the baseline.** — **Fixed.** Rollback now puts kept items back first, then deletes the additions.
- **Example:** add a new parent, move an existing component under it, then roll back. Rollback
  deletes the additions before it restores the rest (baseline.R:158-163), so the delete hits the
  RESTRICT on `parent_id`, and the whole rollback is refused.
- **Fix:** restore modified and removed entities first, then delete the additions.

**A3 (P2) Interfaces in use can't be deleted.** — **Fixed.** Ports, steps and events stay, with their interface and field cleared, and Undo puts them back.
- **Why:** `port.interface_id`, `sm_event.interface_id` and `seq_step.interface_id` have no ON
  DELETE rule.
- **What the user sees:** the dialog says the ports "will have no interface", then the delete
  fails. Events can't have their interface cleared by hand at all (see B4), so IF-002 to IF-004 in
  the sample can never go.
- **Fix:** null those references in the delete's change set, and say so in the confirm.

**A4 (P2) Requirements and test cases with dependants can't be deleted.** — **Fixed in core.**
- Children move up to the deleted requirement's parent.
- Budgets, analysis limits and assertions stop naming it.
- A requirement or test case with recorded results is refused in plain words, because the results are evidence.
- The app still has no requirement delete; that's B1.
- **Why:** budgets, analysis limits, child requirements and verification records point at them
  without an ON DELETE rule (schema.sql:125, 252, 366). An assertion's `requirement_id` blocks the
  delete as well.
- **Fix:** as A1. Refuse naming the holders, or re-point them.

**A5 (P2) Deleting a component silently deletes its budgets and glossary aliases, and Undo
doesn't restore them.** — **Fixed.** Budgets are now revisioned dependants, so Undo restores them. Aliases are kept, as plain glossary terms.
- **Why:** they're removed by an ON DELETE CASCADE in the database, which writes no revision.
- **Fix:** make budgets revisioned dependants of their element, and warn about aliases.

**A6 (P3) Failure waivers outlive their subject.** — **Fixed.** They go with their subject. A deleted port's waiver stays under "Set aside"
as a raw id, and it's carried into baselines.
- **Fix:** delete a waiver with its subject.

**A7 (P3) Deleting a state breaks assertions and removes failure modes, without saying so.** — **Fixed.** A state an assertion starts or ends in is refused, naming the assertion. The confirm lists what goes.
- **Fix:** name both in the confirm, and refuse while an assertion starts or ends in that state.

## B. Things that can be added but not changed or removed

**B1 (P1) A requirement can't be deprecated or deleted.**
- **Why:** `deprecated` exists in the schema, and every query filters it out, but nothing sets it.
  Nothing in the app deletes a requirement.
- **Consequence:** a bad import, a duplicate or a blank draft (see G3) stays in the project for
  good.
- **Fix:** add **Deprecate** (with a reason) and **Delete draft** to the Specification inspector.

**B2 (P1) A scenario can't be renamed or deleted, and a wrong AI sketch can only be cleared by
endorsing it.**
- **Why:** an unreviewed scenario blocks the baseline, and the only action on it is "Review, with
  my note".
- **Fix:** add rename, edit description and delete, plus **Discard sketch** on an unreviewed one.

**B3 (P1) A lifeline can't be removed.** Together with A1, this means a component used in any
scenario can never be deleted.
- **Fix:** add **Remove lifeline**, which takes its steps.

**B4 (P2) A state machine's definition can only grow.**
- **Machines:** no rename, change of owner or delete.
- **Events and variables:** no rename, delete or type change, and a variable's unit can't be set.
- **Event bindings:** an event can't be tied to an interface field, although the schema, Where
  used and the sample all use that.
- **Ignores:** they can't be listed or removed. An ignore stays after the pair gets a transition.
- **Fix:** turn Events and variables into an editable table. Add rename and delete for the
  machine, and a list of ignores.

**B5 (P2) Assertions can't be edited or deleted.**
- Their `requirement_id` duplicates the `verified_by` link, and it blocks deleting the
  requirement even after that link is removed.
- **Fix:** add edit and delete, and take the requirement from the link.

**B6 (P2) Test cases can't be renamed, unlinked or deleted.**
- **Fix:** add these to the Verification panel. A delete either keeps or clears its records.

**B7 (P2) A port can't be edited after it's created.**
- **Consequence:** a port added with no interface stays a gate finding until it's deleted. That
  also loses its connectors and failure modes.
- Changing a connected port's interface in the core isn't re-checked against its connectors.
- **Fix:** add name, direction and interface controls, and re-run `check_connector` on change.

**B8 (P2) A component can't be moved to another parent or ungrouped.** A grouping done in error,
once it's past Undo, can only be fixed by deleting and recreating. That's often blocked by A1, and
when it works it loses the ids, links and failure modes.
- **Fix:** add **Move to…**, which the cycle check already guards, and **Ungroup**.

**B9 (P2) Property definitions can't be edited or deleted, and a property can't be removed from a
component.**
- **Consequence:** a property on the wrong component becomes a permanent baseline blocker, unless
  someone invents a value for it. A property created without a roll-up can never have a budget.
- **Fix:** add **Remove** beside each value. Allow edits to a definition's label and roll-up, and a
  unit change within the same quantity. Allow delete when it's unused.

**B10 (P2) `derives_from` and `refines` links can't be created.** The exports have columns for
them, and those are always empty.
- **Fix:** add pickers for both in the Specification inspector.

**B11 (P2) Functional interfaces don't appear on the Interfaces screen, and can't be created
there.** IF-011 to IF-017 in the sample can't be browsed or edited. The Add-port dialog offers both
layers to every element.
- **Fix:** add a Layer control and filter, and offer only the layer that matches the element.

**B12 (P2) Saved views and stereotypes are fixed.**
- A new project has no stereotypes, and nothing creates them.
- **Software only** filters on the `SW` stereotype, not on the Domain field the app now uses. So
  it's empty in a new project.
- The two property-based views only work when properties are named `mass` and `trl`.
- **Fix:** filter Software only on domain, and add a small editor for stereotypes and views, or
  hide those controls when there are none.

**B13 (P3) Projects can only be created and switched.** There's no rename, copy or delete, and the
project name can't be edited after it's created.

**B14 (P3) A requirement's level isn't in the inspector, and parent and child levels aren't
checked.** A stakeholder requirement can sit under a subsystem one.

**B15 (P3) Analyses, analysis limits and connector quantities can't be added from the app.**

## C. Checks and verification that can be fooled

**C1 (P1) Evidence goes stale when nothing has changed.** *Confirmed.* — **Fixed.**
- Each entry in a record's context now carries a hash of the content that bears on the evidence. For a requirement that's its text and verification method; for others, everything except bookkeeping fields.
- Evidence stays current through an undo, a rationale edit, re-committing the same text, and a rollback to the baseline it was recorded at.
- Records made before this are judged by the content of the revisions they name, so existing evidence benefits too.
- **Why:** the evidence context records each entity's revision number (db_query.R:65), and every
  write bumps it. So all of these make a passing test stale:
  - editing only a requirement's rationale or title;
  - re-committing identical text;
  - an edit followed by Undo ("Mass was 1.36 kg, now 1.36 kg");
  - a rollback to the very baseline the evidence was recorded against.
- **Consequence:** stale evidence blocks the next baseline, so this forces needless re-testing.
  The BL-03 Systems document shows TC-007 as stale, although it was current at BL-03.
- **Fix:** key the context on a hash of the entity's content, not its revision. Skip writes that
  change nothing.

**C2 (P1) A budget on a Largest or Smallest property with no values yet breaks three screens.** — **Fixed.** It reads "no values yet / 5 A" and passes. The Stakeholder meter says "Not known yet".
*Confirmed.* (This came in with the Units & properties work.)
- **Why:** `rollup()` returns NA, and `budget_status` then fails. That breaks the Structure
  inspector, Health and the Budgets table. The Remove button is inside the failing table.
- **Fix:** treat NA as "no values yet" in `budget_status`.

**C3 (P2) Completeness passes for a state machine nobody has scoped.** *Confirmed.*
- **Why:** `v_sm_unhandled` counts only pairs in `sm_event_scope`. New machines and new events get
  no scope, so the tab says "Every in-scope pair is handled". The how-to (§9 step 5) says it lists
  each unhandled event.
- **Fix:** put a new event in scope for every non-final state by default, or report "N events
  have no scope".

**C4 (P2) Budgets that leave out placeholders read as passing.** With the only Mass value a
placeholder, a budget reads "0 / 2 kg" and passes. The "excludes N placeholder" note is computed
but only the documents show it.
- **Fix:** show the note, and mark the budget incomplete rather than passing.

**C5 (P2) "Handled" can rest on links that handle nothing.**
- Rule 2 accepts any `handled_by` link, including one to a deprecated requirement or a transition
  in an unrelated machine.
- Rule 6 accepts any `verified_by` link, even to a failing assertion or a test never run.
- **Fix:** ignore deprecated handlers. Count a requirement as verified only when its latest result
  passes, or at least exists.

**C6 (P2) A stale-evidence blocker can't be cleared once its test case is unlinked.** The blocker
counts records with no link filter, and Specification lists only linked test cases.
- **Fix:** count only test cases still linked, or offer **Retire evidence**.

**C7 (P2) Assertion results never reach the requirement.** Specification shows "No test cases yet"
for SYS-012, which assertion A-001 verifies. The user guide says a requirement's checks run live.
- **Fix:** list linked assertions in the Verification panel, with their live and recorded results.

**C8 (P2) A baseline can be taken with failing checks, and nothing says so.** Only placeholders,
unreviewed drafts and stale evidence block. None of these are mentioned:
- failing budgets;
- failing or never-run assertions;
- open failure responses;
- unhandled pairs;
- unallocated requirements.

The how-to says to "decide they're acceptable", but nothing records that decision.
- **Fix:** list them as warnings when creating a baseline, and store the acknowledged list with
  it.

**C9 (P3) The gate score and resolved terms are stored when saved, and drift as the model
changes.** The Specification score and "Lowest score first" can be out of date; the inspector's
findings are live.
- **Fix:** re-gate saved requirements on model changes, or compute the score for display.

**C10 (P3) An assertion's `set` values aren't converted to the variable's type.** `{"mode": 1}`
fails `mode == 1`. The Fire buttons reuse those values.

**C11 (P3) No cross-check between failure links and the state machine.** Nothing flags an event
that detects a failure but that no state handles, or a handling transition on a different event.

## D. Undo, projects and AI drafts

**D1 (P1) Undo is one stack for the whole session, its button doesn't say what it undoes, and much
can't be undone.** *Doc claim wrong.*
- **Where it lives:** the Undo button is only on Model and the Behavior diagrams. So Undo on
  Structure can silently revert a Trace link or a budget added in Project.
- **Not undoable:**
  - commits and saved drafts;
  - Specification inspector edits, test cases, results and imports;
  - dismissals, the glossary, units, prefixes and scope.
- The user guide says "every model edit can be undone… including edits made in inspectors".
- **Fix:** one global **Undo ‹what›** control; put Write and Specification edits on the stack; and
  correct the guide.

**D2 (P1) Switching projects can carry the open draft into the other project.** — **Fixed.**
- Write clears its draft and the requirement it was editing when the project changes.
- Baselines clears its AI ratings.
- A switch also resets the failure-mode selection, behaviour focus, return bar and any pending "open in Write".
*From the code; not tried in a browser.*
- **Why:** `switch_project` resets selections and Undo only. Write keeps `editing()` and the draft
  text, so committing after a switch writes that id, for example SYS-002, into the other project.
- **Other state that leaks:** Baselines' AI narratives, which are keyed by change-set id and so
  collide between projects, and the failure-mode selection, behaviour focus and return bar.
- **Fix:** reset per-project state when the connection changes.

**D3 (P2) Undoing an AI Keep leaves the proposal marked kept, pointing at nothing.** It can't be
kept again.
- **Fix:** reopen a proposal when undo or rollback removes what it created.

**D4 (P3) AI proposals whose target was deleted stay open for ever.**
- They can't be kept, because the write path refuses them.
- **Start from this in Write** marks a proposal kept before anything is saved.
- **Fix:** supersede proposals with a missing target. Mark one kept only when the draft is saved.

**D5 (P2) Rollback doesn't leave drafts alone, though its dialog says it does.** A draft committed
after the baseline is deleted outright: it can only be recovered from the safety baseline. Links
from drafts are baselined, so rollback removes them.
- **Fix:** put such requirements back to their draft revision, and leave draft-end links out of
  baselines.

**D6 (P2) Change cards are poor for deletions and the newer kinds.**
- A deleted component's card is titled by its first dependant ("LINK · L-0310 removed").
- Removed items show "0 elements impacted".
- Failure modes, transitions, waivers and the newer links have no impact edges.
- Their titles and change text are raw ids and field names.
- **Fix:** title a card by what the user acted on, and add title, text and impact cases for these
  kinds.

**D7 (P3) Some project state is outside baselines.** The glossary, units, prefixes, scope,
viewpoints, layout and saved views aren't captured, so a rollback doesn't restore a removed alias.
- **Fix:** say so in the guide, or snapshot the glossary and viewpoints into a baseline.

## E. References by name

**E1 (P2) Renaming an interface field breaks what names it.**
- Failure modes, sequence steps and events store the field's name, not its id.
- After a rename, any edit to that failure mode is refused ("IF-002 has no field gnss_valid") until
  its Field is changed.
- The Consistency panel's "Add field to interface" puts the old name back.
- **Fix:** carry a rename into those rows in the same change set, and validate `field_name` only
  when it's the field being changed.

**E2 (P2) `prop(Element.property)` in guards breaks when the component is renamed.** The simulator
then errors. The transition can't be saved until its guard is rewritten, and nothing flags it.
- **Fix:** rewrite expressions on rename, or list broken ones in Health and Completeness.

**E3 (P3) Connector labels copy the interface name, and keep the old one after a rename.**

## F. Requirements entry and import

**F1 (P2) Exporting to CSV and importing it back makes duplicates and drops the metadata.** Import
reads only the text column, at level "system". Re-importing the sample's export added 9 new
drafts. Because of B1, they can't be removed. There's no ReqIF import.
- **Fix:** map the known columns, and update by id when it matches an existing requirement.

**F2 (P2) Import triage can't accept a row holding `[value]`,** though a draft may hold one.
- **Fix:** add such a row as a draft.

**F3 (P2) A term added as an interface field never resolves, and adding it again gives a raw
UNIQUE error.** `model_terms` doesn't read interface fields.

**F4 (P2) An unsaved draft is silently replaced.** Choosing another outline item, **Rewrite it in
Write**, **+ Requirement** or a scope-gap draft all overwrite the text without asking.
*From the code.*

**F5 (P2) There's no way to find uncoached requirements.** Coach state isn't shown in
Specification. The only way to coach one is to rewrite and re-commit it, which makes its evidence
stale (C1). Committing without a coach downgrades an already-coached requirement.
- **Fix:** show coach state, and add **Ask the coach** that doesn't bump the revision.

**F6 (P3) Save draft accepts empty text.**

**F7 (P3) A term added as a property of a second element stops resolving everywhere.**

## G. Smaller dead ends and raw errors (P3)

- **G1:** outside equipment placed on Flows before there's a system block is drawn twice
  (`structure_view` adds top-level externals twice when the root is empty). *Confirmed.*
- **G2:** double-clicking outside equipment opens an empty "level" inside it, with no way back.
- **G3:** changing a step's **Kind** to or from a fragment gives raw CHECK errors. Deleting a
  fragment's opener leaves its closer behind.
- **G4:** with every component already a lifeline, **+ Lifeline** offers an empty list, and
  confirming gives a raw NOT NULL error.
- **G5:** duplicate port, interface or field names give raw UNIQUE errors.
- **G6:** AI-sketched steps can only be reviewed one at a time; reviewing the scenario doesn't
  review its steps.
- **G7:** with nothing selected, the inspector labels every budget "This level", whatever the
  level.
- **G8:** a budget on a ×4 part counts four times the part's value, and nothing says so.
- **G9:** a scenario can't have the other kind of diagram added later.

## Doc corrections needed now — **Fixed**

The guide now says what Undo covers and what it doesn't. The guide and how-to explain **Must handle**. The rollback dialog and guide say what happens to drafts committed since the baseline. D1, C3 and D5 themselves are still open.

- **USER_GUIDE §2:** "Every model edit can be undone with Undo" isn't true (D1).
- **HOW_TO §9 step 5:** Completeness lists nothing until each state's events are scoped (C3).
- **The rollback dialog:** "Draft requirements stay as they are" isn't true (D5).

## Checked and found sound

- **Writes:** every write goes through the write path with change sets. A refused delete or
  rollback rolls back completely.
- **Undo:** a component delete brings back its ports, links, state machine and failure modes,
  apart from A5.
- **Rollback** restores failure modes, waivers, hosting links and budgets. Compare then shows no
  changes.
- **Baselines** are refused on the server while blockers exist. Health and the Create dialog use
  the same blocker list.
- **Migrations** 002 to 004 change no columns, so baselines taken before them still restore.
- **Documents** from a baseline match it and leave the working copy untouched.
- **Commit** re-runs the gate in core. A committed requirement can't hold `[value]`, and the
  coach's review only stands for the exact text it saw.
- **References by id** survive renames. Only the name-based ones in E break.
- **Failure-mode rules:** links are validated in core; the AI can't set a response; the effect
  chain is protected against cycles.
- **Model rules:** connector checks, the hosting rules, the parent cycle check and grouping
  validation all hold.

## Suggested order

1. **Correct the three doc claims,** and fix C2, the budget crash, which is small.
2. **Make deleting safe (A1–A5, A2):** refuse in plain words, naming what holds an item, or clean
   up references in the same change set. This one change turns most raw errors into explanations.
3. **Fix evidence staleness (C1)** and **project-switch state (D2).** Both quietly cost the user
   work.
4. **Add the missing remove and edit actions,** most-hit first:
   - B1, deprecate or delete a requirement;
   - B2 and B3, scenarios and lifelines;
   - B4 and B5, state machines and assertions;
   - B7, ports;
   - B8, Move to and Ungroup;
   - B9, properties.
5. **Tighten the checks:** C3, C4, C5, C7 and C8.
6. **Then the rest:** undo coverage (D1), rename propagation (E), import (F), and G.
