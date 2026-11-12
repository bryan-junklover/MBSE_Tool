# How-to: set up and analyse a project

This walk-through takes a new project from an empty file to a baseline and a set of documents
for the hardware and software teams. It uses a small example, a **solar weather station**: a
mast with sensors and a logger, powered by a solar panel and battery, that sends its readings by
radio to a base station. Use your own system instead if you like; the steps are the same.

Allow an hour the first time. Most steps can be undone, so experiment freely.

## 1. Create the project

1. Open **Project**. Under **General**, type a name in **New project name**, such as *Weather
   station*, and click **Create project**. The app opens it.
2. Write the **Scope** in a few sentences. For example: *A solar-powered weather station that
   measures air temperature, humidity and wind, logs the readings, and sends them by radio to a
   base station PC.* It saves as you type. The AI reads it for context, and documents quote it.
3. Leave the **ID prefixes** as they are unless your organisation uses its own (for example
   `WS-SYS`). They apply only to new items.
4. **Add the properties you'll track.** Open **Project › Units & properties**. Under
   **Properties**, add *Mass* (number, kg, roll-up *Sum*) and *Power* (number, W, roll-up *Sum*).
   Roll-ups add values up the tree, times how many of each part there are.
5. **Check the units.** Every project has the standard units (kg, W, V, °C, Hz and so on). If you
   need one that isn't listed, add it under **Units**. Give its symbol, what it measures, its SI
   unit, and how many SI units one of it is. For example, knots: *kn*, *speed*, *m/s*, *0.5144*.

## 2. Decide how the AI helps (optional)

Everything in this guide works without AI. Without a coach, requirements commit on the gate
alone and are marked *uncoached*, which Health lists.

To use the coach and the other AI features:

1. Open **Project › AI**. Under **AI providers**, paste a key for Anthropic or OpenAI. The key
   goes to your operating system's keyring, not the project. Alternatively, point LM Studio at a
   model running on your machine. Click **Test the connection**.
2. Under **Task routing**, check which model each task uses. The *Requirement light check* runs
   on every edit; it's quickest with a small local model.
3. Set a **Daily cloud spend cap**.
4. If the project is sensitive, open **Data protection**. Either turn on **Local only**, or list
   names to **Redact before cloud calls**.

## 3. Write the requirements

Open **Write › Draft**.

1. **Start with what the stakeholders need.** Set **Level** to *stakeholder* and write, for
   example:
   *The weather station shall report air temperature to the base station every 10 minutes.*
   The gate scores the sentence as you type, and lists anything that blocks it: no "shall", no
   measurable bound, or a vague word.
2. **Look at Terms in this draft.** Words the model doesn't know yet, such as *base station*,
   can be added as components straight from here, so the structure starts from your words.
3. If a coach is set up, wait for its review and read its questions. Dismiss one you disagree
   with, giving a reason.
4. Choose **Verify by** (*test*, *analysis*, *inspection*, *demonstration* or *simulation*) and
   give a **Short title**.
5. Click **Commit to specification**.
6. **Add system requirements beneath it.** Set **Level** to *system* and **Parent** to the
   stakeholder requirement. For example:
   - *The logger shall record air temperature every 60 s.*
   - *The radio modem shall transmit stored readings within 120 s of a link being restored.*
   - *The power system shall keep the logger running for 72 h without sun.*
7. **Leave undecided numbers as `[value]`.** Write `[value]` for a number nobody has decided
   yet, and **Save draft**. It can't be committed until the number is chosen, so nothing
   half-decided slips into the specification.

Open **Write › Specification** to see them all, sorted by score with **Lowest score first**.

## 4. Build the structure

Open **Model › Structure**.

1. **Add the system block first.** Choose the **Component** tool and click the canvas. Name the
   new block *Weather station* in the inspector. The first component in a project is the whole
   system: the diagram opens inside it, and everything you add next is one of its parts.
2. **Add the parts.** Still with **Component**, click the canvas once for each: *Solar panel*,
   *Battery*, *Logger board*, *Sensor head*, *Radio modem*. Name each in the inspector.
3. **Set each one's domain:** *Hardware* for all of these. A part that's both, such as a smart
   sensor with firmware, is *Hardware and software*.
4. **Add the software.** Add *Logger firmware* and set its domain to *Software*. In its inspector,
   set **Runs on** to *Logger board*. Software with nothing to run on shows as a gap.
5. **Group parts if it helps.** Select *Solar panel* and *Battery*, then click **Group into
   subsystem**, and name the new block *Power system*. Double-click it to open the level inside;
   the breadcrumb takes you back.
6. **Give parts their properties.** Select *Battery*, choose *Mass* under **Add a property**, and
   type its value. Do the same for the others as you learn them. A property left blank is a
   placeholder, shown as `[value]`, and it blocks a baseline until it's set.
7. **Set a budget.** Back in **Project › Units & properties**, under **Budgets**, add *Mass over
   Weather station, at most 8 kg*. With nothing selected, the Structure inspector shows the
   roll-up against the limit. Health shows it too, and flags it when it's over.

## 5. Define the interfaces

Open **Model › Interfaces** and click **+ Interface** for each thing that moves between parts:

| Interface | Flow | Fields |
|---|---|---|
| *Power12V* | energy | none |
| *SensorReadings* | data | *air_temp* (float32), *humidity* (float32) |
| *Uplink* | data | *readings*, *seq* |

For each field, set its type and unit, and its range if you know it. For example, *air_temp* in
°C, from −40 to 60. Leave a range **to be decided** if not.

## 6. Connect the parts

Back on **Structure**:

1. **Add ports.** Choose the **Port** tool and click a block. Give the port a name, a direction
   (*in*, *out* or *inout*) and an interface. For example, *Battery* gets `pwr_out` (out,
   Power12V), and *Logger board* gets `pwr_in` (in, Power12V).
2. **Connect them.** Choose **Connect**, or stay in **Select**, and drag from one port to another.
   ModelBench refuses a connection between different interfaces, or two outputs, and says why.
3. **Finish the wiring inside the station:** sensor head to logger board (*SensorReadings*), and
   logger board to radio modem (*Uplink*).
4. **Check for gaps.** Any port left unconnected shows as a dashed amber gap. With nothing
   selected, the inspector lists every gap at this level.
5. **Tidy up.** **Auto-layout** rearranges the diagram, and **Fit** brings it all into view.

**Add the base station.** The base station PC sits outside the system, so it isn't a part. Open
**Model › Flows**, where outside equipment is shown.

1. Choose **Outside equipment** and click the canvas, then name it *Base station PC*.
2. Give it a port, then connect the radio modem's uplink port to it.

Flows shows the same structure coloured by what moves: power, data and matter.

## 7. Add functions and allocate them

Open **Model › Functional**.

1. **Add the functions.** Choose **Function** and add *Measure weather*, *Store readings* and
   *Send readings*. Connect them as the data flows between them.
2. **Allocate each one.** In its inspector, use **Allocate to** to pick the component that
   performs it: *Measure weather* → *Sensor head*, *Store readings* → *Logger firmware*. **Ask AI
   about this** argues for candidates if you're unsure.
3. **Check the Consistency panel.** It lists functional flows with no physical connector carrying
   them. Use **Carried by** to name the connector.

## 8. Trace requirements to components

Open **Write › Traceability**.

1. Click the cell where each requirement meets the component that satisfies it. For example, the
   72 h requirement meets *Battery*.
2. Rows with nothing allocated are marked as gaps. **Ask AI about this** ranks candidates, and
   choosing one asks for your rationale.

## 9. Describe the behaviour

Open **Behavior › Scenarios**.

1. Click **New scenario…**, name it *Hourly upload*, and choose *Sequence*.
2. Add lifelines with **+ Lifeline**: *Logger firmware*, *Radio modem*, *Base station*.
3. Add messages with **+ Message**, setting the interface and field in the step inspector. For
   example: logger to modem, *Uplink.readings*.
4. A step naming a field the interface doesn't have turns amber. Fix the step or the interface.

**Sketch with AI…** can draft a scenario from a description. Its steps arrive as unreviewed
drafts for you to check.

Then describe the logger's modes. Open **Behavior › State machines**.

1. **Create it.** Click **New state machine…**. Name it *Logger modes*, choose *Logger firmware*
   under **Behaviour of**, and set **Starts in** to *Sleeping*.
2. **Add the other states** with **Add state**: *Measuring* and *Sending*.
3. **Add events** with **Add event…**: *timer*, *done*, *sent* and *link_lost*. Event names are
   snake_case.
4. **Add transitions.** Drag between a state's handles, such as *Sleeping* to *Measuring* on
   *timer*. Give a transition a guard or an effect in the inspector if it needs one.
5. **Say what each state must handle.** Select a state and, under **Must handle**, tick the events
   it has to respond to. For example, *timer* in *Sleeping*, and *link_lost* in *Sending*.
6. **Check completeness.** The **Completeness** tab lists each ticked event that has no transition
   from its state yet. Add a transition, or **Ignore, with my note** when ignoring it is safe. An
   event you haven't ticked anywhere isn't checked, so an empty list only means the ticked ones
   are covered.
7. **Try it.** In the simulator under the canvas, fire events and watch the state change. Open
   the trace to see each step.
8. **Make it a check.** **Add assertion…** records a start state, events and an expected state,
   tied to a requirement. For example: from *Sending*, *link_lost* ends in *Sleeping*. **Run and
   record** runs every assertion.

## 10. Verify

Open **Write › Specification** and select a requirement.

1. Add a test case. For example, *72 h dark run*, method *test*.
2. When it's run, **Record result** with pass or fail, an evidence reference and a note.
   Evidence recorded against an older model is marked stale when the model changes, so you know
   what to re-test.

## 11. Work out how it fails

Open **Behavior › Failure modes**. The **Checks** column already lists cases nobody has recorded,
worked out from your structure. For the weather station it will say things like:

- *Logger board is fed Power12V by Battery, but nothing records what happens if that's lost.*
- *Logger firmware receives SensorReadings from Sensor head, but nothing records what happens if
  it's lost or stale.*
- *SensorReadings.air_temp has a range, but nothing records what happens when a value falls
  outside it.*

For each one:

1. **Add the failure mode.** Click **Add lost failure** (or the matching button) to start one from
   the check. Or use **+ Failure mode for…** to add one for any part, interface, port or state.
2. **Describe it.** Give the cause in words, the effect nearby, and the effect on the system. For
   example: *Battery connector corrodes* → *Logger resets* → *Readings stop until someone
   visits*.
3. **Decide the response:**
   - **Handled:** link what handles it under **Handled by** (a requirement, transition or
     component), and say how in the note. Link what notices it under **Detected by**.
   - **Accepted:** say why it's tolerable.
   - **Unhandled:** a known gap, left visible.
4. **Read the Effect chain.** For the power loss, it shows the logger board stopping, then the
   logger firmware that runs on it, then the radio modem losing its uplink. A ⚠ marks each loss
   nobody has decided about. Record a response where it lands and the chain stops there (✓).
5. **Set aside what doesn't apply.** Use **Doesn't apply…** with a reason, such as *the sensor
   head has its own fuse*.
6. **Ask the AI what nobody thought of (optional).** **Suggest with AI** proposes failure modes
   that aren't recorded. Keep the ones that matter with a note, review them with a second note,
   then decide their response yourself.

## 12. Check health and take a baseline

1. **Open Analyze › Health.** It lists everything that blocks a baseline: placeholders such as
   `[value]` or a range still to be decided, and AI drafts not yet reviewed. Each links to where
   it's fixed. The cards below it count the other gaps: unallocated requirements, unconnected
   ports, unhosted software, open failure responses and uncovered cases.
2. **Fix the blockers**, or decide they're acceptable for now. Open AI items wait in
   **Analyze › Review queue**.
3. **Take the baseline.** Open **Analyze › Baselines & impact**. Once nothing blocks it, click
   **Create baseline**, then give it a name and a note.

## 13. Send each team its document

Open **Analyze › Documents**.

1. **Choose Hardware.** The preview shows what hardware engineers get:
   - the physical breakdown, with the logger firmware under the logger board;
   - the interfaces, with every connection;
   - properties;
   - physical failures and their effect chains;
   - the requirements on those parts;
   - the open items in their part of the model.
2. **Choose Software** for the software team: where the firmware runs, the signals with their
   ranges, state tables, software failures, and their open items.
3. **Choose Systems** for a combined report.
4. **Choose the source.** **From** builds a document from your new baseline instead of the
   working copy.
5. **Export.** Click **Export PDF** or **Export HTML**, and send it.

The documents are read-only views of the model. Questions come back to you, and you change the
model; the next export picks the change up.

## 14. Make a change and see what it reaches

1. **Change something.** For example, swap the battery for a smaller one, or change the logging
   interval.
2. **See what changed.** Open **Analyze › Baselines & impact**. It compares your baseline with the
   working copy and groups the changes. Select one to see what it reaches: requirements, evidence
   that's now stale, and scenarios. **Behavior › Behavioral impact** shows the affected scenario
   steps before and after.
3. **Undo if needed.** **Roll back…** returns to the baseline, saving a safety baseline first so
   nothing is lost.
4. **Re-baseline.** When the change is settled, take the next baseline and export fresh documents.

## Where to go next

- The **User guide** tab describes every screen in detail.
- Open the Survey UAV sample for a fuller model to explore, with a richer set of failure modes
  and effect chains.
