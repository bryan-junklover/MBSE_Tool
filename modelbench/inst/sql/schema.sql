-- ModelBench project schema, version 1
-- One SQLite file per project. init_project_db() runs this file on every open;
-- every statement is idempotent (IF NOT EXISTS). Later versions add numbered
-- migrations; see SCHEMA.md §9.
--
-- Conventions (SCHEMA.md §2):
--   * Primary keys are stable, human-readable TEXT ids (SYS-012, C-004) allocated
--     from id_counter and never reused. Renaming never changes an id.
--   * Revisioned tables carry rev; every write goes through the core write path,
--     which bumps rev and writes a row to `revision` in the same transaction.
--   * "Tiered coaching" columns: origin / review_state / review_note. Anything the
--     AI originated starts 'unreviewed'; it becomes 'reviewed' only with a note.
--   * Placeholders are explicit states ('placeholder'), never magic values.
--   * Timestamps are ISO-8601 UTC text.

PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;

-- ---------------------------------------------------------------------------
-- 1. Project
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS project_meta (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
);
INSERT OR IGNORE INTO project_meta (key, value) VALUES ('schema_version', '1');
-- Known keys: schema_version, project_name, scope_text, coaching_policy
-- ('tiered' | 'strict' | 'assistive'), local_only ('0' | '1'),
-- spend_cap_usd, redact_terms (JSON array), coach_wait_ms.

CREATE TABLE IF NOT EXISTS id_counter (
  prefix   TEXT PRIMARY KEY,          -- 'STK','SYS','C','F','EXT','IF','FLD','P','K','PD','STY' (stereotype),'ST' (seq step),'SC','SM','S','E','T','VAR','LL','A','TC','VR','BL','PR','L','N','G','V','B','VW','CS' (change set)
  next_num INTEGER NOT NULL DEFAULT 1 CHECK (next_num >= 1)
);

CREATE TABLE IF NOT EXISTS unit (
  symbol    TEXT PRIMARY KEY,         -- 'kg', 'W', 'mAh', 'deg', '°C'
  quantity  TEXT NOT NULL,            -- 'mass', 'power', 'charge', 'angle'
  si_symbol TEXT NOT NULL,
  si_factor REAL NOT NULL,            -- value_in_si = value * si_factor (+ si_offset)
  si_offset REAL NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS glossary (
  term       TEXT PRIMARY KEY COLLATE NOCASE,
  definition TEXT,
  element_id TEXT REFERENCES element(id) ON DELETE CASCADE,  -- set: alias of a model element
  created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
);

-- ---------------------------------------------------------------------------
-- 2. Stereotypes and properties
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS stereotype (
  id          TEXT PRIMARY KEY,       -- STY-001
  rev INTEGER NOT NULL DEFAULT 1,
  name        TEXT NOT NULL UNIQUE,   -- 'HW', 'SW', 'HW+SW', 'Avionics'
  applies_to  TEXT NOT NULL CHECK (applies_to IN ('component','function','connector','interface','requirement')),
  description TEXT
);

CREATE TABLE IF NOT EXISTS property_def (
  id            TEXT PRIMARY KEY,     -- PD-001
  rev INTEGER NOT NULL DEFAULT 1,
  stereotype_id TEXT REFERENCES stereotype(id) ON DELETE CASCADE,  -- NULL = available on every owner
  name          TEXT NOT NULL,        -- snake_case: mass, power, battery_capacity
  label         TEXT NOT NULL,        -- 'Mass'
  value_type    TEXT NOT NULL CHECK (value_type IN ('number','text','enum','bool')),
  unit          TEXT REFERENCES unit(symbol),
  enum_values   TEXT,                 -- JSON array when value_type = 'enum'
  rollup        TEXT NOT NULL DEFAULT 'none' CHECK (rollup IN ('none','sum','max','min')),
  required      INTEGER NOT NULL DEFAULT 0 CHECK (required IN (0,1)),
  UNIQUE (stereotype_id, name)
);

CREATE TABLE IF NOT EXISTS property_value (
  id              TEXT PRIMARY KEY,   -- V-0001
  owner_kind      TEXT NOT NULL CHECK (owner_kind IN ('element','connector','interface','requirement')),
  owner_id        TEXT NOT NULL,      -- polymorphic; integrity enforced by core (SCHEMA.md §4)
  property_def_id TEXT NOT NULL REFERENCES property_def(id),
  value_state     TEXT NOT NULL DEFAULT 'placeholder' CHECK (value_state IN ('set','placeholder','computed')),
  num_value       REAL,
  text_value      TEXT,
  unit            TEXT REFERENCES unit(symbol),
  set_by          TEXT NOT NULL DEFAULT 'human' CHECK (set_by IN ('human','code')),  -- never the AI
  rev             INTEGER NOT NULL DEFAULT 1,
  updated_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  UNIQUE (owner_kind, owner_id, property_def_id),
  CHECK (value_state <> 'set' OR num_value IS NOT NULL OR text_value IS NOT NULL),
  CHECK (value_state <> 'placeholder' OR (num_value IS NULL AND text_value IS NULL)),
  CHECK (value_state <> 'computed' OR set_by = 'code')
);

CREATE TABLE IF NOT EXISTS budget (
  id               TEXT PRIMARY KEY,  -- B-001
  rev INTEGER NOT NULL DEFAULT 1,
  property_def_id  TEXT NOT NULL REFERENCES property_def(id),
  scope_element_id TEXT NOT NULL REFERENCES element(id) ON DELETE CASCADE,
  limit_value      REAL NOT NULL,
  unit             TEXT NOT NULL REFERENCES unit(symbol),
  comparison       TEXT NOT NULL DEFAULT 'max' CHECK (comparison IN ('max','min')),
  requirement_id   TEXT REFERENCES requirement(id),
  UNIQUE (property_def_id, scope_element_id)
);

-- Results of registered R analyses (e.g. endurance). Code only; never AI.
CREATE TABLE IF NOT EXISTS analysis_result (
  analysis    TEXT NOT NULL,          -- registry name, e.g. 'endurance'
  scope_id    TEXT NOT NULL,
  value       REAL,
  unit        TEXT REFERENCES unit(symbol),
  status      TEXT NOT NULL CHECK (status IN ('ok','missing_input','error')),
  inputs_hash TEXT NOT NULL,
  detail_json TEXT,
  computed_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  PRIMARY KEY (analysis, scope_id)
);

-- Limit an analysis result is judged against (budget needs a property; an analysis has none).
CREATE TABLE IF NOT EXISTS analysis_limit (
  analysis       TEXT NOT NULL,          -- registry name
  scope_id       TEXT NOT NULL,
  limit_value    REAL NOT NULL,
  unit           TEXT NOT NULL REFERENCES unit(symbol),
  comparison     TEXT NOT NULL CHECK (comparison IN ('max','min')),
  requirement_id TEXT REFERENCES requirement(id),
  rev            INTEGER NOT NULL DEFAULT 1,
  PRIMARY KEY (analysis, scope_id)
);

-- ---------------------------------------------------------------------------
-- 3. Structure: elements, interfaces, ports, connectors
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS element (
  id            TEXT PRIMARY KEY,     -- C-004 (component), F2 (function), EXT-1 (external actor)
  kind          TEXT NOT NULL CHECK (kind IN ('component','function','external')),
  name          TEXT NOT NULL,
  parent_id     TEXT REFERENCES element(id) ON DELETE RESTRICT,
  stereotype_id TEXT REFERENCES stereotype(id),
  domain        TEXT CHECK (domain IN ('hw','sw','hw_sw','other')),
  multiplicity  INTEGER NOT NULL DEFAULT 1 CHECK (multiplicity >= 1),
  description   TEXT,
  plain_summary        TEXT,          -- one sentence for the stakeholder view
  plain_summary_origin TEXT CHECK (plain_summary_origin IN ('human','ai')),
  status        TEXT NOT NULL DEFAULT 'not_started'
                CHECK (status IN ('not_started','in_progress','designed','decision_needed','open_question')),
  sort_order    INTEGER NOT NULL DEFAULT 0,
  origin        TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state  TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note   TEXT,
  rev           INTEGER NOT NULL DEFAULT 1,
  created_at    TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  updated_at    TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  CHECK (parent_id IS NULL OR parent_id <> id),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);
CREATE INDEX IF NOT EXISTS idx_element_parent ON element(parent_id);

CREATE TABLE IF NOT EXISTS interface (
  id           TEXT PRIMARY KEY,      -- IF-002
  name         TEXT NOT NULL UNIQUE,  -- NavSolution
  flow_type    TEXT NOT NULL CHECK (flow_type IN ('data','energy','matter')),
  layer        TEXT NOT NULL DEFAULT 'physical' CHECK (layer IN ('physical','functional')),
  rate_value   REAL,
  rate_unit    TEXT REFERENCES unit(symbol),
  transport    TEXT,
  description  TEXT,
  origin       TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note  TEXT,
  rev          INTEGER NOT NULL DEFAULT 1,
  created_at   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  updated_at   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  CHECK ((rate_value IS NULL) = (rate_unit IS NULL)),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);

CREATE TABLE IF NOT EXISTS interface_field (
  id           TEXT PRIMARY KEY,      -- FLD-0012
  interface_id TEXT NOT NULL REFERENCES interface(id) ON DELETE CASCADE,
  name         TEXT NOT NULL,
  value_type   TEXT NOT NULL,         -- float32, float64, int32, uint64, bool, enum, string, bytes
  unit         TEXT REFERENCES unit(symbol),
  range_state  TEXT NOT NULL DEFAULT 'placeholder' CHECK (range_state IN ('set','placeholder','not_applicable')),
  min_value    REAL,
  max_value    REAL,
  enum_values  TEXT,                  -- JSON array
  notes        TEXT,
  sort_order   INTEGER NOT NULL DEFAULT 0,
  rev          INTEGER NOT NULL DEFAULT 1,
  UNIQUE (interface_id, name),
  CHECK (range_state <> 'set' OR (min_value IS NOT NULL AND max_value IS NOT NULL AND min_value <= max_value)),
  CHECK (range_state = 'set' OR (min_value IS NULL AND max_value IS NULL))
);

CREATE TABLE IF NOT EXISTS port (
  id           TEXT PRIMARY KEY,      -- P-0007
  element_id   TEXT NOT NULL REFERENCES element(id) ON DELETE CASCADE,
  name         TEXT NOT NULL,
  direction    TEXT NOT NULL CHECK (direction IN ('in','out','inout')),
  interface_id TEXT REFERENCES interface(id),
  side         TEXT CHECK (side IN ('top','right','bottom','left')),  -- layout hint only
  rev          INTEGER NOT NULL DEFAULT 1,
  UNIQUE (element_id, name)
);

CREATE TABLE IF NOT EXISTS connector (
  id             TEXT PRIMARY KEY,    -- K-0003
  source_port_id TEXT NOT NULL REFERENCES port(id) ON DELETE CASCADE,
  target_port_id TEXT NOT NULL REFERENCES port(id) ON DELETE CASCADE,
  label          TEXT,
  origin         TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state   TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note    TEXT,
  rev            INTEGER NOT NULL DEFAULT 1,
  created_at     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  UNIQUE (source_port_id, target_port_id),
  CHECK (source_port_id <> target_port_id),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);

-- ---------------------------------------------------------------------------
-- 4. Requirements
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS requirement (
  id                  TEXT PRIMARY KEY,   -- STK-003, SYS-012
  parent_id           TEXT REFERENCES requirement(id) ON DELETE RESTRICT,
  level               TEXT NOT NULL CHECK (level IN ('stakeholder','system','subsystem')),
  sort_order          INTEGER NOT NULL DEFAULT 0,
  short_title         TEXT,
  text                TEXT NOT NULL,
  req_type            TEXT CHECK (req_type IN ('functional','performance','physical','interface','nfr','constraint')),
  status              TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft','committed','deprecated')),
  rationale           TEXT,
  verification_method TEXT CHECK (verification_method IN ('test','analysis','inspection','demonstration','simulation')),
  actor_element_id    TEXT REFERENCES element(id),
  gate_score          INTEGER CHECK (gate_score BETWEEN 0 AND 100),
  gate_pass           INTEGER NOT NULL DEFAULT 0 CHECK (gate_pass IN (0,1)),
  gate_json           TEXT,              -- itemised points ledger from the gate
  coach_state         TEXT NOT NULL DEFAULT 'none' CHECK (coach_state IN ('none','reviewed','uncoached')),
  coach_call_id       INTEGER REFERENCES llm_call(id),
  origin              TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state        TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note         TEXT,
  rev                 INTEGER NOT NULL DEFAULT 1,
  created_at          TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  updated_at          TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  CHECK (parent_id IS NULL OR parent_id <> id),
  -- A committed requirement passed the gate, has no placeholder, and was reviewed by a person.
  CHECK (status <> 'committed' OR gate_pass = 1),
  CHECK (status <> 'committed' OR text NOT GLOB '*[[]*[]]*'),
  CHECK (status <> 'committed' OR review_state = 'reviewed'),
  CHECK (status <> 'committed' OR coach_state IN ('reviewed','uncoached')),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);
CREATE INDEX IF NOT EXISTS idx_requirement_parent ON requirement(parent_id);

-- Terms the gate found in a requirement and how each resolved against the model.
CREATE TABLE IF NOT EXISTS requirement_term (
  requirement_id TEXT NOT NULL REFERENCES requirement(id) ON DELETE CASCADE,
  term           TEXT NOT NULL COLLATE NOCASE,
  role           TEXT NOT NULL CHECK (role IN ('actor','object','quantity')),
  resolution     TEXT NOT NULL CHECK (resolution IN ('model','not_model','unresolved')),
  element_id     TEXT REFERENCES element(id) ON DELETE SET NULL,
  PRIMARY KEY (requirement_id, term),
  CHECK (resolution <> 'model' OR element_id IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS requirement_conflict (
  id               TEXT PRIMARY KEY,      -- conflict found by the coach, recorded only on verbatim match
  requirement_a    TEXT NOT NULL REFERENCES requirement(id) ON DELETE CASCADE,
  requirement_b    TEXT NOT NULL REFERENCES requirement(id) ON DELETE CASCADE,
  explanation      TEXT NOT NULL,
  status           TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open','resolved','dismissed')),
  resolution_note  TEXT,
  llm_call_id      INTEGER REFERENCES llm_call(id),
  created_at       TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  CHECK (requirement_a <> requirement_b),
  CHECK (status = 'open' OR length(trim(coalesce(resolution_note,''))) > 0)
);

CREATE TABLE IF NOT EXISTS import_triage (
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  source_name    TEXT NOT NULL,
  row_no         INTEGER NOT NULL,
  raw_text       TEXT NOT NULL,
  raw_json       TEXT,                    -- other columns from the source row
  gate_json      TEXT,
  status         TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','fixed','skipped','committed')),
  requirement_id TEXT REFERENCES requirement(id) ON DELETE SET NULL,
  created_at     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
);

-- ---------------------------------------------------------------------------
-- 5. Links (trace, allocation, realisation, verification)
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS link (
  id             TEXT PRIMARY KEY,        -- L-0042
  link_type      TEXT NOT NULL CHECK (link_type IN (
                   'satisfied_by',        -- requirement  -> element (allocation of a requirement)
                   'allocated_to',        -- function     -> component
                   'realized_by',         -- connector (functional) -> connector (physical)
                   'derives_from',        -- requirement  -> requirement (outside the parent tree)
                   'refines',             -- requirement  -> requirement | scenario
                   'verified_by',         -- requirement  -> test_case | scenario | sm_assertion
                   'traces_to')),         -- anything     -> anything (fallback, discouraged)
  source_kind    TEXT NOT NULL CHECK (source_kind IN ('requirement','element','connector','interface','scenario','state_machine','test_case','sm_assertion')),
  source_id      TEXT NOT NULL,
  target_kind    TEXT NOT NULL CHECK (target_kind IN ('requirement','element','connector','interface','scenario','state_machine','test_case','sm_assertion')),
  target_id      TEXT NOT NULL,
  rationale      TEXT,
  origin         TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai_candidate')),
  ai_proposal_id TEXT REFERENCES ai_proposal(id),
  rev            INTEGER NOT NULL DEFAULT 1,
  created_at     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  created_by     TEXT NOT NULL DEFAULT 'engineer',
  UNIQUE (link_type, source_kind, source_id, target_kind, target_id),
  -- A link chosen from AI candidates needs a note in the engineer's own words.
  CHECK (origin = 'human' OR length(trim(coalesce(rationale,''))) > 0),
  CHECK (origin = 'human' OR ai_proposal_id IS NOT NULL)
);
CREATE INDEX IF NOT EXISTS idx_link_source ON link(source_kind, source_id);
CREATE INDEX IF NOT EXISTS idx_link_target ON link(target_kind, target_id);

CREATE TABLE IF NOT EXISTS test_case (
  id          TEXT PRIMARY KEY,           -- TC-007
  name        TEXT NOT NULL,
  method      TEXT NOT NULL CHECK (method IN ('test','analysis','inspection','demonstration','simulation')),
  description TEXT,
  rev         INTEGER NOT NULL DEFAULT 1
);

-- Evidence is tied to what it was recorded against (SCHEMA.md §7).
CREATE TABLE IF NOT EXISTS verification_record (
  id             TEXT PRIMARY KEY,        -- VR-0003
  requirement_id TEXT NOT NULL REFERENCES requirement(id) ON DELETE CASCADE,
  test_case_id   TEXT REFERENCES test_case(id),
  result         TEXT NOT NULL CHECK (result IN ('pass','fail','inconclusive')),
  evidence_ref   TEXT,                    -- report id, file path, URL
  context_json   TEXT NOT NULL,           -- [{"kind":"requirement","id":"SYS-001","rev":3}, ...]
  context_hash   TEXT NOT NULL,           -- sha256 of canonical context_json
  recorded_at    TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  recorded_by    TEXT NOT NULL,
  note           TEXT
);

-- ---------------------------------------------------------------------------
-- 6. Behaviour: scenarios (sequence + activity) and state machines
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS scenario (
  id           TEXT PRIMARY KEY,          -- SC-001
  name         TEXT NOT NULL,
  description  TEXT,
  has_sequence INTEGER NOT NULL DEFAULT 1 CHECK (has_sequence IN (0,1)),
  has_activity INTEGER NOT NULL DEFAULT 0 CHECK (has_activity IN (0,1)),
  origin       TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note  TEXT,
  rev          INTEGER NOT NULL DEFAULT 1,
  created_at   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  updated_at   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);

CREATE TABLE IF NOT EXISTS seq_lifeline (
  id          TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  scenario_id TEXT NOT NULL REFERENCES scenario(id) ON DELETE CASCADE,
  element_id  TEXT NOT NULL REFERENCES element(id),
  sort_order  INTEGER NOT NULL,
  UNIQUE (scenario_id, element_id)
);

CREATE TABLE IF NOT EXISTS seq_step (
  id               TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  scenario_id      TEXT NOT NULL REFERENCES scenario(id) ON DELETE CASCADE,
  seq              INTEGER NOT NULL,
  step_kind        TEXT NOT NULL CHECK (step_kind IN ('message','self','fragment_open','fragment_else','fragment_close','note')),
  from_lifeline_id TEXT REFERENCES seq_lifeline(id) ON DELETE CASCADE,
  to_lifeline_id   TEXT REFERENCES seq_lifeline(id) ON DELETE CASCADE,
  interface_id     TEXT REFERENCES interface(id),
  field_name       TEXT,                  -- checked against interface_field by code; a miss is shown amber
  label            TEXT NOT NULL,
  fragment_kind    TEXT CHECK (fragment_kind IN ('alt','opt','loop','par')),
  guard            TEXT,
  timing_state     TEXT NOT NULL DEFAULT 'none' CHECK (timing_state IN ('none','set','placeholder')),
  timing_s         REAL,
  origin           TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state     TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note      TEXT,
  UNIQUE (scenario_id, seq),
  CHECK (step_kind NOT IN ('message') OR (from_lifeline_id IS NOT NULL AND to_lifeline_id IS NOT NULL)),
  CHECK (step_kind <> 'fragment_open' OR fragment_kind IS NOT NULL),
  CHECK (timing_state <> 'set' OR timing_s IS NOT NULL),
  CHECK (timing_state = 'set' OR timing_s IS NULL),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);

CREATE TABLE IF NOT EXISTS act_node (
  id                   TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  scenario_id          TEXT NOT NULL REFERENCES scenario(id) ON DELETE CASCADE,
  node_kind            TEXT NOT NULL CHECK (node_kind IN ('initial','final','action','decision','merge','fork','join')),
  label                TEXT,
  performer_element_id TEXT REFERENCES element(id),
  reads_json           TEXT,              -- [{"owner_kind":"element","owner_id":"C-003","property":"battery_capacity"}]
  origin               TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state         TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note          TEXT,
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);

CREATE TABLE IF NOT EXISTS act_edge (
  id           TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  scenario_id  TEXT NOT NULL REFERENCES scenario(id) ON DELETE CASCADE,
  from_node_id TEXT NOT NULL REFERENCES act_node(id) ON DELETE CASCADE,
  to_node_id   TEXT NOT NULL REFERENCES act_node(id) ON DELETE CASCADE,
  guard        TEXT                       -- 'yes' / 'no' / expression text
);

CREATE TABLE IF NOT EXISTS state_machine (
  id               TEXT PRIMARY KEY,      -- SM-001
  owner_element_id TEXT NOT NULL REFERENCES element(id) ON DELETE CASCADE,
  name             TEXT NOT NULL,
  initial_state_id TEXT,                  -- REFERENCES sm_state(id); checked by the gate
  rev              INTEGER NOT NULL DEFAULT 1
);

CREATE TABLE IF NOT EXISTS sm_state (
  id           TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  sm_id        TEXT NOT NULL REFERENCES state_machine(id) ON DELETE CASCADE,
  name         TEXT NOT NULL,
  kind         TEXT NOT NULL DEFAULT 'normal' CHECK (kind IN ('normal','final')),
  entry_effect TEXT,                      -- effect expression (handoff §5.8)
  origin       TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note  TEXT,
  UNIQUE (sm_id, name),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);

CREATE TABLE IF NOT EXISTS sm_event (
  id           TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  sm_id        TEXT NOT NULL REFERENCES state_machine(id) ON DELETE CASCADE,
  name         TEXT NOT NULL,             -- snake_case: gnss_lost
  interface_id TEXT REFERENCES interface(id),
  field_name   TEXT,
  description  TEXT,
  UNIQUE (sm_id, name)
);

-- States in which an event must be handled (a transition) or explicitly ignored.
CREATE TABLE IF NOT EXISTS sm_event_scope (
  event_id TEXT NOT NULL REFERENCES sm_event(id) ON DELETE CASCADE,
  state_id TEXT NOT NULL REFERENCES sm_state(id) ON DELETE CASCADE,
  rev INTEGER NOT NULL DEFAULT 1,
  PRIMARY KEY (event_id, state_id)
);

CREATE TABLE IF NOT EXISTS sm_variable (
  id            TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  sm_id         TEXT NOT NULL REFERENCES state_machine(id) ON DELETE CASCADE,
  name          TEXT NOT NULL,
  value_type    TEXT NOT NULL CHECK (value_type IN ('number','bool','text')),
  initial_value TEXT NOT NULL,
  unit          TEXT REFERENCES unit(symbol),
  UNIQUE (sm_id, name)
);

CREATE TABLE IF NOT EXISTS sm_transition (
  id            TEXT PRIMARY KEY,
  rev INTEGER NOT NULL DEFAULT 1,
  sm_id         TEXT NOT NULL REFERENCES state_machine(id) ON DELETE CASCADE,
  from_state_id TEXT NOT NULL REFERENCES sm_state(id) ON DELETE CASCADE,
  to_state_id   TEXT NOT NULL REFERENCES sm_state(id) ON DELETE CASCADE,
  event_id      TEXT NOT NULL REFERENCES sm_event(id) ON DELETE CASCADE,
  guard_expr    TEXT,                     -- guard language (handoff §5.8)
  effect_expr   TEXT,
  hold_state    TEXT NOT NULL DEFAULT 'none' CHECK (hold_state IN ('none','set','placeholder')),
  hold_s        REAL,                     -- condition must persist this long before firing
  priority      INTEGER NOT NULL DEFAULT 0,
  origin        TEXT NOT NULL DEFAULT 'human' CHECK (origin IN ('human','ai')),
  review_state  TEXT NOT NULL DEFAULT 'reviewed' CHECK (review_state IN ('reviewed','unreviewed')),
  review_note   TEXT,
  CHECK (hold_state <> 'set' OR hold_s IS NOT NULL),
  CHECK (hold_state = 'set' OR hold_s IS NULL),
  CHECK (origin = 'human' OR review_state = 'unreviewed' OR length(trim(coalesce(review_note,''))) > 0)
);

CREATE TABLE IF NOT EXISTS sm_ignore (
  state_id  TEXT NOT NULL REFERENCES sm_state(id) ON DELETE CASCADE,
  event_id  TEXT NOT NULL REFERENCES sm_event(id) ON DELETE CASCADE,
  rationale TEXT NOT NULL CHECK (length(trim(rationale)) > 0),
  rev INTEGER NOT NULL DEFAULT 1,
  PRIMARY KEY (state_id, event_id)
);

CREATE TABLE IF NOT EXISTS sm_assertion (
  id             TEXT PRIMARY KEY,        -- A-001
  rev INTEGER NOT NULL DEFAULT 1,
  sm_id          TEXT NOT NULL REFERENCES state_machine(id) ON DELETE CASCADE,
  requirement_id TEXT REFERENCES requirement(id) ON DELETE SET NULL,
  description    TEXT NOT NULL,
  script_json    TEXT NOT NULL,           -- handoff §5.8
  expect_json    TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS sm_run (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  assertion_id TEXT NOT NULL REFERENCES sm_assertion(id) ON DELETE CASCADE,
  result       TEXT NOT NULL CHECK (result IN ('pass','fail','error')),
  trace_json   TEXT NOT NULL,
  model_hash   TEXT NOT NULL,
  run_at       TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
);

-- ---------------------------------------------------------------------------
-- 7. Views and layout (not revisioned)
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS view_def (
  id              TEXT PRIMARY KEY,       -- VW-structure-root
  name            TEXT NOT NULL,
  view_kind       TEXT NOT NULL CHECK (view_kind IN ('structure','flows','functional','state_machine','activity')),
  root_element_id TEXT REFERENCES element(id) ON DELETE CASCADE,
  owner_id        TEXT,                   -- sm id or scenario id for behaviour views
  filter_json     TEXT,                   -- saved query, handoff §5.4
  is_builtin      INTEGER NOT NULL DEFAULT 0 CHECK (is_builtin IN (0,1))
);

-- Built-in views, seeded on every open (handoff §5.4).
INSERT OR IGNORE INTO view_def (id, name, view_kind, filter_json, is_builtin) VALUES
 ('VW-all', 'All components', 'structure', '{"kinds":["component"]}', 1),
 ('VW-budget', 'Power & mass budget', 'structure', '{"kinds":["component"],"property":{"name":"mass","op":">","value":0}}', 1),
 ('VW-software', 'Software only', 'structure', '{"kinds":["component"],"stereotypes":["SW"]}', 1),
 ('VW-trl', 'TRL below 6', 'structure', '{"kinds":["component"],"property":{"name":"trl","op":"<","value":6}}', 1),
 ('VW-flows', 'Flows', 'flows', '{"kinds":["component","external"]}', 1),
 ('VW-functional', 'Functional', 'functional', NULL, 1);

CREATE TABLE IF NOT EXISTS layout (
  view_id    TEXT NOT NULL REFERENCES view_def(id) ON DELETE CASCADE,
  item_kind  TEXT NOT NULL CHECK (item_kind IN ('element','port','connector','sm_state','act_node')),
  item_id    TEXT NOT NULL,
  x REAL, y REAL, w REAL, h REAL,
  extra_json TEXT,                        -- bend points, label offsets
  PRIMARY KEY (view_id, item_kind, item_id)
);

-- ---------------------------------------------------------------------------
-- 8. Revisions and baselines
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS revision (
  entity_kind TEXT NOT NULL,              -- table name: element, requirement, link, ...
  entity_id   TEXT NOT NULL,
  rev         INTEGER NOT NULL,
  hash        TEXT NOT NULL,              -- sha256 of canonical JSON (SCHEMA.md §6)
  data_json   TEXT NOT NULL,
  deleted     INTEGER NOT NULL DEFAULT 0 CHECK (deleted IN (0,1)),
  changed_at  TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  changed_by  TEXT NOT NULL,
  change_note TEXT,
  change_set  TEXT NOT NULL,              -- one user action; Compare groups revisions by it
  PRIMARY KEY (entity_kind, entity_id, rev)
);
CREATE INDEX IF NOT EXISTS idx_revision_change_set ON revision(change_set);

CREATE TABLE IF NOT EXISTS baseline (
  id         TEXT PRIMARY KEY,            -- BL-03
  name       TEXT NOT NULL,
  note       TEXT,
  model_hash TEXT NOT NULL,
  is_safety  INTEGER NOT NULL DEFAULT 0 CHECK (is_safety IN (0,1)),  -- saved automatically before a rollback
  created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  created_by TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS baseline_item (
  baseline_id TEXT NOT NULL REFERENCES baseline(id) ON DELETE CASCADE,
  entity_kind TEXT NOT NULL,
  entity_id   TEXT NOT NULL,
  rev         INTEGER NOT NULL,
  hash        TEXT NOT NULL,
  PRIMARY KEY (baseline_id, entity_kind, entity_id),
  FOREIGN KEY (entity_kind, entity_id, rev) REFERENCES revision(entity_kind, entity_id, rev)
);

-- ---------------------------------------------------------------------------
-- 9. AI layer
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS provider (
  id       TEXT PRIMARY KEY,              -- 'anthropic', 'openai', 'lmstudio'
  kind     TEXT NOT NULL CHECK (kind IN ('anthropic','openai','openai_compatible')),
  label    TEXT NOT NULL,
  base_url TEXT,
  is_local INTEGER NOT NULL DEFAULT 0 CHECK (is_local IN (0,1)),
  enabled  INTEGER NOT NULL DEFAULT 1 CHECK (enabled IN (0,1))
  -- API keys are never stored here: OS keyring (keyring package) or environment variables.
);

CREATE TABLE IF NOT EXISTS task_route (
  task                 TEXT PRIMARY KEY CHECK (task IN (
                         'req_light_check','req_deep_check','scope_gaps','trace_coach',
                         'gap_inference','allocation_coach','impact_narrative',
                         'behavior_sketch','behavior_analysis','stakeholder_summary','model_chat')),
  trigger_mode         TEXT NOT NULL CHECK (trigger_mode IN ('every_edit','on_demand','when_asked')),
  provider_id          TEXT REFERENCES provider(id),
  model                TEXT,
  fallback_provider_id TEXT REFERENCES provider(id),
  fallback_model       TEXT
);

-- Default providers and routes, seeded on every open. The local model is left for the
-- engineer to pick, so no coach counts as configured until then.
INSERT OR IGNORE INTO provider (id, kind, label, base_url, is_local) VALUES
 ('anthropic', 'anthropic', 'Anthropic', NULL, 0),
 ('openai', 'openai', 'OpenAI', NULL, 0),
 ('lmstudio', 'openai_compatible', 'LM Studio', 'http://localhost:1234/v1', 1);
INSERT OR IGNORE INTO task_route (task, trigger_mode, provider_id, model, fallback_provider_id, fallback_model) VALUES
 ('req_light_check', 'every_edit', 'lmstudio', NULL, NULL, NULL),
 ('req_deep_check', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna'),
 ('scope_gaps', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna'),
 ('trace_coach', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna'),
 ('gap_inference', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna'),
 ('allocation_coach', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna'),
 ('impact_narrative', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'lmstudio', NULL),
 ('behavior_sketch', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna'),
 ('behavior_analysis', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna'),
 ('stakeholder_summary', 'on_demand', 'openai', 'gpt-5.6-luna', 'lmstudio', NULL),
 ('model_chat', 'when_asked', 'anthropic', 'claude-haiku-5-5', 'lmstudio', NULL);

CREATE TABLE IF NOT EXISTS llm_call (
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  task            TEXT NOT NULL,
  provider_id     TEXT,
  model           TEXT,
  prompt_builder  TEXT NOT NULL,          -- R function name, e.g. 'req_coach_prompt'
  prompt_version  TEXT NOT NULL,          -- 'req-coach v4'
  system_prompt   TEXT NOT NULL,
  user_message    TEXT NOT NULL,
  schema_json     TEXT,                   -- structured-output schema sent
  candidates_json TEXT,                   -- numbered handle -> (kind, id) map; never sent to the model
  response_json   TEXT,
  status          TEXT NOT NULL CHECK (status IN ('ok','error','cancelled','blocked')),
  error_kind      TEXT,                   -- no_key | auth | not_found | rate_limit | local_down | model_not_loaded | tls | timeout | spend_cap | local_only | other
  error_text      TEXT,
  input_tokens    INTEGER,
  output_tokens   INTEGER,
  cost_usd        REAL,
  duration_ms     INTEGER,
  snapshot_hash   TEXT NOT NULL,          -- model_hash at invoke time
  target_kind     TEXT,
  target_id       TEXT,
  redactions_json TEXT,
  created_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
);
CREATE INDEX IF NOT EXISTS idx_llm_call_created ON llm_call(created_at);

CREATE TABLE IF NOT EXISTS llm_cache (
  task        TEXT NOT NULL,
  key_hash    TEXT NOT NULL,              -- sha256 of the exact input text (+ prompt_version)
  llm_call_id INTEGER NOT NULL REFERENCES llm_call(id) ON DELETE CASCADE,
  created_at  TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  PRIMARY KEY (task, key_hash)
);

CREATE TABLE IF NOT EXISTS ai_proposal (
  id                  TEXT PRIMARY KEY,   -- PR-0042
  task                TEXT NOT NULL,
  proposal_kind       TEXT NOT NULL CHECK (proposal_kind IN (
                        'question','candidate_link','sketch_element','sketch_function',
                        'sketch_step','sketch_requirement','example_rewrite','gap')),
  source              TEXT NOT NULL DEFAULT 'ai' CHECK (source IN ('ai','structural')),  -- structural = code check
  target_kind         TEXT,
  target_id           TEXT,
  title               TEXT NOT NULL,      -- phrased as a question
  body                TEXT,
  payload_json        TEXT NOT NULL DEFAULT '{}',  -- structure only; validated free of numbers (handoff §5.3)
  candidates_json     TEXT,               -- [{"n":"C2","kind":"element","id":"C-004","why":"..."}]
  confidence          TEXT CHECK (confidence IN ('low','medium','high')),
  status              TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open','kept','dismissed','superseded')),
  engineer_note       TEXT,
  dismiss_reason      TEXT,
  created_entity_kind TEXT,
  created_entity_id   TEXT,
  llm_call_id         INTEGER REFERENCES llm_call(id),
  created_at          TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  resolved_at         TEXT,
  CHECK (status <> 'kept' OR length(trim(coalesce(engineer_note,''))) > 0),
  CHECK (status <> 'dismissed' OR length(trim(coalesce(dismiss_reason,''))) > 0)
);
CREATE INDEX IF NOT EXISTS idx_ai_proposal_status ON ai_proposal(status);

CREATE TABLE IF NOT EXISTS chat_message (
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id      TEXT NOT NULL,
  role            TEXT NOT NULL CHECK (role IN ('user','assistant','tool')),
  content         TEXT NOT NULL,
  tool_calls_json TEXT,
  llm_call_id     INTEGER REFERENCES llm_call(id),
  created_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
);

CREATE TABLE IF NOT EXISTS audit_log (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  at          TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
  actor       TEXT NOT NULL CHECK (actor IN ('engineer','ai','system')),
  action      TEXT NOT NULL,              -- create | update | delete | commit | keep | dismiss | baseline | rollback | import
  entity_kind TEXT,
  entity_id   TEXT,
  detail_json TEXT
);

-- ---------------------------------------------------------------------------
-- 10. Views used by Health and the baseline gate
-- ---------------------------------------------------------------------------

-- Every value the engineer still has to supply.
CREATE VIEW IF NOT EXISTS v_placeholders AS
  SELECT 'property_value' AS entity_kind, pv.id AS entity_id,
         pv.owner_kind AS owner_kind, pv.owner_id AS owner_id,
         pd.name AS item, pd.unit AS unit
    FROM property_value pv JOIN property_def pd ON pd.id = pv.property_def_id
   WHERE pv.value_state = 'placeholder'
  UNION ALL
  SELECT 'interface_field', f.id, 'interface', f.interface_id, f.name, f.unit
    FROM interface_field f
   WHERE f.range_state = 'placeholder'
  UNION ALL
  SELECT 'sm_transition', t.id, 'state_machine', t.sm_id, e.name || ' hold', 's'
    FROM sm_transition t JOIN sm_event e ON e.id = t.event_id
   WHERE t.hold_state = 'placeholder'
  UNION ALL
  SELECT 'seq_step', s.id, 'scenario', s.scenario_id, s.label || ' timing', 's'
    FROM seq_step s
   WHERE s.timing_state = 'placeholder';

-- Requirement drafts holding placeholders: shown on Health, never block a baseline.
CREATE VIEW IF NOT EXISTS v_draft_placeholders AS
  SELECT id AS requirement_id, text
    FROM requirement
   WHERE status = 'draft' AND text GLOB '*[[]*[]]*';

-- AI-originated content nobody has annotated yet.
CREATE VIEW IF NOT EXISTS v_unreviewed AS
  SELECT 'element'       AS entity_kind, id AS entity_id, name  AS label FROM element       WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'interface',     id, name  FROM interface     WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'connector',     id, coalesce(label, id) FROM connector WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'scenario',      id, name  FROM scenario      WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'seq_step',      id, label FROM seq_step      WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'act_node',      id, coalesce(label, id) FROM act_node WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'sm_state',      id, name  FROM sm_state      WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'sm_transition', id, id    FROM sm_transition WHERE review_state = 'unreviewed';

-- Event/state pairs that need a transition or an explicit ignore.
CREATE VIEW IF NOT EXISTS v_sm_unhandled AS
  SELECT s.sm_id, s.id AS state_id, s.name AS state_name, e.id AS event_id, e.name AS event_name
    FROM sm_event_scope sc
    JOIN sm_state s ON s.id = sc.state_id
    JOIN sm_event e ON e.id = sc.event_id
   WHERE NOT EXISTS (SELECT 1 FROM sm_transition t WHERE t.from_state_id = s.id AND t.event_id = e.id)
     AND NOT EXISTS (SELECT 1 FROM sm_ignore i WHERE i.state_id = s.id AND i.event_id = e.id);

-- Ports with no connector. Ports on unreviewed AI sketches are reported through
-- v_unreviewed instead, so one sketch doesn't show up as several gaps.
CREATE VIEW IF NOT EXISTS v_unconnected_ports AS
  SELECT p.id AS port_id, p.element_id, p.name
    FROM port p JOIN element e ON e.id = p.element_id
   WHERE e.review_state = 'reviewed'
     AND NOT EXISTS (SELECT 1 FROM connector c WHERE c.source_port_id = p.id OR c.target_port_id = p.id);

-- Committed requirements with no satisfied_by link.
CREATE VIEW IF NOT EXISTS v_unallocated_requirements AS
  SELECT r.id, r.short_title
    FROM requirement r
   WHERE r.status = 'committed' AND r.level <> 'stakeholder'
     AND NOT EXISTS (SELECT 1 FROM link l WHERE l.link_type = 'satisfied_by'
                       AND l.source_kind = 'requirement' AND l.source_id = r.id);

-- Functions not allocated to any component.
CREATE VIEW IF NOT EXISTS v_unallocated_functions AS
  SELECT e.id, e.name
    FROM element e
   WHERE e.kind = 'function'
     AND NOT EXISTS (SELECT 1 FROM link l WHERE l.link_type = 'allocated_to'
                       AND l.source_kind = 'element' AND l.source_id = e.id);

-- Blockers the database can see. Stale evidence is added by core code
-- (stale_evidence()), because it needs dependency traversal (SCHEMA.md §7).
CREATE VIEW IF NOT EXISTS v_baseline_blockers AS
  SELECT 'placeholder' AS blocker, entity_kind, entity_id, item AS label FROM v_placeholders
  UNION ALL
  SELECT 'unreviewed', entity_kind, entity_id, label FROM v_unreviewed;
