-- The AI's failure-mode suggestions (docs/VIEWPOINTS_AND_FAILURES.md §5): a
-- 'failure_suggest' task route, a 'sketch_failure' proposal kind, and unreviewed failure
-- modes counted with the other unreviewed drafts. As in 002, tables are rebuilt to change
-- their CHECK constraints. link.ai_proposal_id refers to ai_proposal; run_migrations turns
-- foreign keys off for the rebuild and checks every reference before it commits.
PRAGMA legacy_alter_table = ON;

CREATE TABLE task_route_new (
  task                 TEXT PRIMARY KEY CHECK (task IN (
                         'req_light_check','req_deep_check','scope_gaps','trace_coach',
                         'gap_inference','allocation_coach','impact_narrative',
                         'behavior_sketch','behavior_analysis','stakeholder_summary','model_chat','failure_suggest')),
  trigger_mode         TEXT NOT NULL CHECK (trigger_mode IN ('every_edit','on_demand','when_asked')),
  provider_id          TEXT REFERENCES provider(id),
  model                TEXT,
  fallback_provider_id TEXT REFERENCES provider(id),
  fallback_model       TEXT
);


INSERT INTO task_route_new (task, trigger_mode, provider_id, model, fallback_provider_id, fallback_model) SELECT task, trigger_mode, provider_id, model, fallback_provider_id, fallback_model FROM task_route;
DROP TABLE task_route;
ALTER TABLE task_route_new RENAME TO task_route;
INSERT OR IGNORE INTO task_route (task, trigger_mode, provider_id, model, fallback_provider_id, fallback_model) VALUES
 ('failure_suggest', 'on_demand', 'anthropic', 'claude-sonnet-5-5', 'openai', 'gpt-5.6-luna');

CREATE TABLE ai_proposal_new (
  id                  TEXT PRIMARY KEY,   -- PR-0042
  task                TEXT NOT NULL,
  proposal_kind       TEXT NOT NULL CHECK (proposal_kind IN (
                        'question','candidate_link','sketch_element','sketch_function',
                        'sketch_step','sketch_requirement','example_rewrite','gap','sketch_failure')),
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


INSERT INTO ai_proposal_new (id, task, proposal_kind, source, target_kind, target_id, title, body, payload_json, candidates_json, confidence, status, engineer_note, dismiss_reason, created_entity_kind, created_entity_id, llm_call_id, created_at, resolved_at) SELECT id, task, proposal_kind, source, target_kind, target_id, title, body, payload_json, candidates_json, confidence, status, engineer_note, dismiss_reason, created_entity_kind, created_entity_id, llm_call_id, created_at, resolved_at FROM ai_proposal;
DROP TABLE ai_proposal;
ALTER TABLE ai_proposal_new RENAME TO ai_proposal;
CREATE INDEX IF NOT EXISTS idx_ai_proposal_status ON ai_proposal(status);

DROP VIEW IF EXISTS v_baseline_blockers;
DROP VIEW IF EXISTS v_unreviewed;
CREATE VIEW IF NOT EXISTS v_unreviewed AS
  SELECT 'element'       AS entity_kind, id AS entity_id, name  AS label FROM element       WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'interface',     id, name  FROM interface     WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'connector',     id, coalesce(label, id) FROM connector WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'scenario',      id, name  FROM scenario      WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'seq_step',      id, label FROM seq_step      WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'act_node',      id, coalesce(label, id) FROM act_node WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'sm_state',      id, name  FROM sm_state      WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'sm_transition', id, id    FROM sm_transition WHERE review_state = 'unreviewed'
  UNION ALL SELECT 'failure_mode',  id, id    FROM failure_mode  WHERE review_state = 'unreviewed';

CREATE VIEW IF NOT EXISTS v_baseline_blockers AS
  SELECT 'placeholder' AS blocker, entity_kind, entity_id, item AS label FROM v_placeholders
  UNION ALL
  SELECT 'unreviewed', entity_kind, entity_id, label FROM v_unreviewed;

PRAGMA legacy_alter_table = OFF;
