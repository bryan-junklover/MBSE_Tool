-- Adds the 'hosted_on' link type: software element -> the hardware it runs on.
-- SQLite can't change a CHECK constraint in place, so the link table is rebuilt.
-- Legacy rename keeps SQLite from re-checking the views that name `link` while
-- the table is briefly missing; they resolve to the new table afterwards.
PRAGMA legacy_alter_table = ON;

CREATE TABLE link_new (
  id             TEXT PRIMARY KEY,        -- L-0042
  link_type      TEXT NOT NULL CHECK (link_type IN (
                   'satisfied_by',        -- requirement  -> element (allocation of a requirement)
                   'allocated_to',        -- function     -> component
                   'realized_by',         -- connector (functional) -> connector (physical)
                   'hosted_on',           -- software element -> the hardware element it runs on
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

INSERT INTO link_new (id, link_type, source_kind, source_id, target_kind, target_id, rationale, origin, ai_proposal_id, rev, created_at, created_by)
  SELECT id, link_type, source_kind, source_id, target_kind, target_id, rationale, origin, ai_proposal_id, rev, created_at, created_by FROM link;
DROP TABLE link;
ALTER TABLE link_new RENAME TO link;
CREATE INDEX IF NOT EXISTS idx_link_source ON link(source_kind, source_id);
CREATE INDEX IF NOT EXISTS idx_link_target ON link(target_kind, target_id);

PRAGMA legacy_alter_table = OFF;
