-- Sample project: Survey UAV (the data shown in the wireframes).
-- Load after schema.sql. This is the WORKING COPY state, i.e. after the battery
-- change. tools/build_sample_project.R builds the project properly through the
-- core write path (so revisions exist): it loads the pre-change values, creates
-- BL-03, then applies the three changes listed at the bottom of this file.
-- Expected Health counts on this data are asserted in tests (BUILD_PLAN.md, Phase 0).

PRAGMA foreign_keys = ON;

INSERT INTO project_meta (key, value) VALUES
 ('project_name','Survey UAV'),
 ('scope_text','A battery-powered quadcopter that flies a pre-planned route and photographs an area for mapping, operated from a ground laptop over radio.'),
 ('coaching_policy','tiered'), ('local_only','0'), ('coach_wait_ms','2000');

INSERT INTO unit (symbol, quantity, si_symbol, si_factor, si_offset) VALUES
 ('kg','mass','kg',1,0), ('g','mass','kg',0.001,0),
 ('W','power','W',1,0), ('V','voltage','V',1,0), ('A','current','A',1,0),
 ('mAh','charge','C',3.6,0), ('Wh','energy','J',3600,0),
 ('s','time','s',1,0), ('ms','time','s',0.001,0), ('min','time','s',60,0), ('µs','time','s',0.000001,0),
 ('Hz','frequency','Hz',1,0), ('m','length','m',1,0), ('mm','length','m',0.001,0), ('km','length','m',1000,0),
 ('cm','length','m',0.01,0), ('deg','angle','rad',0.0174532925199433,0), ('%','ratio','1',0.01,0),
 ('°C','temperature','K',1,273.15), ('N','force','N',1,0), ('km2','area','m2',1000000,0);

INSERT INTO stereotype (id, name, applies_to, description) VALUES
 ('STY-001','HW','component','Hardware item'),
 ('STY-002','SW','component','Software item'),
 ('STY-003','HW+SW','component','Hardware with embedded software'),
 ('STY-004','Avionics','component','Flight-critical avionics');

INSERT INTO property_def (id, stereotype_id, name, label, value_type, unit, rollup, required) VALUES
 ('PD-001', NULL, 'mass',             'Mass',             'number','kg', 'sum', 0),
 ('PD-002', NULL, 'power',            'Power',            'number','W',  'sum', 0),
 ('PD-003', NULL, 'trl',              'TRL',              'number', NULL,'none',0),
 ('PD-004', NULL, 'supplier',         'Supplier',         'text',   NULL,'none',0),
 ('PD-005', NULL, 'battery_capacity', 'Battery capacity', 'number','mAh','none',0),
 ('PD-006', NULL, 'return_reserve',   'Return reserve',   'number','mAh','none',0),
 ('PD-007', NULL, 'bus_voltage',      'Bus voltage',      'number','V',  'none',0),
 ('PD-008', NULL, 'pack_voltage',     'Pack voltage',     'number','V',  'none',0),
 ('PD-009', NULL, 'usable_fraction',  'Usable fraction',  'number', NULL,'none',0);

-- Elements: physical
INSERT INTO element (id, kind, name, parent_id, stereotype_id, domain, multiplicity, status, sort_order, plain_summary, plain_summary_origin) VALUES
 ('C-000','component','Survey UAV',              NULL,   NULL,    'other',1,'in_progress',0,'The whole system: aircraft plus ground software.','human'),
 ('C-001','component','Navigation Sensors',      'C-000','STY-001','hw',   1,'designed',   1,'Work out where the drone is and which way it''s pointing.','ai'),
 ('C-004','component','Flight Controller',       'C-000','STY-004','hw_sw',1,'in_progress',2,'The drone''s brain: keeps it stable and flies the planned route.','ai'),
 ('C-003','component','Power System',            'C-000','STY-001','hw',   1,'decision_needed',3,'Battery and wiring that power everything on board.','ai'),
 ('C-006','component','Propulsion',              'C-000','STY-001','hw',   4,'designed',   4,'Four motors and propellers that lift and move the drone.','ai'),
 ('C-005','component','Comms Link',              'C-000','STY-001','hw',   1,'in_progress',5,'Radio connection between the drone and the operator.','ai'),
 ('C-007','component','Payload Camera',          'C-000','STY-001','hw',   1,'open_question',6,'Takes the survey photos and streams previews to the ground.','ai'),
 ('C-002','component','Ground Control Software', 'C-000','STY-002','sw',   1,'not_started',7,'The laptop app the operator uses to plan and watch flights.','ai'),
 ('C-008','component','FC Hardware',             'C-004','STY-001','hw',   1,'designed',   0,NULL,NULL),
 ('C-009','component','Autopilot Software',      'C-004','STY-002','sw',   1,'in_progress',1,NULL,NULL),
 ('EXT-1','external', 'Operator',                NULL,   NULL,    NULL,   1,'designed',   0,NULL,NULL),
 ('EXT-2','external', 'Environment',             NULL,   NULL,    NULL,   1,'designed',   1,NULL,NULL);

-- AI sketch the engineer added from the review queue (still unreviewed)
INSERT INTO element (id, kind, name, parent_id, stereotype_id, domain, status, sort_order, origin, review_state) VALUES
 ('C-010','component','Flight Termination Unit','C-000','STY-001','hw','not_started',8,'ai','unreviewed');

-- Elements: functional
INSERT INTO element (id, kind, name, sort_order, status) VALUES
 ('F1','function','Sense position & attitude',1,'designed'),
 ('F2','function','Control flight',2,'in_progress'),
 ('F3','function','Generate thrust',3,'designed'),
 ('F4','function','Store & distribute energy',4,'decision_needed'),
 ('F5','function','Communicate with operator',5,'in_progress'),
 ('F6','function','Capture imagery',6,'open_question'),
 ('F7','function','Plan & monitor mission',7,'not_started');
INSERT INTO element (id, kind, name, sort_order, status, origin, review_state) VALUES
 ('F8','function','Detect & handle failures',8,'not_started','ai','unreviewed');

INSERT INTO glossary (term, definition, element_id) VALUES
 ('autopilot', NULL, 'C-009'), ('battery', NULL, 'C-003'), ('ground station', NULL, 'C-002'), ('UAV', NULL, 'C-000');

-- Property values (working copy). Values are per unit and roll up times multiplicity,
-- so Propulsion (multiplicity 4, 0.30 kg, 95 W each) gives 1.20 kg and 380 W.
-- Masses sum to 3.31 kg; power to 412 W.
INSERT INTO property_value (id, owner_kind, owner_id, property_def_id, value_state, num_value, text_value, unit) VALUES
 ('V-0001','element','C-001','PD-001','set',0.08,NULL,'kg'),
 ('V-0002','element','C-004','PD-001','set',0.12,NULL,'kg'),
 ('V-0003','element','C-003','PD-001','set',1.36,NULL,'kg'),
 ('V-0004','element','C-006','PD-001','set',0.30,NULL,'kg'),
 ('V-0005','element','C-005','PD-001','set',0.09,NULL,'kg'),
 ('V-0006','element','C-007','PD-001','set',0.46,NULL,'kg'),
 ('V-0007','element','C-004','PD-002','set',8,NULL,'W'),
 ('V-0008','element','C-006','PD-002','set',95,NULL,'W'),
 ('V-0009','element','C-005','PD-002','set',12,NULL,'W'),
 ('V-0010','element','C-007','PD-002','set',12,NULL,'W'),
 ('V-0011','element','C-004','PD-003','set',7,NULL,NULL),
 ('V-0013','element','C-003','PD-005','set',4200,NULL,'mAh'),
 ('V-0014','element','C-003','PD-006','placeholder',NULL,NULL,'mAh'),
 ('V-0015','element','C-003','PD-007','set',28,NULL,'V'),
 ('V-0016','element','C-010','PD-001','placeholder',NULL,NULL,'kg'),
 ('V-0017','element','C-003','PD-008','set',56.4,NULL,'V'),
 ('V-0018','element','C-003','PD-009','set',0.9,NULL,NULL);
-- Endurance analysis (handoff §5.6): capacity_Ah * pack_V * usable * 60 / cruise_W
--   working copy: 4.2 * 56.4 * 0.9 * 60 / 412 = 31.0 min (FAIL vs SYS-001's 35)
--   BL-03:        5.0 * 56.4 * 0.9 * 60 / 412 = 37.0 min
-- An optional property nobody has filled in (e.g. supplier) simply has no row;
-- only values someone has declared as needed are placeholders.

INSERT INTO budget (id, property_def_id, scope_element_id, limit_value, unit, comparison, requirement_id) VALUES
 ('B-001','PD-001','C-000',3.60,'kg','max',NULL),
 ('B-002','PD-002','C-000',450,'W','max',NULL);

-- Interfaces
INSERT INTO interface (id, name, flow_type, layer, rate_value, rate_unit, transport, description) VALUES
 ('IF-001','PowerBus28V','energy','physical',NULL,NULL,'harness','28 V DC distribution'),
 ('IF-002','NavSolution','data','physical',50,'Hz',NULL,'Fused position, velocity and attitude estimate from GNSS and IMU'),
 ('IF-003','TelemetryMsg','data','physical',5,'Hz',NULL,'Aircraft state to the ground'),
 ('IF-004','CommandMsg','data','physical',NULL,NULL,NULL,'Operator commands to the aircraft'),
 ('IF-005','MotorCmd','data','physical',400,'Hz','PWM','Per-motor thrust demand'),
 ('IF-006','ImageStream','data','physical',NULL,NULL,NULL,'Preview images'),
 ('IF-007','Thrust','energy','physical',NULL,NULL,NULL,'Mechanical thrust to the environment'),
 ('IF-008','AirFlow','matter','physical',NULL,NULL,NULL,'Air through the propellers'),
 ('IF-009','RFLink','data','physical',NULL,NULL,'900 MHz radio','Air-ground radio link'),
 ('IF-010','CameraTrigger','data','physical',NULL,NULL,NULL,'Shutter trigger');

INSERT INTO interface_field (id, interface_id, name, value_type, unit, range_state, min_value, max_value, notes, sort_order) VALUES
 ('FLD-0001','IF-002','lat','float64','deg','set',-90,90,'WGS-84',1),
 ('FLD-0002','IF-002','lon','float64','deg','set',-180,180,'WGS-84',2),
 ('FLD-0003','IF-002','alt_msl','float32','m','set',-100,5000,NULL,3),
 ('FLD-0004','IF-002','heading','float32','deg','set',0,360,NULL,4),
 ('FLD-0005','IF-002','heading_accuracy','float32','deg','placeholder',NULL,NULL,'new since BL-03',5),
 ('FLD-0006','IF-002','gnss_valid','bool',NULL,'not_applicable',NULL,NULL,'drives SYS-012',6),
 ('FLD-0007','IF-002','timestamp','uint64','µs','not_applicable',NULL,NULL,'since boot',7),
 ('FLD-0008','IF-004','cmd_type','enum',NULL,'not_applicable',NULL,NULL,'["arm","takeoff","start_mission","rth","land"]',1),
 ('FLD-0009','IF-004','seq','uint32',NULL,'not_applicable',NULL,NULL,NULL,2),
 ('FLD-0010','IF-003','soc','float32','%','set',0,100,NULL,1);

-- Ports
INSERT INTO port (id, element_id, name, direction, interface_id, side) VALUES
 ('P-0001','C-001','nav_out','out','IF-002','bottom'),
 ('P-0002','C-004','nav_in','in','IF-002','top'),
 ('P-0003','C-004','pwr_in','in','IF-001','left'),
 ('P-0004','C-004','motor_out','out','IF-005','bottom'),
 ('P-0005','C-004','tlm_out','out','IF-003','right'),
 ('P-0006','C-004','cmd_in','in','IF-004','right'),
 ('P-0007','C-003','pwr_fc','out','IF-001','right'),
 ('P-0008','C-003','pwr_prop','out','IF-001','bottom'),
 ('P-0009','C-003','pwr_comms','out','IF-001','right'),
 ('P-0010','C-003','pwr_cam','out','IF-001','bottom'),
 ('P-0011','C-005','tlm_in','in','IF-003','left'),
 ('P-0012','C-005','cmd_out','out','IF-004','left'),
 ('P-0013','C-005','rf','inout','IF-009','top'),
 ('P-0014','C-002','rf','inout','IF-009','bottom'),
 ('P-0015','C-006','motor_in','in','IF-005','top'),
 ('P-0016','C-006','pwr_in','in','IF-001','left'),
 ('P-0017','C-006','thrust_out','out','IF-007','bottom'),
 ('P-0018','C-007','img_out','out','IF-006','top'),
 ('P-0019','C-007','trigger_in','in','IF-010','left'),
 ('P-0020','C-007','pwr_in','in','IF-001','left'),
 ('P-0021','C-005','img_in','in','IF-006','bottom'),
 ('P-0022','C-005','pwr_in','in','IF-001','left'),
 ('P-0023','C-010','pwr_in','in','IF-001','left'),
 ('P-0024','C-010','cmd_in','in','IF-004','right'),
 ('P-0025','EXT-2','thrust_in','in','IF-007','top'),
 ('P-0026','C-006','air_in','in','IF-008','bottom'),
 ('P-0027','EXT-2','air_out','out','IF-008','top');

INSERT INTO connector (id, source_port_id, target_port_id, label) VALUES
 ('K-0001','P-0001','P-0002','NavSolution'),
 ('K-0002','P-0005','P-0011','TelemetryMsg'),
 ('K-0003','P-0012','P-0006','CommandMsg'),
 ('K-0004','P-0013','P-0014','RF link'),
 ('K-0005','P-0007','P-0003','PowerBus28V'),
 ('K-0006','P-0008','P-0016','PowerBus28V'),
 ('K-0007','P-0004','P-0015','MotorCmd ×4'),
 ('K-0008','P-0018','P-0021','ImageStream'),
 ('K-0009','P-0009','P-0022','PowerBus28V'),
 ('K-0010','P-0010','P-0020','PowerBus28V'),
 ('K-0011','P-0017','P-0025','thrust'),
 ('K-0012','P-0027','P-0026','air');

INSERT INTO property_value (id, owner_kind, owner_id, property_def_id, value_state, num_value, unit) VALUES
 ('V-0101','connector','K-0005','PD-002','set',8,'W'),
 ('V-0102','connector','K-0006','PD-002','set',380,'W'),
 ('V-0103','connector','K-0009','PD-002','set',12,'W'),
 ('V-0104','connector','K-0010','PD-002','set',12,'W'),
 ('V-0105','connector','K-0005','PD-007','set',28,'V'),
 ('V-0106','connector','K-0006','PD-007','set',28,'V'),
 ('V-0107','connector','K-0009','PD-007','set',28,'V'),
 ('V-0108','connector','K-0010','PD-007','set',28,'V');

-- Requirements
INSERT INTO requirement (id, parent_id, level, sort_order, short_title, text, req_type, status, verification_method, actor_element_id, gate_score, gate_pass, coach_state) VALUES
 ('STK-001',NULL,'stakeholder',1,'Map an area in one flight','During a single flight, the UAV shall survey an area of at least 1 km2.','performance','committed','test','C-000',60,1,'reviewed'),
 ('STK-002',NULL,'stakeholder',2,'Field portability','During field deployment, the UAV system shall be transportable by one person.','physical','committed','demonstration','C-000',60,1,'uncoached'),
 ('STK-003',NULL,'stakeholder',3,'Safe return','When the UAV can no longer continue its mission, the UAV shall return to its launch point.','functional','committed','simulation','C-000',60,1,'reviewed'),
 ('SYS-001','STK-001','system',1,'Endurance','The UAV shall provide a flight endurance of at least 35 minutes at maximum take-off mass in still air.','performance','committed','test','C-000',80,1,'reviewed'),
 ('SYS-002','STK-001','system',2,'Telemetry rate and range','The comms link shall transmit telemetry to the ground station at no less than 5 Hz within 8 km line of sight.','performance','committed','test','C-005',80,1,'reviewed'),
 ('SYS-012','STK-003','system',3,'Return home on GNSS loss','When GNSS signal is lost for more than 3 s, the flight controller shall initiate return-to-home using inertial navigation.','functional','committed','simulation','C-004',100,1,'reviewed'),
 ('SYS-014','STK-003','system',4,'Flight data logging','The autopilot software shall log all flight-critical parameters at 50 Hz.','functional','committed','inspection','C-009',80,1,'reviewed'),
 ('SYS-018','STK-001','system',5,'Ground sample distance','The payload camera shall capture images with a ground sample distance of no more than 2 cm at 100 m above ground level.','performance','committed','analysis','C-007',80,1,'reviewed'),
 ('SYS-021','STK-003','system',6,'Geofence','During flight, the UAV shall not exceed a geofence boundary defined by the operator.','functional','committed','test','C-000',60,1,'reviewed');
-- Drafts (failing the gate, or holding placeholders)
INSERT INTO requirement (id, parent_id, level, sort_order, short_title, text, req_type, status, gate_score, gate_pass) VALUES
 ('SYS-003',NULL,'system',7,'Reliable','The system should be reliable.','nfr','draft',0,0),
 ('SYS-007','STK-002','system',8,'Lightweight, portable','The system shall be lightweight and easy to transport.','physical','draft',20,0),
 ('SYS-031','SYS-001','subsystem',1,'Battery temperature','The power system shall maintain battery cell temperature between [X] °C and [Y] °C during flight.','performance','draft',100,0);

INSERT INTO requirement (id, parent_id, level, sort_order, short_title, text, req_type, status, gate_score, gate_pass, origin, review_state) VALUES
 ('SYS-007a','STK-002','system',9,'Take-off mass','The UAV shall have a maximum take-off mass of no more than [value] kg.','physical','draft',80,0,'human','reviewed');

UPDATE budget SET requirement_id = 'SYS-001' WHERE id = 'B-002';
INSERT INTO analysis_limit (analysis, scope_id, limit_value, unit, comparison, requirement_id) VALUES
 ('endurance','C-000',35,'min','min','SYS-001');

-- Links
INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id, rationale) VALUES
 ('L-0001','satisfied_by','requirement','SYS-001','element','C-003','Battery capacity sets endurance'),
 ('L-0002','satisfied_by','requirement','SYS-001','element','C-006','Propulsion efficiency sets endurance'),
 ('L-0003','satisfied_by','requirement','SYS-002','element','C-005',NULL),
 ('L-0004','satisfied_by','requirement','SYS-012','element','C-004',NULL),
 ('L-0005','satisfied_by','requirement','SYS-012','element','C-001',NULL),
 ('L-0006','satisfied_by','requirement','SYS-014','element','C-004',NULL),
 ('L-0007','satisfied_by','requirement','SYS-018','element','C-007',NULL),
 ('L-0101','allocated_to','element','F1','element','C-001',NULL),
 ('L-0102','allocated_to','element','F2','element','C-009',NULL),
 ('L-0103','allocated_to','element','F3','element','C-006',NULL),
 ('L-0104','allocated_to','element','F4','element','C-003',NULL),
 ('L-0105','allocated_to','element','F5','element','C-005',NULL),
 ('L-0106','allocated_to','element','F5','element','C-002',NULL),
 ('L-0107','allocated_to','element','F6','element','C-007',NULL),
 ('L-0108','allocated_to','element','F7','element','C-002',NULL);

-- Functional flows (artboard 12): ports on functions, typed by functional interfaces,
-- each realised by the physical connector that carries it. The wireframe claims 9
-- flows and labels 8; the ninth (F4 -> F6, electrical) is derived from K-0010.
INSERT INTO interface (id, name, flow_type, layer, description) VALUES
 ('IF-011','PositionEstimate','data','functional','Where the aircraft is and how it is pointing'),
 ('IF-012','FlightStatus','data','functional','Aircraft state for the operator'),
 ('IF-013','OperatorCommands','data','functional','What the operator asks the aircraft to do'),
 ('IF-014','MissionPlan','data','functional','The planned route'),
 ('IF-015','ElectricalEnergy','energy','functional','Electrical energy to on-board functions'),
 ('IF-016','ThrustDemand','data','functional','How much thrust is wanted'),
 ('IF-017','Imagery','data','functional','Captured images');
INSERT INTO port (id, element_id, name, direction, interface_id) VALUES
 ('P-0028','F1','position_out','out','IF-011'), ('P-0029','F2','position_in','in','IF-011'),
 ('P-0030','F2','status_out','out','IF-012'),   ('P-0031','F5','status_in','in','IF-012'),
 ('P-0032','F5','commands_out','out','IF-013'), ('P-0033','F2','commands_in','in','IF-013'),
 ('P-0034','F7','plan_out','out','IF-014'),     ('P-0035','F5','plan_in','in','IF-014'),
 ('P-0036','F4','energy_ctl','out','IF-015'),   ('P-0037','F2','energy_in','in','IF-015'),
 ('P-0038','F4','energy_thrust','out','IF-015'),('P-0039','F3','energy_in','in','IF-015'),
 ('P-0040','F2','demand_out','out','IF-016'),   ('P-0041','F3','demand_in','in','IF-016'),
 ('P-0042','F6','imagery_out','out','IF-017'),  ('P-0043','F5','imagery_in','in','IF-017'),
 ('P-0044','F4','energy_img','out','IF-015'),   ('P-0045','F6','energy_in','in','IF-015');
INSERT INTO connector (id, source_port_id, target_port_id, label) VALUES
 ('K-0013','P-0028','P-0029','position estimate'),
 ('K-0014','P-0030','P-0031','status'),
 ('K-0015','P-0032','P-0033','commands'),
 ('K-0016','P-0034','P-0035','mission plan'),
 ('K-0017','P-0036','P-0037','electrical'),
 ('K-0018','P-0038','P-0039','electrical'),
 ('K-0019','P-0040','P-0041','thrust demand'),
 ('K-0020','P-0042','P-0043','imagery'),
 ('K-0021','P-0044','P-0045','electrical');
INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id) VALUES
 ('L-0301','realized_by','connector','K-0013','connector','K-0001'),
 ('L-0302','realized_by','connector','K-0014','connector','K-0002'),
 ('L-0303','realized_by','connector','K-0015','connector','K-0003'),
 ('L-0304','realized_by','connector','K-0016','connector','K-0004'),
 ('L-0305','realized_by','connector','K-0017','connector','K-0005'),
 ('L-0306','realized_by','connector','K-0018','connector','K-0006'),
 ('L-0307','realized_by','connector','K-0019','connector','K-0007'),
 ('L-0308','realized_by','connector','K-0020','connector','K-0008'),
 ('L-0309','realized_by','connector','K-0021','connector','K-0010');

INSERT INTO test_case (id, name, method, description) VALUES
 ('TC-007','Endurance flight test','test','Hover-cruise profile at MTOM until reserve');
INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id) VALUES
 ('L-0201','verified_by','requirement','SYS-001','test_case','TC-007');
-- Recorded against the BL-03 battery (5000 mAh): stale in the working copy.
INSERT INTO verification_record (id, requirement_id, test_case_id, result, evidence_ref, context_json, context_hash, recorded_by, note) VALUES
 ('VR-0001','SYS-001','TC-007','pass','FT-2026-09-30',
  '[{"kind":"requirement","id":"SYS-001","rev":1},{"kind":"property_value","id":"V-0013","rev":1}]',
  'sha256:recorded-against-bl03','engineer','37.2 min measured');

-- State machine: Flight Controller flight modes
INSERT INTO state_machine (id, owner_element_id, name, initial_state_id) VALUES ('SM-001','C-004','Flight modes','S-DIS');
INSERT INTO sm_state (id, sm_id, name) VALUES
 ('S-DIS','SM-001','Disarmed'), ('S-ARM','SM-001','Armed'), ('S-TKO','SM-001','Takeoff'),
 ('S-MIS','SM-001','Survey mission'), ('S-HLD','SM-001','Hold position'),
 ('S-RTH','SM-001','Return to home'), ('S-LND','SM-001','Land');
INSERT INTO sm_event (id, sm_id, name, interface_id, field_name) VALUES
 ('E-01','SM-001','arm','IF-004','cmd_type'), ('E-02','SM-001','takeoff','IF-004','cmd_type'),
 ('E-03','SM-001','alt_reached',NULL,NULL), ('E-04','SM-001','gnss_lost','IF-002','gnss_valid'),
 ('E-05','SM-001','battery_low','IF-003','soc'), ('E-06','SM-001','link_lost',NULL,NULL),
 ('E-07','SM-001','link_restored',NULL,NULL), ('E-08','SM-001','hold_timeout',NULL,NULL),
 ('E-09','SM-001','mission_done',NULL,NULL), ('E-10','SM-001','home_reached',NULL,NULL),
 ('E-11','SM-001','landed',NULL,NULL);
INSERT INTO sm_variable (id, sm_id, name, value_type, initial_value, unit) VALUES
 ('VAR-1','SM-001','gnss_valid','bool','true',NULL),
 ('VAR-2','SM-001','link_ok','bool','true',NULL),
 ('VAR-3','SM-001','soc','number','78','%');
INSERT INTO sm_transition (id, sm_id, from_state_id, to_state_id, event_id, guard_expr, effect_expr, hold_state, hold_s) VALUES
 ('T-01','SM-001','S-DIS','S-ARM','E-01',NULL,NULL,'none',NULL),
 ('T-02','SM-001','S-ARM','S-TKO','E-02',NULL,NULL,'none',NULL),
 ('T-03','SM-001','S-TKO','S-MIS','E-03',NULL,NULL,'none',NULL),
 ('T-04','SM-001','S-MIS','S-HLD','E-06',NULL,'link_ok := false','none',NULL),
 ('T-05','SM-001','S-MIS','S-RTH','E-04','gnss_valid == false','gnss_valid := false','set',3),
 ('T-06','SM-001','S-MIS','S-RTH','E-05',NULL,NULL,'none',NULL),
 ('T-07','SM-001','S-MIS','S-RTH','E-09',NULL,NULL,'none',NULL),
 ('T-08','SM-001','S-HLD','S-MIS','E-07',NULL,'link_ok := true','none',NULL),
 ('T-09','SM-001','S-HLD','S-RTH','E-08',NULL,NULL,'placeholder',NULL),
 ('T-10','SM-001','S-RTH','S-LND','E-10',NULL,NULL,'none',NULL),
 ('T-11','SM-001','S-LND','S-DIS','E-11',NULL,NULL,'none',NULL);
INSERT INTO sm_event_scope (event_id, state_id) VALUES
 ('E-04','S-MIS'),
 ('E-05','S-MIS'), ('E-05','S-HLD'),
 ('E-06','S-MIS'), ('E-06','S-RTH'), ('E-06','S-LND');
INSERT INTO sm_assertion (id, sm_id, requirement_id, description, script_json, expect_json) VALUES
 ('A-001','SM-001','SYS-012','GNSS loss during the survey leads to return-to-home within 3 s',
  '[{"start":"S-MIS"},{"event":"gnss_lost","set":{"gnss_valid":false},"hold_s":3}]',
  '{"state":"S-RTH","within_s":3.0}');
INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id) VALUES
 ('L-0202','verified_by','requirement','SYS-012','sm_assertion','A-001');

-- Scenario: GNSS loss -> return home (sequence)
INSERT INTO scenario (id, name, description, has_sequence, origin, review_state, review_note) VALUES
 ('SC-002','GNSS loss → return home','Verifies SYS-012',1,'ai','reviewed','Checked step order against the autopilot design');
INSERT INTO seq_lifeline (id, scenario_id, element_id, sort_order) VALUES
 ('LL-1','SC-002','C-001',1), ('LL-2','SC-002','C-004',2), ('LL-3','SC-002','C-006',3),
 ('LL-4','SC-002','C-005',4), ('LL-5','SC-002','C-002',5);
INSERT INTO seq_step (id, scenario_id, seq, step_kind, from_lifeline_id, to_lifeline_id, interface_id, field_name, label, fragment_kind, guard, origin, review_state) VALUES
 ('ST-01','SC-002',1,'message','LL-1','LL-2','IF-002','gnss_valid','gnss_valid = false',NULL,NULL,'human','reviewed'),
 ('ST-02','SC-002',2,'note','LL-2','LL-2',NULL,NULL,'3 s timeout, GNSS still invalid',NULL,NULL,'human','reviewed'),
 ('ST-03','SC-002',3,'message','LL-2','LL-4','IF-003',NULL,'Telemetry: RTH start',NULL,NULL,'human','reviewed'),
 ('ST-04','SC-002',4,'message','LL-4','LL-5','IF-009',NULL,'operator alert',NULL,NULL,'human','reviewed'),
 ('ST-05','SC-002',5,'message','LL-2','LL-3','IF-005',NULL,'MotorCmd: climb',NULL,NULL,'human','reviewed'),
 ('ST-06','SC-002',6,'message','LL-1','LL-2','IF-002',NULL,'INS-only position',NULL,NULL,'human','reviewed'),
 ('ST-07','SC-002',7,'fragment_open',NULL,NULL,NULL,NULL,'operator aborts','opt','operator aborts','human','reviewed'),
 ('ST-08','SC-002',8,'message','LL-5','LL-4','IF-004','abort','CommandMsg: abort',NULL,NULL,'human','reviewed'),
 ('ST-09','SC-002',9,'message','LL-4','LL-2','IF-004','abort','CommandMsg: abort',NULL,NULL,'human','reviewed'),
 ('ST-10','SC-002',10,'fragment_close',NULL,NULL,NULL,NULL,'end opt',NULL,NULL,'human','reviewed');
INSERT INTO seq_step (id, scenario_id, seq, step_kind, from_lifeline_id, to_lifeline_id, interface_id, label, origin, review_state) VALUES
 ('ST-11','SC-002',11,'message','LL-2','LL-4','IF-003','Acknowledge abort to operator','ai','unreviewed');

-- The other three scenarios named on artboard 9. Only "Low battery -> return" is
-- drawn there; the steps of the other two are a plain reading of the state machine.
INSERT INTO scenario (id, name, description, has_sequence, has_activity) VALUES
 ('SC-001','Normal survey mission','Plan, launch, survey and return',1,0),
 ('SC-003','Low battery → return','Reserve check while flying',0,1),
 ('SC-004','Comms loss → hold, then return','Link lost during the survey',1,0);
INSERT INTO seq_lifeline (id, scenario_id, element_id, sort_order) VALUES
 ('LL-6','SC-001','C-002',1), ('LL-7','SC-001','C-005',2), ('LL-8','SC-001','C-004',3), ('LL-9','SC-001','C-006',4), ('LL-10','SC-001','C-007',5),
 ('LL-11','SC-004','C-005',1), ('LL-12','SC-004','C-004',2), ('LL-13','SC-004','C-003',3);
INSERT INTO seq_step (id, scenario_id, seq, step_kind, from_lifeline_id, to_lifeline_id, interface_id, field_name, label) VALUES
 ('ST-12','SC-001',1,'message','LL-6','LL-7','IF-009',NULL,'mission upload'),
 ('ST-13','SC-001',2,'message','LL-7','LL-8','IF-004','cmd_type','CommandMsg: start_mission'),
 ('ST-14','SC-001',3,'message','LL-8','LL-9','IF-005',NULL,'MotorCmd: follow route'),
 ('ST-15','SC-001',4,'message','LL-8','LL-7','IF-003','soc','Telemetry: state of charge'),
 ('ST-16','SC-001',5,'message','LL-10','LL-7','IF-006',NULL,'preview images'),
 ('ST-17','SC-004',1,'message','LL-11','LL-12','IF-003',NULL,'link lost'),
 ('ST-18','SC-004',2,'note','LL-12','LL-12',NULL,NULL,'hold position until the link returns or the hold times out'),
 ('ST-19','SC-004',3,'message','LL-13','LL-12','IF-001',NULL,'power available for the hold');
INSERT INTO act_node (id, scenario_id, node_kind, label, performer_element_id, reads_json) VALUES
 ('N-1','SC-003','initial',NULL,NULL,NULL),
 ('N-2','SC-003','action','Monitor battery state of charge','C-004',NULL),
 ('N-3','SC-003','decision','Below RTH reserve?','C-004','[{"owner_kind":"element","owner_id":"C-003","property":"return_reserve"}]'),
 ('N-4','SC-003','action','Compute energy needed to reach home','C-004','[{"owner_kind":"element","owner_id":"C-003","property":"battery_capacity"}]'),
 ('N-5','SC-003','decision','Enough energy to reach home?','C-004','[{"owner_kind":"element","owner_id":"C-003","property":"battery_capacity"}]'),
 ('N-6','SC-003','action','Return to home and land','C-004',NULL),
 ('N-7','SC-003','action','Land at nearest safe point','C-004',NULL),
 ('N-8','SC-003','final',NULL,NULL,NULL);
INSERT INTO act_edge (id, scenario_id, from_node_id, to_node_id, guard) VALUES
 ('G-1','SC-003','N-1','N-2',NULL), ('G-2','SC-003','N-2','N-3',NULL),
 ('G-3','SC-003','N-3','N-2','no'), ('G-4','SC-003','N-3','N-4','yes'),
 ('G-5','SC-003','N-4','N-5',NULL), ('G-6','SC-003','N-5','N-6','yes'),
 ('G-7','SC-003','N-5','N-7','no'), ('G-8','SC-003','N-6','N-8',NULL), ('G-9','SC-003','N-7','N-8',NULL);

-- AI proposals (review queue)
INSERT INTO llm_call (id, task, provider_id, model, prompt_builder, prompt_version, system_prompt, user_message, status, snapshot_hash, input_tokens, output_tokens, cost_usd, duration_ms) VALUES
 (1,'gap_inference','anthropic','claude-sonnet-5-5','gap_inference_prompt','gaps v2','[system prompt]','[message]','ok','sha256:sample',5200,900,0.03,8400);
INSERT INTO ai_proposal (id, task, proposal_kind, target_kind, target_id, title, body, payload_json, confidence, status, engineer_note, created_entity_kind, created_entity_id, llm_call_id) VALUES
 ('PR-0001','gap_inference','sketch_element',NULL,NULL,'What brings the aircraft down if the Flight Controller fails?',
  'Every failure-handling path runs through the Flight Controller.',
  '{"name":"Flight Termination Unit","stereotype":"HW","ports":[{"name":"pwr_in","direction":"in","interface":"PowerBus28V"},{"name":"cmd_in","direction":"in","interface":"CommandMsg"}]}',
  'medium','kept','Independent path needed for the flight-termination rule in our ops manual','element','C-010',1),
 ('PR-0002','gap_inference','question','element','C-003','What keeps the battery inside its temperature range?',
  'Lithium cells lose capacity outside their range; SYS-001 depends on capacity.','{}','medium','open',NULL,NULL,NULL,1),
 ('PR-0003','gap_inference','question','element','C-005','Who is allowed to command the aircraft over the uplink?',
  NULL,'{}','high','open',NULL,NULL,NULL,1);
INSERT INTO ai_proposal (id, task, proposal_kind, source, target_kind, target_id, title, payload_json, status) VALUES
 ('PR-0004','gap_inference','gap','structural','port','P-0019','Payload Camera port trigger_in has no connector','{}','open'),
 ('PR-0005','gap_inference','gap','structural','element','C-002','Ground Control Software has no linked requirements','{}','open');

INSERT INTO id_counter (prefix, next_num) VALUES
 ('STK',4),('SYS',32),('C',11),('F',9),('EXT',3),('IF',18),('FLD',11),('P',46),('K',22),('PD',10),('STY',5),('ST',20),
 ('SC',5),('SM',2),('E',12),('T',12),('VAR',4),('LL',14),('A',2),('N',9),('G',10),
 ('TC',8),('VR',2),('BL',4),('PR',6),('L',310),('V',109),('B',3);

-- Changes since BL-03 (applied by tools/build_sample_project.R through the write path):
--   1. One change set: V-0013 battery_capacity 5000 -> 4200 mAh and V-0003 Power System mass 1.47 -> 1.36 kg
--   2. SYS-002 text: "within 5 km" -> "within 8 km"
--   3. FLD-0005 NavSolution.heading_accuracy added (range placeholder)
