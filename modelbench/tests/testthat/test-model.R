test_that("the sample's connectors all pass the gate", {
  con <- local_sample()
  expect_identical(nrow(element_gate_findings(con)), 0L)
})

test_that("a connector needs one interface and compatible directions", {
  con <- local_sample()
  expect_true(check_connector(con, "P-0001", "P-0002")$ok)
  # MotorCmd out to PowerBus28V in: the handoff's example, with the toast's words.
  wrong <- check_connector(con, "P-0004", "P-0016")
  expect_false(wrong$ok)
  expect_identical(wrong$reason, "Can't connect: MotorCmd out to PowerBus28V in.")
  expect_error(write_connector(con, fields = list(source_port_id = "P-0004", target_port_id = "P-0016")), class = "modelbench_refused")
  # Same interface, two outs.
  expect_false(check_connector(con, "P-0007", "P-0008")$ok)
  # In to out, the wrong way round.
  expect_false(check_connector(con, "P-0002", "P-0001")$ok)
  # Inout works both ways.
  expect_true(check_connector(con, "P-0014", "P-0013")$ok)
  write_port(con, "P-X", list(element_id = "C-005", name = "blank", direction = "in"))
  expect_match(check_connector(con, "P-0001", "P-X")$reason, "has no interface")
  expect_match(element_gate_findings(con)$problem, "blank has no interface")
})

test_that("between a parent and child, directions match", {
  con <- local_sample()
  write_port(con, "P-C", list(element_id = "C-009", name = "nav", direction = "in", interface_id = "IF-002"))
  expect_true(check_connector(con, "P-0002", "P-C")$ok)
  write_port(con, "P-D", list(element_id = "C-009", name = "nav_o", direction = "out", interface_id = "IF-002"))
  expect_false(check_connector(con, "P-0002", "P-D")$ok)
})

test_that("a value's unit must measure the same thing as its property", {
  con <- local_sample()
  expect_error(write_property_value(con, "V-0001", list(unit = "W")), class = "modelbench_refused")
  expect_no_error(write_property_value(con, "V-0001", list(unit = "g", num_value = 80)))
})

test_that("roll-ups match SCHEMA §10 and report placeholders", {
  con <- local_sample()
  mass <- rollup(con, "mass", "C-000")
  expect_equal(mass$value, 3.31)
  expect_identical(mass$excluded_placeholders, 1L)
  expect_identical(mass$excluded_owners, "C-010")
  expect_equal(rollup(con, "power", "C-000")$value, 412)
  expect_equal(rollup(con, "mass", "C-006")$value, 1.2)
  expect_error(rollup(con, "trl", "C-000"), "doesn't roll up")
  expect_error(rollup(con, "nope", "C-000"), "No property")
})

test_that("roll-ups convert units and carry multiplicity down the tree", {
  con <- local_sample()
  write_property_value(con, "V-0001", list(num_value = 80, unit = "g"))
  expect_equal(rollup(con, "mass", "C-000")$value, 3.31)
  write_element(con, "C-006", list(multiplicity = 2L))
  expect_equal(rollup(con, "mass", "C-000")$value, 3.31 - 1.2 + 0.6)
})

test_that("budgets state the sample's numbers", {
  budgets <- budget_status(local_sample())
  expect_identical(budgets$text, c("3.31 / 3.60 kg", "412 / 450 W"))
  expect_true(all(budgets$pass))
  expect_match(budgets$note[1], "excludes 1 placeholder")
})

test_that("endurance is 31.0 min and fails its limit, and BL-03 form was 37.0", {
  con <- local_sample()
  result <- run_analysis(con, "endurance", "C-000")
  expect_equal(round(result$value, 1), 31.0)
  expect_identical(result$status, "ok")
  stored <- DBI::dbGetQuery(con, "SELECT * FROM analysis_result WHERE analysis = 'endurance'")
  expect_equal(stored$value, result$value)
  status <- analysis_status(con)
  expect_false(status$pass)
  expect_identical(status$text, "31 / 35 min")
  write_property_value(con, "V-0013", list(num_value = 5000))
  expect_equal(round(run_analysis(con, "endurance", "C-000")$value, 1), 37.0)
  expect_true(analysis_status(con)$pass)
})

test_that("a placeholder input stops an analysis and says which one", {
  con <- local_sample()
  set_property_value(con, "V-0013", "")
  result <- run_analysis(con, "endurance", "C-000")
  expect_identical(result$status, "missing_input")
  expect_identical(result$message, "can't compute: battery_capacity is [value]")
  expect_true(is.na(result$value))
  expect_false(analysis_status(con)$pass)
})

test_that("evidence context includes what the analysis reads", {
  con <- local_sample()
  ids <- vapply(verification_context(con, "SYS-001"), function(entry) entry$id, "")
  expect_true(all(c("V-0013", "V-0017", "V-0018", "V-0007", "V-0008") %in% ids))
  expect_false(anyDuplicated(ids) > 0L)
})

test_that("set_property_value checks numbers and clears to a placeholder", {
  con <- local_sample()
  set_property_value(con, "V-0014", "1250")
  expect_identical(read_entity(con, "property_value", "V-0014")$value_state, "set")
  expect_error(set_property_value(con, "V-0014", "lots"), class = "modelbench_refused")
  set_property_value(con, "V-0014", " ")
  row <- read_entity(con, "property_value", "V-0014")
  expect_identical(row$value_state, "placeholder")
  expect_true(is.na(row$num_value))
})

test_that("review takes a note, and AI origin stays", {
  con <- local_sample()
  expect_error(review_entity(con, "element", "C-010", "  "), class = "modelbench_refused")
  review_entity(con, "element", "C-010", "Ops manual needs it")
  row <- read_entity(con, "element", "C-010")
  expect_identical(row$review_state, "reviewed")
  expect_identical(row$origin, "ai")
  expect_identical(count_rows(con, "v_unreviewed"), 2L)
})

test_that("grouping wraps the selection at its own level, in one change set", {
  con <- local_sample()
  group <- group_into_subsystem(con, c("C-005", "C-007"), "Payload and comms")
  expect_identical(read_entity(con, "element", group$id)$parent_id, "C-000")
  expect_identical(read_entity(con, "element", "C-005")$parent_id, group$id)
  sets <- DBI::dbGetQuery(con, "SELECT DISTINCT change_set FROM revision WHERE entity_id IN (?, 'C-005', 'C-007')", params = list(group$id))
  expect_identical(nrow(sets), 1L)
  expect_error(group_into_subsystem(con, c("C-005", "C-009")), class = "modelbench_refused")
  expect_equal(rollup(con, "mass", "C-000")$value, 3.31)
})

test_that("the structure view shows one level, with gaps, sketches and stubs", {
  view <- structure_view(local_sample())
  expect_identical(view$root, "C-000")
  expect_setequal(view$nodes$id, c("C-001", "C-002", "C-003", "C-004", "C-005", "C-006", "C-007", "C-010"))
  expect_identical(view$nodes$id[view$nodes$gap], "C-007")
  expect_match(view$nodes$gap_note[view$nodes$id == "C-007"], "trigger_in has no connector")
  expect_identical(view$nodes$review_state[view$nodes$id == "C-010"], "unreviewed")
  # The physical connectors at this level; thrust and air reach the hidden environment.
  expect_setequal(view$edges$id[!view$edges$stub], sprintf("K-%04d", 1:10))
  expect_setequal(view$edges$id[view$edges$stub], c("K-0011", "K-0012"))
  expect_false(any(view$edges$id %in% sprintf("K-%04d", 13:21)))
})

test_that("drilling down shows children and the parent's ports on the boundary", {
  view <- structure_view(local_sample(), root = "C-004")
  expect_setequal(view$nodes$id, c("C-008", "C-009"))
  expect_identical(view$breadcrumb, c("C-000", "C-004"))
  expect_setequal(view$boundary_ports$id, c("P-0002", "P-0003", "P-0004", "P-0005", "P-0006"))
  expect_identical(nrow(view$edges), 0L)
})

test_that("a connector into a grouped block draws to the block that holds it", {
  con <- local_sample()
  group <- group_into_subsystem(con, c("C-005", "C-007"), "Group")
  view <- structure_view(con)
  expect_true(group$id %in% view$nodes$id)
  edge <- view$edges[view$edges$id == "K-0002", ]
  expect_identical(edge$target_holder, group$id)
  # Inside the group's own level, the connector between its members is drawn.
  inner <- structure_view(con, root = group$id)
  expect_true("K-0008" %in% inner$edges$id)
  expect_false(any(view$edges$id == "K-0008"))
})

test_that("saved views filter, and hidden ends become stubs", {
  con <- local_sample()
  software <- structure_view(con, "VW-software")
  expect_identical(software$nodes$id, "C-002")
  expect_true(all(software$edges$stub))
  expect_identical(nrow(structure_view(con, "VW-trl")$nodes), 0L)
  write_property_value(con, "V-0011", list(num_value = 4))
  expect_identical(structure_view(con, "VW-trl")$nodes$id, "C-004")
  expect_true("C-006" %in% structure_view(con, "VW-budget")$nodes$id)
  expect_false("C-002" %in% structure_view(con, "VW-budget")$nodes$id)
})

test_that("layout persists per view and isn't a model change", {
  con <- local_sample()
  hash <- model_hash(con)
  save_layout(con, "VW-all", layout_items(data.frame(id = "C-001", x = 10, y = 20)))
  view <- structure_view(con)
  expect_equal(view$nodes$x[view$nodes$id == "C-001"], 10)
  expect_true(is.na(structure_view(con, "VW-budget")$nodes$x[1]))
  expect_identical(model_hash(con), hash)
  set_pinned(con, "VW-all", "C-001", TRUE)
  view <- structure_view(con)
  expect_true(view$nodes$pinned[view$nodes$id == "C-001"])
  expect_equal(view$nodes$x[view$nodes$id == "C-001"], 10)
})

test_that("undo and redo restore edits, deletes and creates, without rewriting history", {
  con <- local_built_sample()
  stack <- new_command_stack()
  edit <- write_element(con, "C-001", list(name = "Nav"))
  stack <- push_command(stack, change_command(edit$change_set))
  stack <- undo_command(con, stack)
  expect_identical(read_entity(con, "element", "C-001")$name, "Navigation Sensors")
  expect_identical(read_entity(con, "element", "C-001")$rev, 3L)
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM revision WHERE entity_id = 'C-001'")$n, 3L)
  expect_length(stack$redo, 1L)
  stack <- redo_command(con, stack)
  expect_identical(read_entity(con, "element", "C-001")$name, "Nav")
  expect_length(stack$undo, 1L)

  created <- write_element(con, fields = list(kind = "component", name = "New", parent_id = "C-000"))
  stack <- push_command(stack, change_command(created$change_set))
  expect_length(stack$redo, 0L)
  stack <- undo_command(con, stack)
  expect_null(read_entity(con, "element", created$id))
  stack <- redo_command(con, stack)
  expect_identical(read_entity(con, "element", created$id)$name, "New")

  ports_before <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM port")$n
  gone <- delete_entity(con, "element", "C-010")
  expect_null(read_entity(con, "port", "P-0023"))
  stack <- push_command(stack, change_command(gone$change_set))
  stack <- undo_command(con, stack)
  expect_identical(read_entity(con, "element", "C-010")$origin, "ai")
  expect_identical(read_entity(con, "element", "C-010")$review_state, "unreviewed")
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM port")$n, ports_before)
  expect_identical(read_entity(con, "property_value", "V-0016")$value_state, "placeholder")
})

test_that("undoing a group restores the parents", {
  con <- local_built_sample()
  group <- group_into_subsystem(con, c("C-005", "C-007"), "Group")
  stack <- push_command(new_command_stack(), change_command(group$change_set))
  stack <- undo_command(con, stack)
  expect_identical(read_entity(con, "element", "C-005")$parent_id, "C-000")
  expect_null(read_entity(con, "element", group$id))
  expect_equal(rollup(con, "mass", "C-000")$value, 3.31)
})

test_that("undo refuses a row with no history instead of deleting it", {
  con <- local_sample()
  edit <- write_element(con, "C-001", list(name = "Nav"))
  expect_error(revert_change_set(con, edit$change_set), class = "modelbench_refused")
  expect_identical(read_entity(con, "element", "C-001")$name, "Nav")
})

test_that("layout moves undo and redo too", {
  con <- local_sample()
  save_layout(con, "VW-all", layout_items(data.frame(id = "C-001", x = 1, y = 1)))
  before <- layout_rows(con, "VW-all")
  save_layout(con, "VW-all", layout_items(data.frame(id = "C-001", x = 9, y = 9)))
  after <- layout_rows(con, "VW-all")
  stack <- push_command(new_command_stack(), layout_command("VW-all", before, after))
  hash <- model_hash(con)
  stack <- undo_command(con, stack)
  expect_equal(layout_rows(con, "VW-all")$x, 1)
  stack <- redo_command(con, stack)
  expect_equal(layout_rows(con, "VW-all")$x, 9)
  expect_identical(model_hash(con), hash)
})

test_that("an empty stack does nothing", {
  con <- local_sample()
  expect_identical(undo_command(con, new_command_stack()), new_command_stack())
  expect_identical(redo_command(con, new_command_stack()), new_command_stack())
})

test_that("where-used finds ports, scenarios, events, requirements and functional flows", {
  used <- interface_where_used(local_sample(), "IF-002")
  expect_setequal(used$ports$element, c("Navigation Sensors", "Flight Controller"))
  expect_true("SC-002" %in% used$scenarios$id)
  expect_identical(used$events$name, "gnss_lost")
  expect_true("SYS-012" %in% used$requirements$id)
  expect_identical(used$functional_flows$id, "K-0013")
})

test_that("clearing a range to placeholder clears its bounds", {
  con <- local_sample()
  save_interface_field(con, "FLD-0005", list(range_state = "set", min_value = 0, max_value = 5))
  expect_equal(read_entity(con, "interface_field", "FLD-0005")$max_value, 5)
  expect_identical(count_rows(con, "v_placeholders"), 3L)
  save_interface_field(con, "FLD-0005", list(range_state = "placeholder"))
  expect_true(is.na(read_entity(con, "interface_field", "FLD-0005")$max_value))
})

test_that("Health shows the SCHEMA §10 numbers", {
  health <- model_health(local_sample())
  expect_length(health$blockers$placeholders$entity_id, 4L)
  expect_true("Power System.return_reserve · [value] mAh" %in% health$blockers$placeholders$text)
  expect_true("NavSolution.heading_accuracy · [min] … [max] deg" %in% health$blockers$placeholders$text)
  expect_identical(nrow(health$blockers$unreviewed), 3L)
  expect_identical(health$blockers$draft_placeholders, 2L)
  expect_identical(health$requirements$committed, 9L)
  expect_identical(health$requirements$failing, 4L)
  expect_identical(health$requirements$uncoached, 1L)
  expect_identical(health$requirements$unallocated, 1L)
  expect_identical(health$architecture$unconnected_ports, 1L)
  expect_identical(health$architecture$interfaces, 10L)
  expect_identical(health$architecture$gate_findings, 0L)
  expect_identical(health$functional$unallocated, 1L)
  expect_identical(health$behavior$scenarios, 4L)
  expect_identical(health$behavior$unhandled, 3L)
  expect_identical(health$behavior$missing_fields, 2L)
  expect_identical(health$budgets$text, c("3.31 / 3.60 kg", "412 / 450 W"))
  expect_identical(health$analyses$text, "31 / 35 min")
  expect_false(health$analyses$pass)
  expect_identical(health$ai$kept, 1L)
})

test_that("an empty project's Health doesn't fall over", {
  health <- model_health(local_project())
  expect_identical(nrow(health$budgets), 0L)
  expect_identical(nrow(health$analyses), 0L)
  expect_identical(health$architecture$components, 0L)
})

test_that("the synthetic model is 150 components and 600 ports", {
  con <- local_project()
  build_synthetic_model(con)
  view <- structure_view(con)
  expect_identical(nrow(view$nodes), 150L)
  expect_identical(nrow(view$ports), 600L)
  expect_gt(nrow(view$edges), 200L)
})
