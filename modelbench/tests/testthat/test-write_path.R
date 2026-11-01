new_element <- function(con, name = "Part", ...) {
  write_element(con, fields = list(kind = "component", name = name, ...))
}

test_that("revision round trip: write, revise, read back by rev", {
  con <- local_project()
  first <- new_element(con, "Battery")
  expect_identical(first$id, "C-001")
  expect_identical(first$rev, 1L)
  expect_identical(first$area, "model")
  second <- write_element(con, first$id, list(name = "Big battery"), note = "Renamed")
  expect_identical(second$rev, 2L)
  expect_false(identical(first$change_set, second$change_set))

  expect_identical(read_entity_rev(con, "element", "C-001", 1L)$name, "Battery")
  expect_identical(read_entity_rev(con, "element", "C-001", 2L)$name, "Big battery")
  expect_null(read_entity_rev(con, "element", "C-001", 3L))
  expect_identical(read_entity(con, "element", "C-001")$name, "Big battery")

  revision <- DBI::dbGetQuery(con, "SELECT * FROM revision WHERE entity_id = 'C-001' ORDER BY rev")
  expect_identical(revision$hash[2], sha256(revision$data_json[2]))
  expect_identical(revision$change_note[2], "Renamed")
  audit <- DBI::dbGetQuery(con, "SELECT action FROM audit_log WHERE entity_id = 'C-001' ORDER BY id")$action
  expect_identical(audit, c("create", "update"))
})

test_that("canonical JSON sorts keys, drops whitespace and keeps NULLs", {
  expect_identical(canonical_json(list(b = 1.36, a = NA, c = list(z = "x", y = 2L))),
                   '{"a":null,"b":1.36,"c":{"y":2,"z":"x"}}')
})

test_that("ids follow each prefix's format and are never reused", {
  con <- local_project()
  expect_identical(allocate_id(con, "F"), "F1")
  expect_identical(allocate_id(con, "BL"), "BL-01")
  expect_identical(allocate_id(con, "FLD"), "FLD-0001")
  expect_identical(allocate_id(con, "ST"), "ST-01")
  expect_identical(allocate_id(con, "STY"), "STY-001")
  expect_error(allocate_id(con, "NOPE"), "Unknown id prefix")

  created <- new_element(con)
  delete_entity(con, "element", created$id)
  expect_identical(new_element(con)$id, "C-002")
})

test_that("a failed write leaves nothing behind", {
  con <- local_project()
  counter_before <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM id_counter")$n
  expect_error(write_element(con, "C-009", list(kind = "bogus", name = "x")), "constraint", ignore.case = TRUE)
  expect_null(read_entity(con, "element", "C-009"))
  expect_identical(count_rows(con, "revision"), 0L)
  expect_identical(count_rows(con, "audit_log"), 0L)
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM id_counter")$n, counter_before)
})

test_that("a change set is atomic and shared", {
  con <- local_project()
  expect_error(with_change_set(con, function(change_set) {
    write_element(con, "C-1", list(kind = "component", name = "a"), change_set = change_set)
    write_element(con, "C-2", list(kind = "bogus", name = "b"), change_set = change_set)
  }), "constraint", ignore.case = TRUE)
  expect_null(read_entity(con, "element", "C-1"))

  with_change_set(con, function(change_set) {
    write_element(con, "C-1", list(kind = "component", name = "a"), change_set = change_set)
    write_element(con, "C-2", list(kind = "component", name = "b"), change_set = change_set)
  })
  expect_length(unique(DBI::dbGetQuery(con, "SELECT change_set FROM revision")$change_set), 1L)
})

test_that("write_entity refuses unknown fields, rev and key changes", {
  con <- local_project()
  created <- new_element(con)
  expect_error(write_element(con, created$id, list(nope = 1)), "Unknown element fields")
  expect_error(write_element(con, created$id, list(rev = 9L)), "set by the write path")
  expect_error(write_element(con, created$id, list(id = "C-9")), "Key columns")
  expect_error(write_entity(con, "seq_note", "x"), "Unknown entity kind")
})

test_that("parent cycles are refused for elements and requirements", {
  con <- local_project()
  a <- new_element(con, "a")$id
  b <- new_element(con, "b", parent_id = a)$id
  c3 <- new_element(con, "c", parent_id = b)$id
  expect_error(write_element(con, a, list(parent_id = c3)), class = "modelbench_refused")
  expect_identical(read_entity(con, "element", a)$rev, 1L)
  expect_no_error(write_element(con, c3, list(parent_id = a)))

  r1 <- write_requirement(con, fields = list(level = "system", text = "x"))$id
  r2 <- write_requirement(con, fields = list(level = "system", text = "y", parent_id = r1))$id
  expect_error(write_requirement(con, r1, list(parent_id = r2)), class = "modelbench_refused")
})

test_that("polymorphic owners and link ends must exist", {
  con <- local_project()
  write_property_def(con, "PD-1", list(name = "mass", label = "Mass", value_type = "number"))
  expect_error(write_property_value(con, fields = list(owner_kind = "element", owner_id = "C-404", property_def_id = "PD-1")),
               class = "modelbench_refused")
  a <- new_element(con)$id
  expect_no_error(write_property_value(con, fields = list(owner_kind = "element", owner_id = a, property_def_id = "PD-1")))
  expect_error(write_link(con, fields = list(link_type = "traces_to", source_kind = "element", source_id = a,
                                             target_kind = "element", target_id = "C-404")),
               class = "modelbench_refused")
  expect_error(write_link(con, fields = list(link_type = "traces_to", source_kind = "requirement", source_id = "SYS-404",
                                             target_kind = "element", target_id = a)),
               class = "modelbench_refused")
})

test_that("a delete writes final revisions for everything that goes with it", {
  con <- local_project()
  write_property_def(con, "PD-1", list(name = "mass", label = "Mass", value_type = "number"))
  a <- new_element(con, "a")$id
  b <- new_element(con, "b")$id
  write_interface(con, "IF-1", list(name = "Data", flow_type = "data"))
  out <- write_port(con, fields = list(element_id = a, name = "o", direction = "out", interface_id = "IF-1"))$id
  inp <- write_port(con, fields = list(element_id = b, name = "i", direction = "in", interface_id = "IF-1"))$id
  connector <- write_connector(con, fields = list(source_port_id = out, target_port_id = inp))$id
  value <- write_property_value(con, fields = list(owner_kind = "element", owner_id = a, property_def_id = "PD-1"))$id
  link <- write_link(con, fields = list(link_type = "traces_to", source_kind = "element", source_id = a,
                                        target_kind = "element", target_id = b))$id

  result <- delete_entity(con, "element", a, note = "Removed")
  deleted <- DBI::dbGetQuery(con, "SELECT entity_id, change_set FROM revision WHERE deleted = 1")
  expect_setequal(deleted$entity_id, c(a, out, connector, value, link))
  expect_identical(unique(deleted$change_set), result$change_set)
  expect_null(read_entity(con, "port", out))
  expect_null(read_entity(con, "connector", connector))
  expect_false(is.null(read_entity(con, "element", b)))
  expect_false(is.null(read_entity(con, "port", inp)))
})

test_that("a re-created entity continues its revision history", {
  con <- local_project()
  write_element(con, "C-1", list(kind = "component", name = "a"))
  delete_entity(con, "element", "C-1")
  expect_identical(write_element(con, "C-1", list(kind = "component", name = "a"))$rev, 3L)
})

test_that("deletes that would break the model are refused", {
  con <- local_project()
  a <- new_element(con, "a")$id
  new_element(con, "b", parent_id = a)
  expect_error(delete_entity(con, "element", a), "constraint", ignore.case = TRUE)
  expect_false(is.null(read_entity(con, "element", a)))
  expect_error(delete_entity(con, "element", "C-404"), class = "modelbench_refused")
})

test_that("composite keys work through the write path", {
  con <- local_project()
  a <- new_element(con)$id
  write_state_machine(con, "SM-1", list(owner_element_id = a, name = "m"))
  write_sm_state(con, "S-1", list(sm_id = "SM-1", name = "st"))
  write_sm_event(con, "E-1", list(sm_id = "SM-1", name = "ev"))
  created <- write_sm_ignore(con, "S-1|E-1", list(rationale = "Not reachable"))
  expect_identical(created$rev, 1L)
  expect_identical(write_sm_ignore(con, "S-1|E-1", list(rationale = "Still not"))$rev, 2L)
  expect_identical(read_entity_rev(con, "sm_ignore", "S-1|E-1", 1L)$rationale, "Not reachable")
  expect_error(write_sm_ignore(con, fields = list(state_id = "S-1")), "explicit id")
  delete_entity(con, "sm_state", "S-1")
  expect_null(read_entity(con, "sm_ignore", "S-1|E-1"))
})

test_that("model_hash changes with any revision and ignores row order", {
  con <- local_project()
  empty <- model_hash(con)
  write_element(con, "C-1", list(kind = "component", name = "a"))
  once <- model_hash(con)
  expect_false(identical(empty, once))
  expect_identical(model_hash(con), once)
  write_element(con, "C-1", list(name = "b"))
  expect_false(identical(model_hash(con), once))
})

test_that("write_plain only touches plain tables", {
  con <- local_project()
  expect_error(write_plain(con, "element", list(id = "C-1")), "isn't a plain table")
  expect_no_error(write_plain(con, "project_meta", list(key = "project_name", value = "x")))
})
