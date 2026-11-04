// Remembers the last workspace and tab per project. Storage can be unavailable, so
// every access is wrapped and the app works without it.
(function () {
  function key(project) { return "modelbench.nav." + project; }
  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("mb_nav_save", function (msg) {
      try { localStorage.setItem(key(msg.project), JSON.stringify({ workspace: msg.workspace, tab: msg.tab })); } catch (e) { /* no storage */ }
    });
    Shiny.addCustomMessageHandler("mb_nav_restore", function (msg) {
      var saved = null;
      try { saved = JSON.parse(localStorage.getItem(key(msg.project))); } catch (e) { saved = null; }
      Shiny.setInputValue("nav_restored", saved || { workspace: "Write", tab: "Draft" }, { priority: "event" });
    });
  });
})();

// Drag-reordering a table's rows: the new order goes to the input named on the tbody.
(function () {
  var dragged = null;
  $(document).on("dragstart", "tr[data-row-id]", function (evt) {
    dragged = this;
    evt.originalEvent.dataTransfer.effectAllowed = "move";
    evt.originalEvent.dataTransfer.setData("text/plain", this.getAttribute("data-row-id"));
  });
  $(document).on("dragover", "tr[data-row-id]", function (evt) { if (dragged) evt.preventDefault(); });
  $(document).on("drop", "tr[data-row-id]", function (evt) {
    if (!dragged || dragged === this) return;
    evt.preventDefault();
    var body = this.parentNode, rows = Array.prototype.slice.call(body.children);
    body.insertBefore(dragged, rows.indexOf(dragged) < rows.indexOf(this) ? this.nextSibling : this);
    var ids = Array.prototype.map.call(body.children, function (row) { return row.getAttribute("data-row-id"); });
    Shiny.setInputValue(body.getAttribute("data-reorder-input"), { ids: ids, n: Math.random() }, { priority: "event" });
    dragged = null;
  });
  $(document).on("dragend", "tr[data-row-id]", function () { dragged = null; });
})();
