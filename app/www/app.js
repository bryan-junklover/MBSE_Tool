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

// The docked chat. Its state is a class on the body, so the layout can make room. Below
// 1600 px the left columns fold away while it's open, so the canvas and inspector stay.
function mbToggleChat(open) {
  var on = typeof open === "boolean" ? open : !document.body.classList.contains("mb-chat-open");
  document.body.classList.toggle("mb-chat-open", on);
  var toggle = document.querySelector(".mb-chat-toggle");
  if (toggle) toggle.setAttribute("aria-expanded", String(on));
  if (window.mbColumns) window.mbColumns.chat(on);
  if (on) { var box = document.querySelector("#mb-chat textarea"); if (box) box.focus(); }
}

// Side columns (UX review §1.4–1.6). Each aside with data-col gets a splitter to resize it
// and a rail to bring it back once hidden. Widths and hidden state are kept per column.
// Up to 1100 px the columns become overlays opened from their rails.
(function () {
  var NARROW = 1100, MIN = 180, MAX = 680;
  var columns = [];
  function narrow() { return window.innerWidth <= NARROW; }
  function load(key) { try { return JSON.parse(localStorage.getItem("modelbench.col." + key)) || {}; } catch (e) { return {}; } }
  function save(key, value) { try { localStorage.setItem("modelbench.col." + key, JSON.stringify(value)); } catch (e) { /* no storage */ } }
  function apply(col) {
    var closed = col.mbClosed, nar = narrow();
    col.classList.toggle("mb-col-closed", closed);
    col.classList.toggle("mb-col-overlay", nar && !closed);
    col.mbRail.hidden = !(closed || nar);
    col.mbRail.setAttribute("aria-expanded", String(!closed));
    col.mbSplit.hidden = closed || nar;
  }
  function remember(col) { save(col.dataset.col, { w: col.mbWidth, closed: col.dataset.auto ? undefined : col.mbClosed }); }
  function setClosed(col, closed, byUser) {
    col.mbClosed = closed;
    if (byUser) col.mbChatClosed = false;
    if (byUser && !narrow()) remember(col);
    apply(col);
    window.dispatchEvent(new Event("resize"));
  }
  function enhance(col) {
    if (col.mbEnhanced) return;
    col.mbEnhanced = true;
    var left = col.classList.contains("mb-left"), label = col.dataset.label || (left ? "Panel" : "Inspector");
    var saved = load(col.dataset.col);
    col.mbWidth = Math.max(MIN, Math.min(MAX, saved.w || +col.dataset.width || (left ? 260 : 340)));
    col.style.setProperty("--col-w", col.mbWidth + "px");
    col.mbClosed = narrow() || (col.dataset.auto ? true : (saved.closed != null ? saved.closed : col.dataset.closed === "true"));
    var rail = document.createElement("button");
    rail.type = "button";
    rail.className = "mb-rail " + (left ? "mb-rail-left" : "mb-rail-right");
    rail.textContent = label;
    rail.setAttribute("aria-label", "Show " + label.toLowerCase());
    rail.addEventListener("click", function () { setClosed(col, !col.mbClosed, true); });
    var split = document.createElement("div");
    split.className = "mb-split";
    split.setAttribute("role", "separator");
    split.setAttribute("aria-orientation", "vertical");
    split.setAttribute("aria-label", "Resize " + label.toLowerCase());
    var hide = document.createElement("button");
    hide.type = "button";
    hide.textContent = left ? "‹" : "›";
    hide.setAttribute("aria-label", "Hide " + label.toLowerCase());
    hide.title = "Hide " + label.toLowerCase();
    hide.addEventListener("click", function (evt) { evt.stopPropagation(); setClosed(col, true, true); });
    split.appendChild(hide);
    var parent = col.parentNode;
    if (left) { parent.insertBefore(rail, col); parent.insertBefore(split, col.nextSibling); }
    else { parent.insertBefore(split, col); parent.insertBefore(rail, col.nextSibling); }
    col.mbRail = rail;
    col.mbSplit = split;
    split.addEventListener("pointerdown", function (evt) {
      if (evt.target === hide) return;
      evt.preventDefault();
      var startX = evt.clientX, startW = col.mbWidth;
      split.classList.add("mb-dragging");
      split.setPointerCapture(evt.pointerId);
      function move(e) {
        var dx = e.clientX - startX;
        col.mbWidth = Math.max(MIN, Math.min(MAX, startW + (left ? dx : -dx)));
        col.style.setProperty("--col-w", col.mbWidth + "px");
      }
      function up() {
        split.classList.remove("mb-dragging");
        split.removeEventListener("pointermove", move);
        split.removeEventListener("pointerup", up);
        remember(col);
        window.dispatchEvent(new Event("resize"));
      }
      split.addEventListener("pointermove", move);
      split.addEventListener("pointerup", up);
    });
    split.addEventListener("dblclick", function () { setClosed(col, true, true); });
    columns.push(col);
    apply(col);
  }
  function scan() { document.querySelectorAll("aside[data-col]").forEach(enhance); }
  var wasNarrow = narrow();
  window.addEventListener("resize", function () {
    var now = narrow();
    if (now === wasNarrow) return;
    wasNarrow = now;
    columns.forEach(function (col) {
      var saved = load(col.dataset.col);
      col.mbClosed = now ? true : (col.dataset.auto ? col.mbAutoOpen !== true : (saved.closed != null ? saved.closed : col.dataset.closed === "true"));
      apply(col);
    });
  });
  // Esc closes an overlay column.
  document.addEventListener("keydown", function (evt) {
    if (evt.key !== "Escape" || !narrow()) return;
    columns.forEach(function (col) { if (!col.mbClosed) setClosed(col, true, false); });
  });
  window.mbColumns = {
    // A selection opens the inspector when it starts closed (auto) or is an overlay.
    reveal: function (anchorId, side, open) {
      var anchor = document.getElementById(anchorId);
      var body = anchor && anchor.closest(".mb-body");
      if (!body) return;
      body.querySelectorAll("aside[data-col].mb-" + side).forEach(function (col) {
        if (col.dataset.auto) col.mbAutoOpen = open;
        if (!col.dataset.auto && !narrow()) return;
        if (col.mbClosed === !open) return;
        setClosed(col, !open, false);
      });
    },
    chat: function (on) {
      columns.forEach(function (col) {
        if (!col.classList.contains("mb-left") || narrow()) return;
        if (on && !col.mbClosed && window.innerWidth < 1600) { col.mbChatClosed = true; setClosed(col, true, false); }
        if (!on && col.mbChatClosed) { col.mbChatClosed = false; setClosed(col, false, false); }
      });
    }
  };
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", scan); else scan();
  $(document).on("shiny:connected", function () {
    scan();
    Shiny.addCustomMessageHandler("mb_column", function (msg) { window.mbColumns.reveal(msg.anchor, msg.side, msg.open); });
  });
})();

// A pane split top and bottom: dragging the bar sets the top pane's height, kept per key.
(function () {
  $(document).on("pointerdown", ".mb-hsplit", function (evt) {
    var bar = this, top = bar.previousElementSibling;
    if (!top) return;
    evt.preventDefault();
    var startY = evt.clientY, startH = top.getBoundingClientRect().height;
    bar.setPointerCapture(evt.pointerId);
    function move(e) { top.style.flexBasis = Math.max(160, startH + e.clientY - startY) + "px"; }
    function up() {
      bar.removeEventListener("pointermove", move);
      bar.removeEventListener("pointerup", up);
      try { localStorage.setItem("modelbench.split." + bar.dataset.key, top.style.flexBasis); } catch (e) { /* no storage */ }
      window.dispatchEvent(new Event("resize"));
    }
    bar.addEventListener("pointermove", move);
    bar.addEventListener("pointerup", up);
  });
  function restore() {
    document.querySelectorAll(".mb-hsplit[data-key]").forEach(function (bar) {
      var saved = null;
      try { saved = localStorage.getItem("modelbench.split." + bar.dataset.key); } catch (e) { saved = null; }
      if (saved && bar.previousElementSibling) bar.previousElementSibling.style.flexBasis = saved;
    });
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", restore); else restore();
})();

// Menus built on <details> close when something else is clicked, or with Esc.
(function () {
  document.addEventListener("click", function (evt) {
    document.querySelectorAll("details.mb-menu[open]").forEach(function (menu) {
      if (!menu.contains(evt.target) || evt.target.closest(".mb-menu-list a, .mb-menu-list button")) menu.removeAttribute("open");
    });
  });
  document.addEventListener("keydown", function (evt) {
    if (evt.key !== "Escape") return;
    document.querySelectorAll("details.mb-menu[open]").forEach(function (menu) { menu.removeAttribute("open"); menu.querySelector("summary").focus(); });
  });
})();

// A confirm step for anything that deletes (§4.2). The button carries the input to send
// and its value; nothing is sent unless the second button is pressed.
function mbConfirm(button) {
  var d = button.dataset;
  var modal = document.createElement("div");
  modal.className = "modal fade";
  modal.tabIndex = -1;
  modal.setAttribute("role", "dialog");
  modal.setAttribute("aria-modal", "true");
  modal.innerHTML = '<div class="modal-dialog"><div class="modal-content"><div class="modal-header"><h4 class="modal-title"></h4></div>' +
    '<div class="modal-body"><p style="margin:0"></p></div><div class="modal-footer">' +
    '<button type="button" class="mb-btn" data-bs-dismiss="modal">Cancel</button>' +
    '<button type="button" class="mb-btn mb-btn-primary"></button></div></div></div>';
  modal.querySelector(".modal-title").textContent = d.title;
  modal.querySelector(".modal-body p").textContent = d.body;
  var go = modal.querySelector(".mb-btn-primary");
  go.textContent = d.confirm;
  document.body.appendChild(modal);
  var dialog = new bootstrap.Modal(modal);
  go.addEventListener("click", function () {
    Shiny.setInputValue(d.input, JSON.parse(d.value), { priority: "event" });
    dialog.hide();
  });
  modal.addEventListener("shown.bs.modal", function () { go.focus(); });
  modal.addEventListener("hidden.bs.modal", function () { modal.remove(); button.focus(); });
  dialog.show();
}

// The traceability matrix highlights the row and column under the pointer (§2.8).
(function () {
  var lit = [];
  function clear() { lit.forEach(function (c) { c.classList.remove("mb-col-hover"); }); lit = []; }
  $(document).on("mouseover", ".mb-matrix td, .mb-matrix th", function () {
    clear();
    var index = this.cellIndex, table = this.closest("table");
    if (index < 1) return;
    Array.prototype.forEach.call(table.rows, function (row) {
      var cell = row.cells[index];
      if (cell) { cell.classList.add("mb-col-hover"); lit.push(cell); }
    });
  });
  $(document).on("mouseleave", ".mb-matrix", clear);
})();
