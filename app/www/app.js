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
