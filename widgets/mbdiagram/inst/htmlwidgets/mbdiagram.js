// Block diagram editing. Events go to Shiny as `<id>_event`; the server decides what
// they mean and sends back new data, so a refused edit simply isn't there on redraw.
HTMLWidgets.widget({
  name: "mbdiagram",
  type: "output",
  factory: function (el) {
    var dia = joint.dia, shapes = joint.shapes;
    var INK = "#1C1D1F", LINE = "#9A9DA3", AI = "#2F5BD3", AI_TINT = "#EAF0FD", AI_TEXT = "#1E3F9E",
        GAP = "#B35A00", GAP_TINT = "#FFF3E3", GAP_TEXT = "#8A4500", EDGE = "#6B6E74", CANVAS = "#FAFAF9";
    var MONO = "IBM Plex Mono, ui-monospace, monospace", SANS = "IBM Plex Sans, system-ui, sans-serif";
    var FLOW_STYLES = { data: { stroke: "#45484D", width: 2 }, energy: { stroke: INK, width: 4 }, matter: { stroke: LINE, width: 2, dash: "6 4" } };
    var BLOCK_W = 180, BLOCK_H = 76, BOUNDARY_W = 110, BOUNDARY_H = 34;

    function portGroup(position) {
      return {
        position: { name: position },
        attrs: { portBody: { magnet: true, width: 10, height: 10, x: -5, y: -5, fill: "#FFFFFF", stroke: INK, strokeWidth: 2, cursor: "crosshair" } },
        markup: [{ tagName: "rect", selector: "portBody" }]
      };
    }
    var Block = dia.Element.define("mb.Block", {
      size: { width: BLOCK_W, height: BLOCK_H },
      attrs: {
        body: { width: "calc(w)", height: "calc(h)", fill: "#FFFFFF", stroke: LINE, strokeWidth: 1, rx: 4, ry: 4 },
        stereotype: { x: "calc(0.5*w)", y: 22, textAnchor: "middle", fontFamily: MONO, fontSize: 11, fill: "#5B5E63" },
        name: { x: "calc(0.5*w)", y: 44, textAnchor: "middle", fontFamily: SANS, fontSize: 13, fontWeight: 600, fill: INK, textWrap: { width: -16, maxLineCount: 2, ellipsis: true } },
        note: { x: "calc(0.5*w)", y: "calc(h+16)", textAnchor: "middle", fontFamily: SANS, fontSize: 12, fill: GAP_TEXT }
      },
      ports: { groups: { top: portGroup("top"), right: portGroup("right"), bottom: portGroup("bottom"), left: portGroup("left") } }
    }, {
      markup: [{ tagName: "rect", selector: "body" }, { tagName: "text", selector: "stereotype" }, { tagName: "text", selector: "name" }, { tagName: "text", selector: "note" }]
    });
    // Decisions in an activity: same attribute names as a block, drawn as a diamond.
    var Diamond = Block.define("mb.Diamond", {
      attrs: { body: { refPoints: "0,10 10,0 20,10 10,20", fill: "#E3E4E6", stroke: "none", width: null, height: null, rx: null, ry: null } }
    }, {
      markup: [{ tagName: "polygon", selector: "body" }, { tagName: "text", selector: "stereotype" }, { tagName: "text", selector: "name" }, { tagName: "text", selector: "note" }]
    });
    var SHAPES = {
      state: { width: 160, height: 60, rx: 20 }, action: { width: 190, height: 60, rx: 16 }, decision: { width: 210, height: 100 },
      merge: { width: 44, height: 44 }, initial: { width: 26, height: 26, rx: 13, solid: true }, final: { width: 26, height: 26, rx: 13, solid: true },
      fork: { width: 130, height: 10, rx: 2, solid: true }, join: { width: 130, height: 10, rx: 2, solid: true }
    };
    var namespace = { mb: { Block: Block, Diamond: Diamond }, standard: shapes.standard };

    var graph = new dia.Graph({}, { cellNamespace: namespace });
    var holder = document.createElement("div");
    holder.style.cssText = "width:100%;height:100%;overflow:auto;background:" + CANVAS + ";outline-offset:-2px";
    holder.tabIndex = 0;
    holder.setAttribute("aria-label", "Block diagram. Arrow keys move between connected blocks.");
    var mount = document.createElement("div");
    holder.appendChild(mount);
    el.appendChild(holder);

    var paper = new dia.Paper({
      el: mount, model: graph, width: 1200, height: 800, gridSize: 8, drawGrid: { name: "dot", args: { color: "#D4D5D8", scaleFactor: 2 } },
      background: { color: CANVAS }, async: false, sorting: dia.Paper.sorting.NONE, cellViewNamespace: namespace,
      linkPinning: false, snapLinks: { radius: 16 }, markAvailable: false,
      defaultLink: function () { return newLink(); },
      validateConnection: function (a, ma, b, mb) { return !!mb && (a !== b || behaviourMode()); },
      validateMagnet: function () { return tool !== "select_only"; },
      interactive: function (view) { return view.model.isLink() ? { linkMove: false, labelMove: false, arrowheadMove: false, vertexMove: false, vertexAdd: false, useLinkTools: false } : { elementMove: tool === "select" }; }
    });

    var data = null, tool = "select", selected = [], painted = [], level = null, elk = new ELK(), inputBase = el.id;

    function newLink() {
      return new shapes.standard.Link({ z: 1, attrs: { line: { stroke: EDGE, strokeWidth: 2, targetMarker: { type: "path", d: "M 8 -4 L 0 0 L 8 4 z", fill: EDGE, stroke: EDGE } } }, connector: { name: "rounded", args: { radius: 6 } } });
    }
    function send(type, payload) {
      if (!window.Shiny || !Shiny.setInputValue) return;
      Shiny.setInputValue(inputBase + "_event", Object.assign({ type: type, n: Math.random() }, payload || {}), { priority: "event" });
    }

    function portSide(port) {
      if (port.side) return port.side;
      return port.direction === "in" ? "top" : "bottom";
    }
    function portItem(port) {
      var gap = port.unconnected && port.review_gap !== false;
      return {
        id: port.id, group: portSide(port),
        attrs: { portBody: gap ? { fill: GAP_TINT, stroke: GAP } : {}, root: { title: (port.name || "") + " · " + port.direction + (port.interface ? " · " + port.interface : "") } }
      };
    }
    function blockSize(ports) {
      var sides = { top: 0, bottom: 0, left: 0, right: 0 };
      ports.forEach(function (p) { sides[portSide(p)]++; });
      return { width: Math.max(BLOCK_W, 28 * Math.max(sides.top, sides.bottom) + 40), height: Math.max(BLOCK_H, 24 * Math.max(sides.left, sides.right) + 24) };
    }
    // Behaviour nodes connect by four handles, one on each side.
    function handles(node) {
      if (["initial", "final", "fork", "join"].indexOf(node.shape) >= 0 && data.mode === "activity") return [];
      return ["top", "right", "bottom", "left"].map(function (side) { return { id: node.id + ":" + side, group: side, attrs: { portBody: { width: 8, height: 8, x: -4, y: -4, fill: CANVAS, stroke: LINE, strokeWidth: 1 } } }; });
    }
    function shapeAttrs(node) {
      var look = SHAPES[node.shape] || SHAPES.action;
      var a = { stereotype: { text: node.initial ? "start" : (node.final ? "final" : ""), y: 13, fontSize: 10 }, name: { text: node.name, y: "calc(0.5*h)", textVerticalAnchor: "middle", fontWeight: 500, textWrap: { width: node.shape === "decision" ? -70 : -16, maxLineCount: 3, ellipsis: true } }, note: { text: node.gap_note || "", textWrap: null, fontFamily: MONO, fontWeight: 600 } };
      if (node.shape !== "decision" && node.shape !== "merge") a.body = { rx: look.rx || 4, ry: look.rx || 4, stroke: LINE, strokeWidth: 1, fill: "#FFFFFF" };
      if (look.solid) a.body = { rx: look.rx, ry: look.rx, fill: INK, stroke: INK };
      if (node.shape === "decision" || node.shape === "merge") a.body = { fill: "#E3E4E6" };
      if (node.initial) a.body.strokeWidth = 2;
      if (node.review_state === "unreviewed") {
        a.body = Object.assign(a.body, { stroke: AI, strokeWidth: 2, strokeDasharray: "6 6", fill: AI_TINT });
        a.name.fill = AI_TEXT;
      } else if (node.gap) {
        a.body = Object.assign(a.body, { stroke: GAP, strokeWidth: 2, strokeDasharray: "6 4", fill: GAP_TINT });
      }
      if (data.current != null && data.current === node.id) {
        a.body = Object.assign(a.body, { fill: INK, stroke: INK, strokeDasharray: null });
        a.name.fill = "#FFFFFF";
        a.stereotype.fill = "#FFFFFF";
      } else {
        if (!a.name.fill) a.name.fill = INK;
        a.stereotype.fill = "#5B5E63";
      }
      return a;
    }
    // Transitions between the same two states would draw on top of each other, so each
    // bows out by its own amount. Self-transitions loop off the right-hand side.
    function spreadParallel() {
      if (!behaviourMode()) return;
      var groups = {}, labelSpots = [];
      graph.getLinks().forEach(function (link) {
        var a = String(link.source().id), b = String(link.target().id);
        var key = a < b ? a + "|" + b : b + "|" + a;
        (groups[key] = groups[key] || []).push(link);
      });
      Object.keys(groups).forEach(function (key) {
        var links = groups[key];
        links.forEach(function (link, i) {
          var s = graph.getCell(link.source().id), t = graph.getCell(link.target().id);
          if (!s || !t) return;
          if (s === t) {
            var box = s.getBBox(), reach = 46 + i * 26;
            link.vertices([{ x: box.x + box.width + reach, y: box.y + 8 }, { x: box.x + box.width + reach, y: box.y + box.height - 8 }]);
            return;
          }
          var from = s.getBBox().center(), to = t.getBBox().center();
          // Measured from one fixed direction, so opposite transitions don't cancel out.
          var flip = String(link.source().id) < String(link.target().id) ? 1 : -1;
          var dx = (to.x - from.x) * flip, dy = (to.y - from.y) * flip, len = Math.sqrt(dx * dx + dy * dy) || 1;
          var base = (i - (links.length - 1) / 2) * 90;
          var others = graph.getElements().filter(function (e) { return e !== s && e !== t; }).map(function (e) { return e.getBBox().inflate(14); });
          var label = link.labels()[0], half = label ? String(label.attrs.text.text).length * 3.7 : 0;
          var tries = [0, 100, -100, 200, -200, 300, -300, 400, -400];
          for (var k = 0; k < tries.length; k++) {
            var bow = base + tries[k];
            var mid = { x: Math.round((from.x + to.x) / 2 - dy / len * bow), y: Math.round((from.y + to.y) / 2 + dx / len * bow) };
            var hits = others.some(function (box) {
              return box.intersectionWithLine(new joint.g.Line(from, mid)) || box.intersectionWithLine(new joint.g.Line(mid, to)) || (Math.abs(mid.x - box.center().x) < box.width / 2 + half && Math.abs(mid.y - box.center().y) < box.height / 2 + 10);
            });
            if (mid.x < half + 12 || mid.y < 16) hits = true;
            var crowded = labelSpots.some(function (p) { return Math.abs(p.x - mid.x) < p.half + half + 8 && Math.abs(p.y - mid.y) < 22; });
            if ((!hits && !crowded) || k === tries.length - 1) {
              labelSpots.push({ x: mid.x, y: mid.y, half: half });
              link.vertices(bow === 0 ? [] : [mid]);
              break;
            }
          }
        });
      });
    }
    function behaviourMode() { return data && (data.mode === "state_machine" || data.mode === "activity"); }
    function blockAttrs(node) {
      if (behaviourMode()) return shapeAttrs(node);
      var kicker = data && data.mode === "functional" ? node.id : (node.stereotype ? "«" + node.stereotype + "»" : "") + (node.multiplicity > 1 ? " ×" + node.multiplicity : "");
      var a = { name: { text: node.name }, stereotype: { text: kicker }, note: { text: node.gap_note ? "Gap: " + node.gap_note : "" } };
      if (node.allocation) a.note = { text: node.allocation, fill: node.unallocated ? GAP_TEXT : "#5B5E63" };
      if (node.review_state === "unreviewed") {
        a.body = { stroke: AI, strokeWidth: 2, strokeDasharray: "6 6", fill: AI_TINT };
        a.name.fill = AI_TEXT;
      } else if (node.gap) {
        a.body = { stroke: GAP, strokeWidth: 2, strokeDasharray: "6 4" };
      }
      return a;
    }

    function build(x) {
      data = x;
      var blocks = x.nodes.map(function (node) {
        if (behaviourMode()) {
          var look = SHAPES[node.shape] || SHAPES.action;
          var Kind = node.shape === "decision" || node.shape === "merge" ? Diamond : Block;
          return new Kind({ id: node.id, z: 2, position: { x: node.x == null ? 0 : node.x, y: node.y == null ? 0 : node.y }, size: { width: look.width, height: look.height }, attrs: blockAttrs(node), ports: { items: handles(node) } });
        }
        var ports = x.ports.filter(function (p) { return p.element_id === node.id; });
        var size = blockSize(ports);
        return new Block({ id: node.id, z: 2, position: { x: node.x == null ? 0 : node.x, y: node.y == null ? 0 : node.y }, size: size, attrs: blockAttrs(node), ports: { items: ports.map(portItem) } });
      });
      var boundary = (x.boundary_ports || []).map(function (port, i) {
        // A parent's port seen from inside: its in ports feed the level, its out ports leave it.
        var incoming = port.direction === "in";
        return new Block({
          id: "B:" + port.id, z: 2, size: { width: BOUNDARY_W, height: BOUNDARY_H },
          position: { x: incoming ? 8 : paperWidth(x) - BOUNDARY_W - 16, y: 8 + i * 48 },
          attrs: { body: { fill: "#ECEDEF", stroke: LINE, strokeDasharray: "3 3" }, stereotype: { text: "" }, name: { text: port.name, y: 21, fontFamily: MONO, fontWeight: 400, fontSize: 12 }, note: { text: "" } },
          ports: { items: [{ id: port.id, group: incoming ? "right" : "left" }] }
        });
      });
      var byId = {};
      blocks.concat(boundary).forEach(function (b) { byId[b.id] = b; });
      function end(portId, holderId) {
        if (holderId == null) return null;
        if (byId[holderId] && byId[holderId].hasPort(portId)) return { id: holderId, port: portId };
        if (byId["B:" + portId]) return { id: "B:" + portId, port: portId };
        return byId[holderId] ? { id: holderId } : null;
      }
      var links = [];
      x.edges.forEach(function (edge) {
        var s = end(edge.source_port_id, edge.source_holder), t = end(edge.target_port_id, edge.target_holder);
        if (!s && !t) return;
        var link = newLink();
        link.set("id", edge.id);
        link.set("mbStub", !!edge.stub);
        link.source(s || { x: 0, y: 0 });
        link.target(t || { x: 0, y: 0 });
        if (edge.review_state === "unreviewed") link.attr("line", { stroke: AI, strokeDasharray: "6 6" });
        if (edge.stub) link.attr("line/strokeDasharray", "4 4");
        if (edge.gap) link.attr("line", { stroke: GAP, strokeDasharray: "6 4", targetMarker: { type: "path", d: "M 8 -4 L 0 0 L 8 4 z", fill: GAP, stroke: GAP } });
        if (x.mode === "flows") {
          var style = FLOW_STYLES[edge.flow_type];
          if (style) {
            link.attr("line", { stroke: style.stroke, strokeWidth: style.width, strokeDasharray: style.dash || null, targetMarker: { type: "path", d: "M 8 -4 L 0 0 L 8 4 z", fill: style.stroke, stroke: style.stroke } });
            link.set("mbWidth", style.width);
          }
        }
        if (edge.label) link.labels([{ attrs: { text: { text: edge.label, fontFamily: MONO, fontSize: 12, fill: "#45484D" }, rect: { fill: CANVAS, stroke: "none" } }, position: 0.5 }]);
        links.push(link);
      });
      var sameLevel = level === x.root;
      var scrollX = sameLevel ? holder.scrollLeft : 0, scrollY = sameLevel ? holder.scrollTop : 0;
      level = x.root;
      painted = [];
      // Links go in first, so they sit below the blocks and can't take a drag meant for a
      // port. Sorting by z does the same, but takes 600 ms at 150 components.
      graph.resetCells(links.concat(blocks).concat(boundary));
      placeStubs(links);
      drawBand();
      spreadParallel();
      fit();
      applySelection();
      holder.scrollLeft = scrollX;
      holder.scrollTop = scrollY;
      if (!sameLevel) showBlocks();
      var unplaced = x.nodes.filter(function (node) { return node.x == null; });
      if (unplaced.length > 0) autoLayout(false);
    }
    // The dashed band at the bottom of Flows, holding what's outside the system.
    function drawBand() {
      if (!data || data.mode !== "flows") return;
      var old = graph.getCell("B:band");
      if (old) old.remove();
      var outside = data.nodes.filter(function (n) { return n.kind === "external"; }).map(function (n) { return graph.getCell(n.id); }).filter(Boolean);
      if (outside.length === 0) return;
      var top = Infinity, bottom = 0;
      outside.forEach(function (b) { top = Math.min(top, b.position().y); bottom = Math.max(bottom, b.position().y + b.size().height); });
      var box = graph.getBBox();
      var band = new Block({
        id: "B:band", position: { x: box.x - 16, y: top - 34 }, size: { width: box.width + 32, height: bottom - top + 50 },
        attrs: { root: { pointerEvents: "none" }, body: { fill: "transparent", stroke: LINE, strokeDasharray: "4 4" },
                 stereotype: { text: "" }, name: { text: "Environment (outside the system)", y: 18, fontWeight: 400, fontSize: 12, fill: "#5B5E63", textWrap: null }, note: { text: "" } }
      });
      graph.addCell(band);
    }
    // A wide layout can start well off to the side, so open where the blocks are.
    function showBlocks() {
      var first = null;
      graph.getElements().forEach(function (b) {
        if (String(b.id).indexOf("B:") === 0) return;
        if (!first || b.position().y < first.position().y) first = b;
      });
      if (!first) return;
      holder.scrollLeft = Math.max(0, first.position().x - 80);
      holder.scrollTop = Math.max(0, first.position().y - 40);
    }
    function paperWidth(x) { return Math.max(el.clientWidth || 600, 500); }
    // A stub runs a short way off its block, towards the view's edge.
    function placeStubs(links) {
      links.forEach(function (link) {
        if (!link.get("mbStub")) return;
        var s = link.source(), t = link.target();
        var known = s.id ? s : t;
        var block = graph.getCell(known.id);
        if (!block) return;
        var box = block.getBBox();
        var point = { x: box.x + box.width / 2, y: box.y + box.height + 70 };
        if (s.id) link.target(point); else link.source(point);
      });
    }
    function fit() {
      var box = graph.getBBox();
      var w = Math.max(el.clientWidth || 800, box ? box.x + box.width + 80 : 0);
      var h = Math.max(el.clientHeight || 500, box ? box.y + box.height + 110 : 0);
      paper.setDimensions(w, h);
    }

    // Touches only the cells whose state changed: a full pass redraws every link.
    function paintSelected(cell, on) {
      if (cell.isLink()) {
        var width = cell.get("mbWidth") || 2;
        cell.attr("line/strokeWidth", on ? width + 1 : width);
        return;
      }
      var node = data && data.nodes.filter(function (n) { return n.id === cell.id; })[0];
      var base = node ? blockAttrs(node).body || {} : {};
      cell.attr("body/stroke", on ? INK : (base.stroke || LINE));
      cell.attr("body/strokeWidth", on ? 2 : (base.strokeWidth || 1));
      if (on) cell.removeAttr("body/strokeDasharray"); else if (base.strokeDasharray) cell.attr("body/strokeDasharray", base.strokeDasharray);
    }
    function applySelection() {
      painted.filter(function (id) { return selected.indexOf(id) < 0; }).forEach(function (id) {
        var cell = graph.getCell(id);
        if (cell) paintSelected(cell, false);
      });
      selected.filter(function (id) { return painted.indexOf(id) < 0; }).forEach(function (id) {
        var cell = graph.getCell(id);
        if (cell) paintSelected(cell, true);
      });
      painted = selected.slice();
    }
    function select(ids, kind) {
      selected = ids;
      applySelection();
      send("select", { ids: ids, kind: kind });
    }

    paper.on("element:pointerclick", function (view, evt) {
      var id = String(view.model.id);
      if (id.indexOf("B:") === 0) { select([id.slice(2)], "port"); return; }
      if (tool === "port") {
        var box = view.model.getBBox(), local = paper.clientToLocalPoint(evt.clientX, evt.clientY);
        var dx = Math.min(local.x - box.x, box.x + box.width - local.x), dy = Math.min(local.y - box.y, box.y + box.height - local.y);
        var side = dx < dy ? (local.x - box.x < box.width / 2 ? "left" : "right") : (local.y - box.y < box.height / 2 ? "top" : "bottom");
        send("add_port", { element: id, side: side });
        return;
      }
      if (evt.shiftKey) {
        var at = selected.indexOf(id);
        var next = at >= 0 ? selected.filter(function (s) { return s !== id; }) : selected.concat([id]);
        select(next, "element");
      } else {
        select([id], "element");
      }
    });
    paper.on("element:pointerdblclick", function (view) {
      var id = String(view.model.id);
      if (id.indexOf("B:") !== 0) send("open", { id: id });
    });
    paper.on("link:pointerclick", function (view) { select([String(view.model.id)], "connector"); });
    paper.on("blank:pointerclick", function (evt, x, y) {
      if (tool === "component") { send("add_component", { x: x, y: y }); return; }
      select([], "none");
    });
    paper.on("element:pointerup", function (view) {
      var model = view.model, id = String(model.id);
      if (id.indexOf("B:") === 0) return;
      var before = model.get("mbLast"), p = model.position();
      model.set("mbLast", { x: p.x, y: p.y }, { silent: true });
      if (before && before.x === p.x && before.y === p.y) return;
      spreadParallel();
      fit();
      send("move", { id: id, x: p.x, y: p.y });
    });
    paper.on("element:pointerdown", function (view) {
      var p = view.model.position();
      view.model.set("mbLast", { x: p.x, y: p.y }, { silent: true });
    });
    paper.on("link:connect", function (view) {
      var s = view.model.source(), t = view.model.target();
      send("connect", { source: s.port, target: t.port, source_id: String(s.id), target_id: t.id == null ? null : String(t.id) });
    });

    function connectedBlocks(id) {
      return graph.getNeighbors(graph.getCell(id)).map(function (c) { return String(c.id); });
    }
    holder.addEventListener("keydown", function (evt) {
      if (["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown"].indexOf(evt.key) < 0 && evt.key !== "Enter") return;
      var blocks = graph.getElements();
      if (blocks.length === 0) return;
      evt.preventDefault();
      var current = selected[0] && graph.getCell(selected[0]) && graph.getCell(selected[0]).isElement() ? selected[0] : null;
      if (evt.key === "Enter") { if (current) send("open", { id: current }); return; }
      if (!current) { select([String(blocks[0].id)], "element"); return; }
      var here = graph.getCell(current).getBBox().center();
      var wanted = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] }[evt.key];
      var best = null, bestScore = -Infinity;
      connectedBlocks(current).forEach(function (id) {
        var c = graph.getCell(id).getBBox().center();
        var dx = c.x - here.x, dy = c.y - here.y, len = Math.sqrt(dx * dx + dy * dy) || 1;
        var score = (dx * wanted[0] + dy * wanted[1]) / len;
        if (score > bestScore) { bestScore = score; best = id; }
      });
      if (best && bestScore > 0 && best.indexOf("B:") !== 0) select([best], "element");
    });

    // Runs ELK layered with port sides. Pinned blocks keep their positions.
    function autoLayout(userAsked) {
      if (!data) return;
      var band = graph.getCell("B:band");
      if (band) band.remove();
      var blocks = graph.getElements().filter(function (b) { return String(b.id).indexOf("B:") !== 0; });
      var known = {};
      blocks.forEach(function (b) { known[b.id] = true; });
      var graphDef = {
        id: "root",
        layoutOptions: { "elk.algorithm": "layered", "elk.direction": "DOWN", "elk.spacing.nodeNode": behaviourMode() ? "150" : "48", "elk.layered.spacing.nodeNodeBetweenLayers": behaviourMode() ? "120" : "80", "elk.padding": "[top=48,left=120,bottom=48,right=120]", "elk.portConstraints": "FIXED_SIDE" },
        children: blocks.map(function (b) {
          var size = b.size();
          return { id: String(b.id), width: size.width, height: size.height, layoutOptions: { "elk.portConstraints": "FIXED_SIDE" },
            ports: b.getPorts().map(function (p) { return { id: p.id, width: 10, height: 10, layoutOptions: { "elk.port.side": { top: "NORTH", right: "EAST", bottom: "SOUTH", left: "WEST" }[p.group] } }; }) };
        }),
        edges: graph.getLinks().filter(function (l) {
          var s = l.source(), t = l.target();
          return s.id && t.id && known[s.id] && known[t.id];
        }).map(function (l) {
          var s = l.source(), t = l.target();
          return { id: String(l.id), sources: [s.port || s.id], targets: [t.port || t.id] };
        })
      };
      return elk.layout(graphDef).then(function (out) {
        var positions = [];
        var pinned = {};
        data.nodes.forEach(function (n) { if (n.pinned) pinned[n.id] = true; });
        graph.startBatch("layout");
        out.children.forEach(function (c) {
          var block = graph.getCell(c.id);
          if (pinned[c.id]) return;
          var x = Math.round(c.x), y = Math.round(c.y);
          block.position(x, y);
          positions.push({ id: c.id, x: x, y: y });
        });
        // What's outside the system sits in a row under everything inside it.
        var outside = data.nodes.filter(function (n) { return n.kind === "external" && !pinned[n.id]; });
        if (outside.length > 0) {
          var deepest = 0;
          blocks.forEach(function (b) { if (!outside.some(function (n) { return n.id === b.id; })) deepest = Math.max(deepest, b.position().y + b.size().height); });
          outside.forEach(function (n, i) {
            var cell = graph.getCell(n.id), x = 120 + i * 240, y = deepest + 110;
            cell.position(x, y);
            positions = positions.filter(function (p) { return p.id !== n.id; }).concat([{ id: n.id, x: x, y: y }]);
          });
        }
        graph.stopBatch("layout");
        placeStubs(graph.getLinks());
        drawBand();
        spreadParallel();
        fit();
        showBlocks();
        send("layout", { positions: positions, user: !!userAsked });
      });
    }

    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler("mbdiagram-" + el.id, function (msg) {
        if (msg.type === "set_tool") { tool = msg.payload.tool; holder.style.cursor = tool === "select" ? "" : "crosshair"; }
        if (msg.type === "set_selection") { selected = msg.payload.ids || []; applySelection(); }
        if (msg.type === "auto_layout") autoLayout(true);
        if (msg.type === "set_current" && data) {
          var was = data.current;
          data.current = msg.payload.id == null ? null : msg.payload.id;
          data.nodes.forEach(function (n) {
            if (n.id !== was && n.id !== data.current) return;
            var cell = graph.getCell(n.id);
            if (cell) cell.attr(blockAttrs(n));
          });
        }
      });
    }
    window.mbdiagramDebug = window.mbdiagramDebug || {};
    window.mbdiagramDebug[el.id] = { graph: graph, paper: paper };

    return {
      renderValue: function (x) {
        var t0 = performance.now();
        var parsed = JSON.parse(x.json);
        if (parsed.tool) tool = parsed.tool;
        selected = parsed.selected || [];
        build(parsed);
        el.setAttribute("data-render-ms", String(Math.round(performance.now() - t0)));
      },
      resize: function () { if (data) fit(); }
    };
  }
});
