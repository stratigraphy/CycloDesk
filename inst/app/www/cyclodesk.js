// CycloDesk front end: menus, file browser, keyboard shortcuts

// Menus
function cdSend(name, value) {
  Shiny.setInputValue(name, value, { priority: 'event' });
}
$(document).on('click', 'a.cd-act', function (e) {
  e.preventDefault();
  cdSend('menu_action', { action: $(this).data('action'), nonce: Math.random() });
});
$(document).on('click', 'a.cd-place', function (e) {
  e.preventDefault();
  cdSend('nav', { path: $(this).data('path'), nonce: Math.random() });
});

// File browser
var cd = { items: [], sel: [], order: [], anchor: null, focus: null, up: false, filter: '' };

var cdIcons = {
  up: 'fa-solid fa-arrow-turn-up', dir: 'fa-solid fa-folder', drive: 'fa-solid fa-hard-drive', data: 'fa-solid fa-file-lines',
  rds: 'fa-solid fa-box-archive', excel: 'fa-solid fa-file-excel', las: 'fa-solid fa-chart-gantt', code: 'fa-solid fa-file-code', fig: 'fa-solid fa-file-image', other: 'fa-solid fa-file'
};

function cdEsc(s) {
  return String(s).replace(/[&<>"]/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]; });
}

function cdVisible() {
  var f = cd.filter.toLowerCase();
  var out = [];
  if (cd.up && !f) { out.push({ v: 'u|..', n: 'Up one level', t: 'up', i: '' }); }
  cd.items.forEach(function (it) {
    if (!f || it.n.toLowerCase().indexOf(f) >= 0) { out.push(it); }
  });
  return out;
}

function cdRender() {
  var vis = cdVisible();
  var h = '';
  if (!vis.length) {
    h = '<div class="cd-empty">No data files here.<br>Open another folder or drive above.</div>';
  }
  vis.forEach(function (it) {
    var cls = 'cd-row cd-' + it.t + (cd.sel.indexOf(it.v) >= 0 ? ' cd-sel' : '') + (cd.focus === it.v ? ' cd-focus' : '');
    var drag = it.v.indexOf('f|') === 0 ? ' draggable="true"' : '';
    h += '<div class="' + cls + '"' + drag + ' data-v="' + cdEsc(it.v) + '" title="' + cdEsc(it.n + (it.d ? '\n' + it.d + ', ' + it.i : '')) + '">' +
      '<i class="' + cdIcons[it.t] + '"></i><span class="cd-name">' + cdEsc(it.n) + '</span>' +
      ((it.t === 'dir' || it.t === 'drive') ? '<i class="fa-solid fa-chevron-right cd-go"></i>' : '<span class="cd-meta">' + cdEsc(it.i) + '</span>') +
      '</div>';
  });
  $('#cd_browser').html(h);
  var f = $('#cd_browser .cd-focus')[0];
  if (f && f.scrollIntoView) { f.scrollIntoView({ block: 'nearest' }); }
}

function cdSendSel() {
  cd.order = cd.order.filter(function (x) { return cd.sel.indexOf(x) >= 0; });
  cd.sel.forEach(function (x) { if (cd.order.indexOf(x) < 0) { cd.order.push(x); } });
  Shiny.setInputValue('files_order', cd.order.slice());
  Shiny.setInputValue('files', cd.sel.slice());
}

function cdOpen(v) {
  if (v === 'u|..') { cdSend('nav', { path: '..', nonce: Math.random() }); return; }
  if (v.indexOf('d|') === 0) { cdSend('nav', { path: v, nonce: Math.random() }); return; }
  if (v.indexOf('p|') === 0) { cdSend('nav', { path: v, nonce: Math.random() }); return; }
  cdSend('file_dblclick', { value: v, nonce: Math.random() });
}

function cdFiles(list) {
  return list.filter(function (x) { return x.indexOf('f|') === 0; });
}

Shiny.addCustomMessageHandler('cd_listing', function (x) {
  cd.items = x.items || [];
  cd.up = !!x.up;
  cd.sel = cdFiles(x.selected || []);
  cd.focus = cd.sel.length ? cd.sel[0] : null;
  cd.anchor = cd.focus;
  cd.filter = '';
  $('#cd_filter').val('');
  var c = '';
  (x.crumbs || []).forEach(function (k, i) {
    if (i > 0) { c += '<i class="fa-solid fa-chevron-right cd-sep"></i>'; }
    c += '<a href="#" class="cd-crumb' + (i === x.crumbs.length - 1 ? ' cd-here' : '') + '" data-path="' + cdEsc(k.p) + '">' +
      (k.p === '::pc' ? '<i class="fa-solid fa-desktop"></i> ' : (i === 0 ? '<i class="fa-solid fa-hard-drive"></i> ' : '')) + cdEsc(k.l) + '</a>';
  });
  $('#cd_crumbs').html(c);
  var el = document.getElementById('cd_crumbs');
  if (el) { el.scrollLeft = el.scrollWidth; }
  cdRender();
  cdSendSel();
});

$(document).on('click', '#cd_crumbs a.cd-crumb', function (e) {
  e.preventDefault();
  cdSend('nav', { path: $(this).data('path'), nonce: Math.random() });
});

$(document).on('click', '#cd_browser .cd-row', function (e) {
  var v = $(this).data('v');
  $('#cd_browser').focus();
  if (v === 'u|..' || v.indexOf('d|') === 0 || v.indexOf('p|') === 0) { cdOpen(v); return; }
  var vis = cdVisible().map(function (it) { return it.v; }).filter(function (x) { return x.indexOf('f|') === 0; });
  if (e.shiftKey && cd.anchor && vis.indexOf(cd.anchor) >= 0) {
    var a = vis.indexOf(cd.anchor), b = vis.indexOf(v);
    cd.sel = vis.slice(Math.min(a, b), Math.max(a, b) + 1);
  } else if (e.ctrlKey || e.metaKey) {
    var k = cd.sel.indexOf(v);
    if (k >= 0) { cd.sel.splice(k, 1); } else { cd.sel.push(v); }
    cd.anchor = v;
  } else {
    cd.sel = [v];
    cd.anchor = v;
  }
  cd.focus = v;
  cdRender();
  cdSendSel();
});

$(document).on('dblclick', '#cd_browser .cd-row', function () {
  var v = $(this).data('v');
  if (v.indexOf('f|') === 0) { cdOpen(v); }
});

$(document).on('keydown', '#cd_browser', function (e) {
  var vis = cdVisible().map(function (it) { return it.v; });
  if (!vis.length) { return; }
  var i = vis.indexOf(cd.focus);
  if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
    e.preventDefault();
    i = e.key === 'ArrowDown' ? Math.min(vis.length - 1, i + 1) : Math.max(0, i - 1);
    cd.focus = vis[i];
    if (cd.focus.indexOf('f|') === 0) {
      if (e.shiftKey && cd.anchor) {
        var f = vis.filter(function (x) { return x.indexOf('f|') === 0; });
        var a = f.indexOf(cd.anchor), b = f.indexOf(cd.focus);
        cd.sel = f.slice(Math.min(a, b), Math.max(a, b) + 1);
      } else {
        cd.sel = [cd.focus];
        cd.anchor = cd.focus;
      }
      cdSendSel();
    }
    cdRender();
  } else if (e.key === 'Enter' || e.key === 'ArrowRight') {
    if (cd.focus && (cd.focus.indexOf('d|') === 0 || cd.focus.indexOf('p|') === 0 || cd.focus === 'u|..' || e.key === 'Enter')) {
      e.preventDefault();
      cdOpen(cd.focus);
    }
  } else if (e.key === 'Backspace' || e.key === 'ArrowLeft') {
    e.preventDefault();
    cdSend('nav', { path: '..', nonce: Math.random() });
  } else if (e.key === 'F5') {
    e.preventDefault();
    cdSend('menu_action', { action: 'cmd:refresh', nonce: Math.random() });
  } else if (e.key === 'Delete') {
    cdSend('menu_action', { action: 'cmd:delete', nonce: Math.random() });
  } else if (e.key === 'a' && (e.ctrlKey || e.metaKey)) {
    e.preventDefault();
    cd.sel = vis.filter(function (x) { return x.indexOf('f|') === 0; });
    cdRender();
    cdSendSel();
  }
});

$(document).on('input', '#cd_filter', function () {
  cd.filter = $(this).val() || '';
  cdRender();
});
$(document).on('keydown', '#cd_filter', function (e) {
  if (e.key === 'ArrowDown') { e.preventDefault(); $('#cd_browser').focus(); }
  if (e.key === 'Escape') { $(this).val(''); cd.filter = ''; cdRender(); }
});

// Folder toolbar
$(document).on('click', '#cd_up', function () { cdSend('nav', { path: '..', nonce: Math.random() }); });
$(document).on('click', '#cd_refresh', function () {
  cdSend('menu_action', { action: 'cmd:refresh', nonce: Math.random() });
});
$(document).on('click', '#cd_edit', function () {
  $('#cd_pathbox').toggle();
  $('#cd_crumbs').toggle();
  if ($('#cd_pathbox').is(':visible')) { $('#path').focus().select(); }
});
$(document).on('keydown', '#path', function (e) {
  if (e.key === 'Enter') {
    cdSend('path_enter', Math.random());
    $('#cd_pathbox').hide();
    $('#cd_crumbs').show();
  } else if (e.key === 'Escape') {
    $('#cd_pathbox').hide();
    $('#cd_crumbs').show();
  }
});

// Status messages
Shiny.addCustomMessageHandler('cd_busy', function (x) {
  $('#cd_busy').text(x.text).toggleClass('cd-on', !!x.text);
});
Shiny.addCustomMessageHandler('cd_close', function (x) {
  if (!x.keep) { window.close(); }
  var title = x.title || 'CycloDesk is closed';
  var text = x.text || 'R has stopped. You can close this tab.<br>Start CycloDesk again from the desktop shortcut.';
  setTimeout(function () {
    document.body.innerHTML = '<div class="cd-closed"><img src="cyclodesk/cyclodesk.png" width="56" height="56" alt="">' +
      '<h3>' + title + '</h3><p>' + text + '</p></div>';
  }, 300);
});
Shiny.addCustomMessageHandler('cd_open', function (x) {
  window.open(x.url, '_blank', 'noopener');
});
$(document).on('click', '#cd_close', function () {
  cdSend('menu_action', { action: 'cmd:quit', nonce: Math.random() });
});

// Drag and drop: move files onto a folder, the up row, a part of the path or a drive (Ctrl = copy)
var cdDrag = null;
$(document).on('dragstart', '#cd_browser .cd-row[draggable]', function (e) {
  var v = $(this).data('v');
  if (cd.sel.indexOf(v) < 0) {
    cd.sel = [v]; cd.anchor = v; cd.focus = v;
    // update the highlight in place: re-rendering the list here would remove the element being dragged
    $('#cd_browser .cd-row').each(function () {
      var w = $(this).data('v');
      $(this).toggleClass('cd-sel', cd.sel.indexOf(w) >= 0).toggleClass('cd-focus', w === v);
    });
    cdSendSel();
  }
  cdDrag = cd.sel.slice();
  var dt = e.originalEvent.dataTransfer;
  dt.effectAllowed = 'copyMove';
  dt.setData('text/plain', cdDrag.join('\n'));
  var g = document.createElement('div');
  g.className = 'cd-dragghost';
  g.textContent = cdDrag.length === 1 ? cdDrag[0].substring(2) : cdDrag.length + ' files';
  document.body.appendChild(g);
  dt.setDragImage(g, 12, 12);
  setTimeout(function () { document.body.removeChild(g); }, 0);
});
$(document).on('dragend', function () { cdDrag = null; $('.cd-drop').removeClass('cd-drop'); });
var cdTargets = '#cd_browser .cd-row.cd-dir, #cd_browser .cd-row.cd-up, #cd_browser .cd-row.cd-drive, #cd_crumbs a.cd-crumb, a.cd-drv';
function cdTarget(el) {
  var $el = $(el);
  if ($el.hasClass('cd-row')) { var v = $el.data('v'); return v === 'u|..' ? '..' : v; }
  return $el.data('path');
}
$(document).on('dragover', cdTargets, function (e) {
  if (!cdDrag) { return; }
  var t = cdTarget(this);
  if (t === '::pc' || $(this).hasClass('cd-here')) { return; }
  e.preventDefault();
  e.originalEvent.dataTransfer.dropEffect = (e.ctrlKey || e.originalEvent.ctrlKey) ? 'copy' : 'move';
  $(this).addClass('cd-drop');
});
$(document).on('dragleave', cdTargets, function () { $(this).removeClass('cd-drop'); });
$(document).on('drop', cdTargets, function (e) {
  if (!cdDrag) { return; }
  e.preventDefault();
  $(this).removeClass('cd-drop');
  cdSend('move_files', { files: cdDrag, target: cdTarget(this), copy: !!(e.ctrlKey || e.originalEvent.ctrlKey), nonce: Math.random() });
  cdDrag = null;
});

// Two movable vertical lines on the tool figure (interval tools: Select / Remove part, Filtering)
var cdLines = { geom: null, a: null, b: null, labels: ['', ''] };
function cdLinesBox() {
  var img = $('#tool_plot img')[0];
  if (!img || !cdLines.geom) { return null; }
  var W = img.clientWidth, H = img.clientHeight, g = cdLines.geom;
  var x0 = W * (g.fig[0] + (g.fig[1] - g.fig[0]) * g.plt[0]);
  var x1 = W * (g.fig[0] + (g.fig[1] - g.fig[0]) * g.plt[1]);
  var yt = H * (1 - (g.fig[2] + (g.fig[3] - g.fig[2]) * g.plt[3]));
  var yb = H * (1 - (g.fig[2] + (g.fig[3] - g.fig[2]) * g.plt[2]));
  return { x0: x0, x1: x1, yt: yt, yb: yb, left: img.offsetLeft, top: img.offsetTop };
}
function cdX2px(v, bx) {
  var u = cdLines.geom.usr;
  var t = cdLines.geom.xlog ? Math.log10(v) : v;
  return bx.x0 + (t - u[0]) / (u[1] - u[0]) * (bx.x1 - bx.x0);
}
function cdPx2x(px, bx) {
  var u = cdLines.geom.usr;
  var t = u[0] + (px - bx.x0) / (bx.x1 - bx.x0) * (u[1] - u[0]);
  return cdLines.geom.xlog ? Math.pow(10, t) : t;
}
function cdFmt(v) { return Number(v.toPrecision(6)).toString(); }
function cdLinesDraw() {
  var $p = $('#tool_plot');
  $p.find('.cd-vline, .cd-vband').remove();
  var bx = cdLinesBox();
  if (!bx || cdLines.a === null || cdLines.b === null || !isFinite(cdLines.a) || !isFinite(cdLines.b)) { return; }
  $p.css('position', 'relative');
  var pa = cdX2px(cdLines.a, bx), pb = cdX2px(cdLines.b, bx);
  var band = $('<div class="cd-vband"></div>').css({ left: bx.left + Math.min(pa, pb), width: Math.abs(pb - pa),
    top: bx.top + bx.yt, height: bx.yb - bx.yt });
  $p.append(band);
  [['a', pa], ['b', pb]].forEach(function (k, i) {
    var v = cdLines[k[0]];
    var el = $('<div class="cd-vline" data-which="' + k[0] + '"><div class="cd-vgrip"></div><div class="cd-vlab"></div></div>');
    el.css({ left: bx.left + k[1] - 6, top: bx.top + bx.yt, height: bx.yb - bx.yt });
    el.find('.cd-vlab').text(cdFmt(v));
    el.attr('title', (cdLines.labels[i] || '') + ': drag, or type the value in the form');
    $p.append(el);
  });
}
Shiny.addCustomMessageHandler('cd_lines', function (x) {
  if (!x.show) { cdLines.geom = null; $('#tool_plot').find('.cd-vline, .cd-vband').remove(); return; }
  cdLines.geom = x.geom; cdLines.a = x.a; cdLines.b = x.b; cdLines.labels = x.labels || ['', ''];
  setTimeout(cdLinesDraw, 60);
});
Shiny.addCustomMessageHandler('cd_lines_pos', function (x) {
  cdLines.a = x.a; cdLines.b = x.b;
  cdLinesDraw();
});
$(document).on('shiny:value', function (e) { if (e.name === 'tool_plot') { setTimeout(cdLinesDraw, 80); } });
$(window).on('resize', function () { setTimeout(cdLinesDraw, 300); });
var cdDragLine = null;
$(document).on('mousedown', '#tool_plot .cd-vline', function (e) {
  e.preventDefault();
  cdDragLine = $(this).data('which');
  $(this).addClass('cd-active');
});
$(document).on('mousemove', function (e) {
  if (!cdDragLine) { return; }
  var bx = cdLinesBox();
  if (!bx) { return; }
  var img = $('#tool_plot img')[0];
  var px = e.pageX - $(img).offset().left;
  px = Math.max(bx.x0, Math.min(bx.x1, px));
  cdLines[cdDragLine] = cdPx2x(px, bx);
  cdLinesDraw();
  $('#tool_plot .cd-vline[data-which="' + cdDragLine + '"]').addClass('cd-active');
});
$(document).on('mouseup', function () {
  if (!cdDragLine) { return; }
  cdDragLine = null;
  Shiny.setInputValue('lines_moved', { a: cdLines.a, b: cdLines.b, nonce: Math.random() }, { priority: 'event' });
});

// Track tab: tracking points as markers that can be selected, deselected and dragged
var cdTrk = { geom: null, pts: [], drag: null };
function cdTrkBox() {
  var img = $('#trk_plot img')[0];
  if (!img || !cdTrk.geom) { return null; }
  var W = img.clientWidth, H = img.clientHeight, g = cdTrk.geom;
  return {
    x0: W * (g.fig[0] + (g.fig[1] - g.fig[0]) * g.plt[0]), x1: W * (g.fig[0] + (g.fig[1] - g.fig[0]) * g.plt[1]),
    yt: H * (1 - (g.fig[2] + (g.fig[3] - g.fig[2]) * g.plt[3])), yb: H * (1 - (g.fig[2] + (g.fig[3] - g.fig[2]) * g.plt[2])),
    left: img.offsetLeft, top: img.offsetTop
  };
}
function cdTrkToPx(x, y, b) {
  var u = cdTrk.geom.usr;
  return [b.x0 + (x - u[0]) / (u[1] - u[0]) * (b.x1 - b.x0), b.yb - (y - u[2]) / (u[3] - u[2]) * (b.yb - b.yt)];
}
function cdTrkFromPx(px, py, b) {
  var u = cdTrk.geom.usr;
  return [u[0] + (px - b.x0) / (b.x1 - b.x0) * (u[1] - u[0]), u[2] + (b.yb - py) / (b.yb - b.yt) * (u[3] - u[2])];
}
function cdTrkDraw() {
  var $p = $('#trk_plot');
  $p.find('.cd-tpt').remove();
  var b = cdTrkBox();
  if (!b) { return; }
  $p.css('position', 'relative');
  cdTrk.pts.forEach(function (p) {
    var xy = cdTrkToPx(p.x, p.y, b);
    if (xy[0] < b.x0 - 1 || xy[0] > b.x1 + 1 || xy[1] < b.yt - 1 || xy[1] > b.yb + 1) { return; }
    var el = $('<div class="cd-tpt' + (p.sel ? ' cd-tsel' : '') + '" data-id="' + p.id + '"></div>');
    el.css({ left: b.left + xy[0], top: b.top + xy[1] });
    el.attr('title', 'Depth ' + Number(p.x.toPrecision(6)) + ', period ' + Number(Math.pow(2, p.y).toPrecision(4)) +
      '\nClick: select / deselect; drag: move');
    // handlers on the marker itself, so the click never reaches the plot (where it would add a new point)
    el.on('mousedown', cdTrkDown);
    el.on('mouseup', function (ev) { ev.stopPropagation(); cdTrkUp(); });
    el.on('click dblclick', function (ev) { ev.stopPropagation(); });
    $p.append(el);
  });
}
Shiny.addCustomMessageHandler('trk_geom', function (x) { cdTrk.geom = x; setTimeout(cdTrkDraw, 60); });
Shiny.addCustomMessageHandler('trk_points', function (x) { cdTrk.pts = x.pts || []; cdTrkDraw(); });
$(document).on('shiny:value', function (e) { if (e.name === 'trk_plot') { setTimeout(cdTrkDraw, 80); } });
$(window).on('resize', function () { setTimeout(cdTrkDraw, 300); });
function cdTrkDown(e) {
  e.preventDefault();
  e.stopPropagation();
  cdTrk.drag = { id: $(this).data('id'), el: $(this), sx: e.pageX, sy: e.pageY, moved: false, add: e.shiftKey || e.ctrlKey || e.metaKey };
}
$(document).on('mousemove', function (e) {
  var d = cdTrk.drag;
  if (!d) { return; }
  if (Math.abs(e.pageX - d.sx) + Math.abs(e.pageY - d.sy) > 3) { d.moved = true; }
  if (!d.moved) { return; }
  var b = cdTrkBox();
  var off = $('#trk_plot img').offset();
  var px = Math.max(b.x0, Math.min(b.x1, e.pageX - off.left));
  var py = Math.max(b.yt, Math.min(b.yb, e.pageY - off.top));
  d.el.addClass('cd-tdrag').css({ left: b.left + px, top: b.top + py });
  d.px = px; d.py = py;
});
$(document).on('mouseup', function () { cdTrkUp(); });
function cdTrkUp() {
  var d = cdTrk.drag;
  if (!d) { return; }
  cdTrk.drag = null;
  if (d.moved && d.px !== undefined) {
    var xy = cdTrkFromPx(d.px, d.py, cdTrkBox());
    Shiny.setInputValue('trk_pt_move', { id: d.id, x: xy[0], y: xy[1], nonce: Math.random() }, { priority: 'event' });
  } else {
    Shiny.setInputValue('trk_pt_click', { id: d.id, add: d.add, nonce: Math.random() }, { priority: 'event' });
  }
}
// Delete / Backspace removes the selected points while the Track tab is shown (not while typing in a field)
$(document).on('keydown', function (e) {
  if ((e.key === 'Delete' || e.key === 'Backspace') && $('#trk_plot').is(':visible') &&
      !$(e.target).is('input, textarea, select, #cd_browser') && $('#trk_plot .cd-tsel').length) {
    e.preventDefault();
    Shiny.setInputValue('trk_delkey', Math.random(), { priority: 'event' });
  }
  if (e.key === 'Escape' && $('#trk_plot').is(':visible')) {
    Shiny.setInputValue('trk_selnone', Math.random(), { priority: 'event' });
  }
});
