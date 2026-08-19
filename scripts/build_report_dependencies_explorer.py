#!/usr/bin/env python3
"""Build a hierarchical JSON file and explorer from parsed report items."""

from __future__ import annotations

import argparse
import csv
import json
import os
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--columns-input", type=Path, default=Path("output/columns.csv"))
    parser.add_argument("--filters-input", type=Path, default=Path("output/filters.csv"))
    parser.add_argument(
        "--json-output",
        type=Path,
        default=Path("output/report_dependencies_hierarchy.json"),
    )
    parser.add_argument("--html-output", type=Path, required=True)
    parser.add_argument(
        "--standalone",
        action="store_true",
        help="Wrap the explorer as a complete HTML document for static hosting.",
    )
    return parser.parse_args()


def common_root(paths: list[str]) -> str:
    return os.path.commonpath(paths).rstrip("/")


def final_identifier(value: str) -> str:
    """Return the last quoted identifier from an Analytics field expression."""
    parts = [part for part in value.split('"') if part and part != "."]
    return parts[-1] if parts else value


def make_hierarchy(
    column_rows: list[dict[str, str]], filter_rows: list[dict[str, str]]
) -> dict:
    column_rows = [row for row in column_rows if row["record_scope"] == "report_column"]
    filter_rows = [row for row in filter_rows if row["record_scope"] == "report_filter"]
    report_paths = sorted(
        {row["report_path"] for row in column_rows + filter_rows if row["report_path"]}
    )
    root_path = common_root(report_paths)
    root = {
        "id": "root",
        "name": Path(root_path).name or "Report dependencies",
        "kind": "root",
        "path": root_path,
        "children": [],
    }

    folders: dict[tuple[str, ...], dict] = {(): root}
    reports: dict[str, dict] = {}

    def add_report(row: dict[str, str]) -> dict:
        relative = row["report_path"][len(root_path) :].strip("/").split("/")
        folder_parts = tuple(relative[:-1])
        parent = root
        for depth in range(1, len(folder_parts) + 1):
            key = folder_parts[:depth]
            if key not in folders:
                folder_path = root_path + "/" + "/".join(key)
                folders[key] = {
                    "id": "folder:" + folder_path,
                    "name": key[-1],
                    "kind": "folder",
                    "path": folder_path,
                    "children": [],
                }
                parent["children"].append(folders[key])
            parent = folders[key]

        report_key = row["report_path"]
        if report_key not in reports:
            report = {
                "id": "report:" + row["report_catalog_index"],
                "name": row["report_title"],
                "kind": "report",
                "path": row["report_path"],
                "catalog_index": int(row["report_catalog_index"]),
                "children": [],
            }
            reports[report_key] = report
            parent["children"].append(report)
        return reports[report_key]

    for row in sorted(
        column_rows,
        key=lambda item: (item["report_path"].casefold(), int(item["column_index"])),
    ):
        report = add_report(row)
        saved = row["column_source"] == "saved_reference"
        node = {
            "id": "column:" + row["column_record_id"],
            "name": row["column_name"] if saved else final_identifier(row["column_name"]),
            "kind": "saved_column" if saved else "inline_column",
            "path": row["definition_path"] if saved else row["report_path"],
            "record_id": row["column_record_id"],
            "item_index": int(row["column_index"]),
            "source": row["column_source"],
            "xml_type": row["column_xml_type"],
        }
        if row["formula"]:
            node["formula"] = row["formula"]
        if row["report_display_name"] and row["report_display_name"] != row["column_name"]:
            node["display_name"] = row["report_display_name"]
        if saved:
            node["resolved"] = row["is_resolved"].strip().upper() == "TRUE"
            if row["definition_catalog_index"]:
                node["resolved_catalog_index"] = int(row["definition_catalog_index"])
            if row["definition_name"]:
                node["resolved_object_title"] = row["definition_name"]
        report["children"].append(node)

    for row in sorted(
        filter_rows,
        key=lambda item: (item["report_path"].casefold(), int(item["filter_index"])),
    ):
        report = add_report(row)
        saved = row["filter_source"] == "saved_reference"
        field_name = final_identifier(row["field"]) if row["field"] else "Filter"
        if saved:
            name = row["definition_name"] or field_name
        else:
            preview = row["value_preview"]
            name = f"{field_name}: {row['operator'].upper()}"
            if preview:
                name += f" ({preview})"
        node = {
            "id": "filter:" + row["filter_record_id"],
            "name": name,
            "kind": "saved_filter" if saved else "inline_filter",
            "path": row["definition_path"] if saved else row["report_path"],
            "record_id": row["filter_record_id"],
            "item_index": int(row["filter_index"]),
            "source": row["filter_source"],
            "operator": row["operator"],
            "field": row["field"],
        }
        if row["filter_text"]:
            node["expression"] = row["filter_text"]
        if saved:
            node["resolved"] = row["is_resolved"].strip().upper() == "TRUE"
            if row["definition_catalog_index"]:
                node["resolved_catalog_index"] = int(row["definition_catalog_index"])
            if row["definition_name"]:
                node["resolved_object_title"] = row["definition_name"]
        report["children"].append(node)

    def summarize(node: dict) -> tuple[int, int, int, int, int, int, int]:
        if not node.get("children"):
            is_column = node["kind"] in {"saved_column", "inline_column"}
            is_filter = node["kind"] in {"saved_filter", "inline_filter"}
            is_saved = node["kind"] in {"saved_column", "saved_filter"}
            is_inline = node["kind"] in {"inline_column", "inline_filter"}
            return (
                int(is_column or is_filter), int(is_column), int(is_filter),
                int(is_saved), int(is_inline), int(node.get("resolved") is True), 0,
            )
        items = columns = filters = saved = inline = resolved = report_count = 0
        for child in node["children"]:
            child_summary = summarize(child)
            items += child_summary[0]
            columns += child_summary[1]
            filters += child_summary[2]
            saved += child_summary[3]
            inline += child_summary[4]
            resolved += child_summary[5]
            report_count += child_summary[6]
        if node["kind"] == "report":
            report_count += 1
        node["item_count"] = items
        node["column_count"] = columns
        node["filter_count"] = filters
        node["saved_count"] = saved
        node["inline_count"] = inline
        node["resolved_count"] = resolved
        node["unresolved_count"] = saved - resolved
        node["report_count"] = report_count
        return items, columns, filters, saved, inline, resolved, report_count

    summarize(root)
    return root


def explorer_fragment(data: dict) -> str:
    payload = json.dumps(data, ensure_ascii=True, separators=(",", ":")).replace("</", "<\\/")
    return f'''<div id="report-dependency-explorer">
  <div class="viz-controls" aria-label="Hierarchy controls">
    <label class="form-label" for="dependency-search">
      Search reports, columns, and filters
      <input class="form-control" id="dependency-search" type="search" autocomplete="off" placeholder="Try 'UCB Check-Out Type'">
    </label>
    <button class="btn" id="dependency-search-button" type="button"><i data-lucide="search" aria-hidden="true"></i> Find</button>
    <button class="btn" id="dependency-fit-button" type="button"><i data-lucide="maximize-2" aria-hidden="true"></i> Fit hierarchy</button>
  </div>
  <div id="dependency-search-results" role="listbox" aria-label="Search results"></div>
  <div class="dependency-status text-small text-muted" aria-live="polite"></div>
  <svg class="dependency-canvas" role="img" aria-labelledby="dependency-title dependency-desc">
    <title id="dependency-title">Report dependency hierarchy</title>
    <desc id="dependency-desc">Zoomable hierarchy of report folders, reports, and all saved or inline columns and filters.</desc>
    <g class="dependency-viewport"><g class="dependency-links"></g><g class="dependency-nodes"></g></g>
  </svg>
  <section class="card dependency-details" aria-live="polite"></section>
</div>
<script type="application/json" id="report-dependency-data">{payload}</script>
<style>
  #report-dependency-explorer {{ color: var(--foreground); width: 100%; }}
  #report-dependency-explorer .viz-controls {{ align-items: end; }}
  #report-dependency-explorer .form-label {{ flex: 1 1 280px; }}
  #report-dependency-explorer .form-control {{ width: 100%; }}
  #dependency-search-results {{ display: none; margin-top: 4px; max-width: 620px; }}
  #dependency-search-results.is-open {{ display: grid; gap: 2px; }}
  #dependency-search-results .btn {{ justify-content: flex-start; text-align: left; width: 100%; }}
  #report-dependency-explorer .dependency-status {{ min-height: 1.4em; margin: 8px 0 0; }}
  #report-dependency-explorer .dependency-canvas {{ display: block; width: 100%; height: 640px; touch-action: none; cursor: grab; }}
  #report-dependency-explorer .dependency-canvas:active {{ cursor: grabbing; }}
  #report-dependency-explorer .dependency-link {{ fill: none; stroke: var(--border); stroke-width: 1.25; }}
  #report-dependency-explorer .dependency-node {{ cursor: pointer; }}
  #report-dependency-explorer .dependency-node circle {{ fill: color-mix(in srgb, var(--viz-series-1) 18%, transparent); stroke: var(--viz-series-1); stroke-width: 1.5; }}
  #report-dependency-explorer .dependency-node[data-kind="folder"] circle,
  #report-dependency-explorer .dependency-node[data-kind="root"] circle {{ fill: color-mix(in srgb, var(--viz-series-2) 18%, transparent); stroke: var(--viz-series-2); }}
  #report-dependency-explorer .dependency-node[data-kind="saved_filter"] circle {{ fill: color-mix(in srgb, var(--viz-series-3) 18%, transparent); stroke: var(--viz-series-3); }}
  #report-dependency-explorer .dependency-node[data-kind="inline_column"] circle {{ fill: color-mix(in srgb, var(--viz-series-4) 18%, transparent); stroke: var(--viz-series-4); }}
  #report-dependency-explorer .dependency-node[data-kind="inline_filter"] circle {{ fill: color-mix(in srgb, var(--viz-series-5) 18%, transparent); stroke: var(--viz-series-5); }}
  #report-dependency-explorer .dependency-node[data-resolved="false"] circle {{ fill: color-mix(in srgb, var(--destructive) 15%, transparent); stroke: var(--destructive); }}
  #report-dependency-explorer .dependency-node.is-selected circle {{ fill: var(--primary); stroke: var(--primary); }}
  #report-dependency-explorer .dependency-node text {{ fill: var(--foreground); font-size: 12px; font-weight: 400; paint-order: stroke; stroke: var(--background); stroke-width: 3px; stroke-linejoin: round; }}
  #report-dependency-explorer .dependency-node .node-count {{ fill: var(--muted-foreground); font-size: 11px; }}
  #report-dependency-explorer .dependency-details {{ margin-top: 10px; }}
  #report-dependency-explorer .dependency-details h3 {{ margin-top: 0; overflow-wrap: anywhere; }}
  #report-dependency-explorer .dependency-details p {{ margin-bottom: 0; overflow-wrap: anywhere; }}
  #report-dependency-explorer .detail-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(160px, 1fr)); gap: 8px 20px; }}
  #report-dependency-explorer .detail-label {{ color: var(--muted-foreground); display: block; }}
  @media (max-width: 520px) {{
    #report-dependency-explorer .dependency-canvas {{ height: 540px; }}
  }}
</style>
<script src="https://cdn.jsdelivr.net/npm/d3@7.9.0/dist/d3.min.js"></script>
<script>
(() => {{
  const rootEl = document.getElementById("report-dependency-explorer");
  const data = JSON.parse(document.getElementById("report-dependency-data").textContent);
  const svg = d3.select(rootEl.querySelector(".dependency-canvas"));
  const viewport = svg.select(".dependency-viewport");
  const linksLayer = svg.select(".dependency-links");
  const nodesLayer = svg.select(".dependency-nodes");
  const details = rootEl.querySelector(".dependency-details");
  const status = rootEl.querySelector(".dependency-status");
  const input = rootEl.querySelector("#dependency-search");
  const resultsEl = rootEl.querySelector("#dependency-search-results");
  const expanded = new Set([data.id]);
  const nodeById = new Map();
  const parentById = new Map();
  const searchable = [];
  let selectedId = data.id;
  let currentRoot;
  let width = 736;
  let height = 640;

  function walk(node, parent = null, depth = 0) {{
    nodeById.set(node.id, node);
    if (parent) parentById.set(node.id, parent.id);
    searchable.push({{ node, depth, haystack: [node.name, node.path, node.kind, node.resolved_object_title || "", node.formula || "", node.expression || "", node.field || ""].join(" ").toLowerCase() }});
    (node.children || []).forEach(child => walk(child, node, depth + 1));
  }}
  walk(data);
  (data.children || []).forEach(child => expanded.add(child.id));

  const zoom = d3.zoom().scaleExtent([0.08, 3.5]).on("zoom", event => viewport.attr("transform", event.transform));
  svg.call(zoom);

  function visibleRoot() {{
    return d3.hierarchy(data, node => expanded.has(node.id) ? node.children : null);
  }}

  function render(animate = true) {{
    currentRoot = visibleRoot();
    d3.tree().nodeSize([34, 280])(currentRoot);
    const duration = animate && !matchMedia("(prefers-reduced-motion: reduce)").matches ? 420 : 0;
    const transition = svg.transition().duration(duration);
    const link = linksLayer.selectAll("path").data(currentRoot.links(), d => d.target.data.id);
    link.exit().transition(transition).style("opacity", 0).remove();
    link.join("path")
      .attr("class", "dependency-link")
      .attr("d", d3.linkHorizontal().x(d => d.y).y(d => d.x))
      .style("opacity", 0)
      .transition(transition).style("opacity", 1);

    const node = nodesLayer.selectAll("g.dependency-node").data(currentRoot.descendants(), d => d.data.id);
    node.exit().transition(transition).style("opacity", 0).remove();
    const enter = node.enter().append("g")
      .attr("class", "dependency-node")
      .attr("role", "button")
      .attr("aria-label", d => d.data.name)
      .attr("data-kind", d => d.data.kind)
      .attr("data-resolved", d => String(d.data.resolved))
      .on("click", (event, d) => {{ event.stopPropagation(); selectNode(d.data.id, true); }});
    enter.append("circle").attr("r", 6);
    enter.append("text").attr("x", 11).attr("dy", "0.32em");
    enter.append("text").attr("class", "node-count").attr("x", -11).attr("dy", "0.32em").attr("text-anchor", "end");
    const merged = enter.merge(node)
      .classed("is-selected", d => d.data.id === selectedId)
      .attr("data-kind", d => d.data.kind)
      .attr("data-resolved", d => String(d.data.resolved));
    merged.select("text:not(.node-count)").text(d => d.data.name.length > 42 ? d.data.name.slice(0, 39) + "..." : d.data.name);
    merged.select(".node-count").text(d => d.data.children ? (expanded.has(d.data.id) ? "-" : "+" ) + d.data.children.length : "");
    merged.transition(transition).attr("transform", d => `translate(${{d.y}},${{d.x}})`).style("opacity", 1);
    status.textContent = `${{currentRoot.descendants().length.toLocaleString()}} of ${{nodeById.size.toLocaleString()}} nodes visible`;
  }}

  function nodeDetails(node) {{
    const labels = {{ root: "Hierarchy", folder: "Folder", report: "Report", saved_column: "Saved column", inline_column: "Inline column", saved_filter: "Saved filter", inline_filter: "Inline filter" }};
    const values = [];
    if (node.report_count != null) values.push(["Reports", node.report_count]);
    if (node.item_count != null) values.push(["Report items", node.item_count]);
    if (node.column_count != null) values.push(["Columns", node.column_count]);
    if (node.filter_count != null) values.push(["Filters", node.filter_count]);
    if (node.saved_count != null) values.push(["Saved items", node.saved_count]);
    if (node.inline_count != null) values.push(["Inline items", node.inline_count]);
    if (node.resolved_count != null) values.push(["Resolved", node.resolved_count]);
    if (node.unresolved_count != null) values.push(["Unresolved", node.unresolved_count]);
    if (node.catalog_index != null) values.push(["Catalog index", node.catalog_index]);
    if (node.record_id) values.push(["Record ID", node.record_id]);
    if (node.item_index != null) values.push(["Position", node.item_index]);
    if (node.source) values.push(["Source", node.source === "saved_reference" ? "Saved reference" : "Inline"]);
    if (node.resolved != null) values.push(["Resolution", node.resolved ? "Resolved" : "Unresolved"]);
    if (node.resolved_catalog_index != null) values.push(["Resolved catalog index", node.resolved_catalog_index]);
    if (node.resolved_object_title) values.push(["Resolved object", node.resolved_object_title]);
    const expression = node.formula || node.expression || "";
    details.innerHTML = `<h3>${{escapeHtml(node.name)}}</h3><div class="detail-grid"><div><span class="detail-label">Type</span>${{escapeHtml(labels[node.kind] || node.kind)}}</div>${{values.map(([label, value]) => `<div><span class="detail-label">${{escapeHtml(label)}}</span>${{escapeHtml(String(value))}}</div>`).join("")}}</div>${{expression ? `<p><span class="detail-label">Expression</span><code>${{escapeHtml(expression)}}</code></p>` : ""}}<p class="text-small text-muted">${{escapeHtml(node.path || "")}}</p>`;
  }}

  function escapeHtml(value) {{
    return value.replace(/[&<>"']/g, char => ({{"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#039;"}})[char]);
  }}

  function revealPath(id) {{
    let cursor = id;
    while (parentById.has(cursor)) {{
      cursor = parentById.get(cursor);
      expanded.add(cursor);
    }}
  }}

  function focusNode(id, scale = 1.05) {{
    const target = currentRoot.descendants().find(item => item.data.id === id);
    if (!target) return;
    const transform = d3.zoomIdentity.translate(width * 0.34 - target.y * scale, height * 0.5 - target.x * scale).scale(scale);
    svg.transition().duration(matchMedia("(prefers-reduced-motion: reduce)").matches ? 0 : 700).call(zoom.transform, transform);
  }}

  function selectNode(id, toggleChildren = false) {{
    const node = nodeById.get(id);
    selectedId = id;
    revealPath(id);
    if (toggleChildren && node.children) {{
      if (expanded.has(id)) expanded.delete(id); else expanded.add(id);
    }} else if (node.children) {{
      expanded.add(id);
    }}
    render();
    nodeDetails(node);
    requestAnimationFrame(() => focusNode(id));
  }}

  function matches(query) {{
    const terms = query.trim().toLowerCase().split(/\\s+/).filter(Boolean);
    if (!terms.length) return [];
    return searchable.filter(item => terms.every(term => item.haystack.includes(term)))
      .sort((a, b) => Number(b.node.name.toLowerCase().startsWith(query.toLowerCase())) - Number(a.node.name.toLowerCase().startsWith(query.toLowerCase())) || a.depth - b.depth || a.node.name.localeCompare(b.node.name))
      .slice(0, 8);
  }}

  function showResults() {{
    const found = matches(input.value);
    resultsEl.replaceChildren(...found.map(item => {{
      const button = document.createElement("button");
      button.type = "button";
      button.className = "btn btn-ghost";
      button.setAttribute("role", "option");
      const label = {{ folder: "Folder", report: "Report", saved_column: "Saved column", inline_column: "Inline column", saved_filter: "Saved filter", inline_filter: "Inline filter" }}[item.node.kind] || "Hierarchy";
      button.textContent = `${{item.node.name}} - ${{label}}`;
      button.addEventListener("click", () => {{ input.value = item.node.name; resultsEl.classList.remove("is-open"); selectNode(item.node.id); }});
      return button;
    }}));
    resultsEl.classList.toggle("is-open", found.length > 0);
    status.textContent = input.value.trim() ? `${{found.length}} best matches` : `${{currentRoot.descendants().length.toLocaleString()}} of ${{nodeById.size.toLocaleString()}} nodes visible`;
    return found;
  }}

  function runSearch() {{
    const found = matches(input.value);
    if (found.length) {{
      resultsEl.classList.remove("is-open");
      selectNode(found[0].node.id);
    }} else {{
      status.textContent = input.value.trim() ? "No matching reports, columns, or filters" : "Enter a report, column, filter, or folder name";
    }}
  }}

  function fitHierarchy() {{
    const nodes = currentRoot.descendants();
    const xExtent = d3.extent(nodes, d => d.x);
    const yExtent = d3.extent(nodes, d => d.y);
    const graphWidth = Math.max(1, yExtent[1] - yExtent[0] + 360);
    const graphHeight = Math.max(1, xExtent[1] - xExtent[0] + 90);
    const scale = Math.max(0.08, Math.min(1.15, width / graphWidth, height / graphHeight));
    const transform = d3.zoomIdentity.translate(width / 2 - (yExtent[0] + yExtent[1]) * scale / 2, height / 2 - (xExtent[0] + xExtent[1]) * scale / 2).scale(scale);
    svg.transition().duration(matchMedia("(prefers-reduced-motion: reduce)").matches ? 0 : 650).call(zoom.transform, transform);
  }}

  input.addEventListener("input", showResults);
  input.addEventListener("keydown", event => {{ if (event.key === "Enter") {{ event.preventDefault(); runSearch(); }} }});
  rootEl.querySelector("#dependency-search-button").addEventListener("click", runSearch);
  rootEl.querySelector("#dependency-fit-button").addEventListener("click", fitHierarchy);
  svg.on("click", () => resultsEl.classList.remove("is-open"));

  new ResizeObserver(entries => {{
    const rect = entries[0].contentRect;
    width = Math.max(320, rect.width);
    height = Math.max(500, rect.height);
    svg.attr("viewBox", `0 0 ${{width}} ${{height}}`);
  }}).observe(svg.node());

  render(false);
  nodeDetails(data);
  requestAnimationFrame(fitHierarchy);
}})();
</script>
'''


def standalone_document(fragment: str) -> str:
    """Wrap the host-ready fragment with the styles needed by GitHub Pages."""
    return f'''<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="description" content="Interactive hierarchy of Alma Analytics reports, columns, and filters.">
  <title>Alma Analytics Report Hierarchy</title>
  <style>
    :root {{
      color-scheme: light dark;
      --background: light-dark(#f8fafc, #111827);
      --foreground: light-dark(#172033, #e8edf7);
      --card: light-dark(#ffffff, #192235);
      --card-foreground: light-dark(#172033, #e8edf7);
      --popover: light-dark(#ffffff, #192235);
      --popover-foreground: light-dark(#172033, #e8edf7);
      --primary: light-dark(#2448a8, #87a7ff);
      --primary-foreground: light-dark(#ffffff, #10182b);
      --secondary: light-dark(#e8edf7, #2a354b);
      --secondary-foreground: light-dark(#172033, #e8edf7);
      --muted: light-dark(#eef2f8, #252f43);
      --muted-foreground: light-dark(#5b667a, #acb7ca);
      --accent: light-dark(#e4ebff, #28375a);
      --accent-foreground: light-dark(#18337f, #dbe5ff);
      --destructive: light-dark(#bd2c38, #ff7d86);
      --border: light-dark(#cbd3df, #455168);
      --input: light-dark(#aeb8c8, #59657a);
      --ring: light-dark(#315cc6, #91aeff);
      --viz-series-1: light-dark(#315cc6, #91aeff);
      --viz-series-2: light-dark(#7b4eb8, #c29cff);
      --viz-series-3: light-dark(#167a67, #5fd3b8);
      --viz-series-4: light-dark(#b05b16, #ffad66);
      --viz-series-5: light-dark(#a53c74, #f291c3);
      --font-size-base: 16px;
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      background: var(--background);
      color: var(--foreground);
      font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      font-size: var(--font-size-base);
    }}
    main {{ width: min(100%, 1500px); margin: 0 auto; padding: 24px; }}
    h1 {{ margin: 0 0 4px; font-size: clamp(1.35rem, 2.5vw, 2rem); font-weight: 500; }}
    .site-intro {{ margin: 0 0 20px; color: var(--muted-foreground); }}
    .viz-controls {{ display: flex; flex-wrap: wrap; gap: 10px; }}
    .form-label {{ display: grid; gap: 6px; font-weight: 500; }}
    .form-control, .btn {{
      min-height: 42px;
      border: 1px solid var(--input);
      border-radius: 8px;
      background: var(--card);
      color: var(--card-foreground);
      font: inherit;
    }}
    .form-control {{ padding: 8px 11px; }}
    .btn {{ display: inline-flex; align-items: center; gap: 7px; padding: 8px 13px; cursor: pointer; }}
    .btn:hover {{ background: var(--accent); color: var(--accent-foreground); }}
    .btn:focus-visible, .form-control:focus-visible {{ outline: 3px solid var(--ring); outline-offset: 2px; }}
    .btn-ghost {{ border-color: transparent; background: transparent; }}
    .card {{
      padding: 16px;
      border: 1px solid var(--border);
      border-radius: 10px;
      background: var(--card);
      color: var(--card-foreground);
    }}
    .text-small {{ font-size: .8125rem; }}
    .text-muted {{ color: var(--muted-foreground); }}
    code {{ overflow-wrap: anywhere; }}
    @media (max-width: 520px) {{ main {{ padding: 14px; }} }}
  </style>
</head>
<body>
  <main>
    <h1>Alma Analytics Report Hierarchy</h1>
    <p class="site-intro">Explore reports and their saved or inline columns and filters.</p>
{fragment}
  </main>
</body>
</html>
'''


def main() -> None:
    args = parse_args()
    with args.columns_input.open(newline="", encoding="utf-8-sig") as source:
        column_rows = list(csv.DictReader(source))
    with args.filters_input.open(newline="", encoding="utf-8-sig") as source:
        filter_rows = list(csv.DictReader(source))
    hierarchy = make_hierarchy(column_rows, filter_rows)
    args.json_output.parent.mkdir(parents=True, exist_ok=True)
    args.html_output.parent.mkdir(parents=True, exist_ok=True)
    args.json_output.write_text(
        json.dumps(hierarchy, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )
    fragment = explorer_fragment(hierarchy)
    html_output = standalone_document(fragment) if args.standalone else fragment
    args.html_output.write_text(html_output, encoding="utf-8")
    print(
        f"Built {hierarchy['report_count']} reports and "
        f"{hierarchy['item_count']} report items "
        f"({hierarchy['saved_count']} saved, {hierarchy['inline_count']} inline)."
    )
    print(args.json_output.resolve())
    print(args.html_output.resolve())


if __name__ == "__main__":
    main()
