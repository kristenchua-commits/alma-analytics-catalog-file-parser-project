#!/usr/bin/env python3
"""Export the parsed report hierarchy as a searchable, printable PDF."""

from __future__ import annotations

import argparse
import json
from collections import defaultdict
from pathlib import Path
from xml.sax.saxutils import escape

from reportlab.lib import colors
from reportlab.lib.enums import TA_LEFT
from reportlab.lib.pagesizes import landscape, letter
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.units import inch
from reportlab.platypus import (
    BaseDocTemplate,
    Frame,
    PageBreak,
    PageTemplate,
    Paragraph,
    Spacer,
    Table,
    TableStyle,
)


PAGE_SIZE = landscape(letter)
NAVY = colors.HexColor("#183153")
BLUE = colors.HexColor("#315CC6")
PURPLE = colors.HexColor("#7B4EB8")
GREEN = colors.HexColor("#167A67")
ORANGE = colors.HexColor("#B05B16")
MAGENTA = colors.HexColor("#A53C74")
INK = colors.HexColor("#172033")
MUTED = colors.HexColor("#5B667A")
LINE = colors.HexColor("#CBD3DF")
PALE = colors.HexColor("#F2F5FA")
WHITE = colors.white


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--input",
        type=Path,
        default=Path("output/report_dependencies_hierarchy.json"),
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("output/pdf/alma_analytics_report_hierarchy.pdf"),
    )
    return parser.parse_args()


def ascii_text(value: object) -> str:
    return (
        str(value or "")
        .replace("\u2010", "-")
        .replace("\u2011", "-")
        .replace("\u2012", "-")
        .replace("\u2013", "-")
        .replace("\u2014", "-")
        .replace("\u2018", "'")
        .replace("\u2019", "'")
        .replace("\u201c", '"')
        .replace("\u201d", '"')
    )


def paragraph(value: object, style: ParagraphStyle) -> Paragraph:
    return Paragraph(escape(ascii_text(value)), style)


def collect_reports(root: dict) -> list[tuple[str, dict]]:
    reports: list[tuple[str, dict]] = []

    def walk(node: dict, folders: tuple[str, ...] = ()) -> None:
        kind = node.get("kind")
        next_folders = folders + (node["name"],) if kind == "folder" else folders
        if kind == "report":
            reports.append((" / ".join(next_folders), node))
        for child in node.get("children", []):
            walk(child, next_folders)

    walk(root)
    return sorted(reports, key=lambda item: (item[0].casefold(), item[1]["name"].casefold()))


class HierarchyDocTemplate(BaseDocTemplate):
    def __init__(self, filename: str, **kwargs) -> None:
        super().__init__(filename, **kwargs)
        frame = Frame(
            self.leftMargin,
            self.bottomMargin,
            self.width,
            self.height,
            id="content",
        )
        self.addPageTemplates(PageTemplate(id="main", frames=frame, onPage=self.draw_page))

    def draw_page(self, canvas, doc) -> None:
        canvas.saveState()
        width, _ = PAGE_SIZE
        canvas.setStrokeColor(LINE)
        canvas.setLineWidth(0.5)
        canvas.line(doc.leftMargin, 0.36 * inch, width - doc.rightMargin, 0.36 * inch)
        canvas.setFillColor(MUTED)
        canvas.setFont("Helvetica", 7.5)
        canvas.drawString(doc.leftMargin, 0.20 * inch, "Alma Analytics report hierarchy")
        canvas.drawRightString(width - doc.rightMargin, 0.20 * inch, f"Page {doc.page}")
        canvas.restoreState()

    def afterFlowable(self, flowable) -> None:
        if not isinstance(flowable, Paragraph):
            return
        level = getattr(flowable.style, "outlineLevel", None)
        if level is None:
            return
        text = flowable.getPlainText()
        key = f"outline-{self.seq.nextf('outline')}"
        self.canv.bookmarkPage(key)
        self.canv.addOutlineEntry(text, key, level=level, closed=level > 0)


def make_styles() -> dict[str, ParagraphStyle]:
    base = getSampleStyleSheet()
    return {
        "cover_title": ParagraphStyle(
            "CoverTitle",
            parent=base["Title"],
            fontName="Helvetica-Bold",
            fontSize=28,
            leading=32,
            textColor=NAVY,
            spaceAfter=10,
        ),
        "cover_subtitle": ParagraphStyle(
            "CoverSubtitle",
            parent=base["BodyText"],
            fontName="Helvetica",
            fontSize=11,
            leading=16,
            textColor=MUTED,
            spaceAfter=18,
        ),
        "folder": ParagraphStyle(
            "Folder",
            parent=base["Heading1"],
            fontName="Helvetica-Bold",
            fontSize=16,
            leading=20,
            textColor=NAVY,
            spaceAfter=7,
            outlineLevel=0,
        ),
        "report": ParagraphStyle(
            "Report",
            parent=base["Heading2"],
            fontName="Helvetica-Bold",
            fontSize=14,
            leading=17,
            textColor=NAVY,
            spaceAfter=4,
            outlineLevel=1,
        ),
        "path": ParagraphStyle(
            "Path",
            parent=base["BodyText"],
            fontName="Helvetica",
            fontSize=7.5,
            leading=10,
            textColor=MUTED,
            spaceAfter=8,
        ),
        "body": ParagraphStyle(
            "Body",
            parent=base["BodyText"],
            fontName="Helvetica",
            fontSize=9,
            leading=12,
            textColor=INK,
        ),
        "cell": ParagraphStyle(
            "Cell",
            parent=base["BodyText"],
            fontName="Helvetica",
            fontSize=7.2,
            leading=9.2,
            textColor=INK,
            alignment=TA_LEFT,
        ),
        "cell_muted": ParagraphStyle(
            "CellMuted",
            parent=base["BodyText"],
            fontName="Helvetica",
            fontSize=6.8,
            leading=8.6,
            textColor=MUTED,
            alignment=TA_LEFT,
        ),
        "table_header": ParagraphStyle(
            "TableHeader",
            parent=base["BodyText"],
            fontName="Helvetica-Bold",
            fontSize=7.2,
            leading=9,
            textColor=WHITE,
        ),
        "stat_value": ParagraphStyle(
            "StatValue",
            parent=base["BodyText"],
            fontName="Helvetica-Bold",
            fontSize=18,
            leading=20,
            textColor=NAVY,
        ),
        "stat_label": ParagraphStyle(
            "StatLabel",
            parent=base["BodyText"],
            fontName="Helvetica",
            fontSize=7.5,
            leading=9,
            textColor=MUTED,
        ),
    }


def stat_box(value: int, label: str, styles: dict[str, ParagraphStyle]) -> Table:
    box = Table(
        [[paragraph(f"{value:,}", styles["stat_value"])], [paragraph(label, styles["stat_label"])]],
        colWidths=[1.48 * inch],
        rowHeights=[0.34 * inch, 0.22 * inch],
    )
    box.setStyle(
        TableStyle(
            [
                ("BACKGROUND", (0, 0), (-1, -1), PALE),
                ("BOX", (0, 0), (-1, -1), 0.6, LINE),
                ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
                ("LEFTPADDING", (0, 0), (-1, -1), 8),
                ("RIGHTPADDING", (0, 0), (-1, -1), 8),
                ("TOPPADDING", (0, 0), (-1, -1), 2),
                ("BOTTOMPADDING", (0, 0), (-1, -1), 2),
            ]
        )
    )
    return box


def item_type(node: dict) -> str:
    return {
        "saved_column": "Saved column",
        "inline_column": "Inline column",
        "saved_filter": "Saved filter",
        "inline_filter": "Inline filter",
    }.get(node.get("kind"), node.get("kind", "Item"))


def item_detail(node: dict) -> str:
    expression = node.get("formula") or node.get("expression") or ""
    if expression:
        return expression
    if node.get("resolved_object_title"):
        return f"Reference: {node['resolved_object_title']}"
    return node.get("path", "")


def report_table(report: dict, styles: dict[str, ParagraphStyle]) -> Table:
    header = [
        paragraph("#", styles["table_header"]),
        paragraph("Type", styles["table_header"]),
        paragraph("Name", styles["table_header"]),
        paragraph("Formula, expression, or reference", styles["table_header"]),
    ]
    rows = [header]
    def sort_key(node: dict) -> tuple[int, int]:
        is_filter = node.get("kind") in {"saved_filter", "inline_filter"}
        return int(is_filter), node.get("item_index", 0)

    for node in sorted(report.get("children", []), key=sort_key):
        prefix = "F" if node.get("kind") in {"saved_filter", "inline_filter"} else "C"
        rows.append(
            [
                paragraph(f"{prefix}{node.get('item_index', '')}", styles["cell_muted"]),
                paragraph(item_type(node), styles["cell_muted"]),
                paragraph(node.get("name", ""), styles["cell"]),
                paragraph(item_detail(node), styles["cell_muted"]),
            ]
        )
    table = Table(
        rows,
        colWidths=[0.34 * inch, 0.86 * inch, 2.35 * inch, 6.21 * inch],
        repeatRows=1,
        hAlign="LEFT",
    )
    commands = [
        ("BACKGROUND", (0, 0), (-1, 0), NAVY),
        ("BOX", (0, 0), (-1, -1), 0.6, LINE),
        ("INNERGRID", (0, 0), (-1, -1), 0.35, LINE),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("LEFTPADDING", (0, 0), (-1, -1), 5),
        ("RIGHTPADDING", (0, 0), (-1, -1), 5),
        ("TOPPADDING", (0, 0), (-1, -1), 4),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 4),
    ]
    for row_index in range(2, len(rows), 2):
        commands.append(("BACKGROUND", (0, row_index), (-1, row_index), PALE))
    table.setStyle(TableStyle(commands))
    return table


def build_pdf(source: Path, destination: Path) -> None:
    root = json.loads(source.read_text(encoding="utf-8"))
    reports = collect_reports(root)
    by_folder: dict[str, list[dict]] = defaultdict(list)
    for folder, report in reports:
        by_folder[folder or "Reports"].append(report)

    styles = make_styles()
    destination.parent.mkdir(parents=True, exist_ok=True)
    doc = HierarchyDocTemplate(
        str(destination),
        pagesize=PAGE_SIZE,
        leftMargin=0.45 * inch,
        rightMargin=0.45 * inch,
        topMargin=0.45 * inch,
        bottomMargin=0.50 * inch,
        title="Alma Analytics Report Hierarchy",
        author="Alma Analytics catalog file parser",
        subject="Reports with all saved and inline columns and filters",
    )

    story = [
        Spacer(1, 0.30 * inch),
        paragraph("Alma Analytics Report Hierarchy", styles["cover_title"]),
        paragraph(
            "A printable view of every report and its saved or inline columns and filters, "
            "parsed from annual_stats_fy_2025_2026.catalog.",
            styles["cover_subtitle"],
        ),
    ]
    stats = [
        (root["report_count"], "Reports"),
        (root["item_count"], "Report items"),
        (root["column_count"], "Columns"),
        (root["filter_count"], "Filters"),
        (root["saved_count"], "Saved references"),
        (root["inline_count"], "Inline items"),
    ]
    stat_grid = Table(
        [
            [stat_box(value, label, styles) for value, label in stats[:3]],
            [stat_box(value, label, styles) for value, label in stats[3:]],
        ],
        colWidths=[1.62 * inch] * 3,
        rowHeights=[0.68 * inch] * 2,
        hAlign="LEFT",
    )
    stat_grid.setStyle(TableStyle([("VALIGN", (0, 0), (-1, -1), "TOP")]))
    story.extend([stat_grid, Spacer(1, 0.22 * inch)])

    legend_rows = [
        ["Saved column", BLUE],
        ["Inline column", ORANGE],
        ["Saved filter", GREEN],
        ["Inline filter", MAGENTA],
    ]
    legend = Table(
        [["", paragraph(label, styles["body"])] for label, _ in legend_rows],
        colWidths=[0.16 * inch, 1.35 * inch],
        rowHeights=[0.22 * inch] * len(legend_rows),
        hAlign="LEFT",
    )
    legend_commands = [
        ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
        ("LEFTPADDING", (0, 0), (-1, -1), 2),
        ("RIGHTPADDING", (0, 0), (-1, -1), 4),
        ("TOPPADDING", (0, 0), (-1, -1), 1),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 1),
    ]
    for index, (_, color) in enumerate(legend_rows):
        legend_commands.extend(
            [
                ("BACKGROUND", (0, index), (0, index), color),
                ("BOX", (0, index), (0, index), 0.4, color),
            ]
        )
    legend.setStyle(TableStyle(legend_commands))
    story.extend([paragraph("Item types", styles["folder"]), legend, Spacer(1, 0.20 * inch)])

    folder_rows = [[paragraph("Folder", styles["table_header"]), paragraph("Reports", styles["table_header"]), paragraph("Items", styles["table_header"])]]
    for folder, folder_reports in sorted(by_folder.items(), key=lambda item: item[0].casefold()):
        folder_rows.append(
            [
                paragraph(folder, styles["cell"]),
                paragraph(len(folder_reports), styles["cell"]),
                paragraph(sum(len(report.get("children", [])) for report in folder_reports), styles["cell"]),
            ]
        )
    folder_table = Table(folder_rows, colWidths=[5.9 * inch, 0.8 * inch, 0.8 * inch], repeatRows=1, hAlign="LEFT")
    folder_table.setStyle(
        TableStyle(
            [
                ("BACKGROUND", (0, 0), (-1, 0), NAVY),
                ("BOX", (0, 0), (-1, -1), 0.6, LINE),
                ("INNERGRID", (0, 0), (-1, -1), 0.35, LINE),
                ("VALIGN", (0, 0), (-1, -1), "TOP"),
                ("LEFTPADDING", (0, 0), (-1, -1), 5),
                ("RIGHTPADDING", (0, 0), (-1, -1), 5),
                ("TOPPADDING", (0, 0), (-1, -1), 4),
                ("BOTTOMPADDING", (0, 0), (-1, -1), 4),
            ]
        )
    )
    story.extend([paragraph("Directory", styles["folder"]), folder_table])

    for folder, folder_reports in sorted(by_folder.items(), key=lambda item: item[0].casefold()):
        story.append(PageBreak())
        story.append(paragraph(folder, styles["folder"]))
        for report_index, report in enumerate(folder_reports):
            if report_index:
                story.append(PageBreak())
            story.append(paragraph(report["name"], styles["report"]))
            story.append(paragraph(report.get("path", ""), styles["path"]))
            story.append(report_table(report, styles))

    doc.build(story)


def main() -> None:
    args = parse_args()
    build_pdf(args.input, args.output)
    print(args.output.resolve())


if __name__ == "__main__":
    main()
