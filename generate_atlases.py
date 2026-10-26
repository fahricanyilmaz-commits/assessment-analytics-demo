"""Build two reproducible PDF atlases from the public synthetic CSV files only."""

import csv
import os
from collections import defaultdict
from pathlib import Path
from math import exp

from reportlab.lib import colors
from reportlab.lib.pagesizes import A4, landscape
from reportlab.pdfgen import canvas


ROOT = Path(os.environ.get("ASSESSMENT_DEMO_ROOT", Path(__file__).resolve().parent))
DATA = ROOT / "data"
OUTPUT = DATA / "atlases"
PAGE_W, PAGE_H = landscape(A4)
INK = colors.HexColor("#18324a")
TEAL = colors.HexColor("#167c81")
MUTED = colors.HexColor("#64788a")
GRID = colors.HexColor("#e3eaf0")
OPTION_COLORS = {
    "A": colors.HexColor("#167c81"),
    "B": colors.HexColor("#dc9250"),
    "C": colors.HexColor("#6658a8"),
    "D": colors.HexColor("#66842f"),
}


def read_csv(name):
    with (DATA / name).open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def text(c, x, y, value, size=9, color=INK, font="Helvetica"):
    c.setFillColor(color)
    c.setFont(font, size)
    c.drawString(x, y, value)


def heading(c, title, subtitle, page_number):
    text(c, 40, PAGE_H - 42, title, 17, INK, "Helvetica-Bold")
    text(c, 40, PAGE_H - 60, subtitle, 9, MUTED)
    c.setStrokeColor(GRID)
    c.line(40, PAGE_H - 72, PAGE_W - 40, PAGE_H - 72)
    c.line(40, 29, PAGE_W - 40, 29)
    text(c, 40, 16, "FICTIONAL DATA  |  Assessment Analytics Demo", 8, MUTED)
    text(c, PAGE_W - 80, 16, str(page_number), 8, MUTED)


def cover(c, title, subtitle, explanation, item_count):
    c.setFillColor(INK)
    c.rect(0, PAGE_H - 150, PAGE_W, 150, fill=1, stroke=0)
    text(c, 48, PAGE_H - 67, "ASSESSMENT ANALYTICS DEMO", 11, colors.white,
         "Helvetica-Bold")
    text(c, 48, PAGE_H - 112, title, 27, colors.white, "Helvetica-Bold")
    text(c, 48, PAGE_H - 188, subtitle, 15, INK, "Helvetica-Bold")
    for index, line in enumerate(explanation):
        text(c, 48, PAGE_H - 222 - 24 * index, line, 11, INK)
    text(c, 48, 94, f"{item_count} generated items | 4 panels per page | Options A-D",
         10, TEAL, "Helvetica-Bold")
    text(c, 48, 66, "All names, item identifiers, responses and values are synthetic.",
         10, MUTED)
    c.showPage()


def chart(c, x0, y0, width, height, item, series, x_labels, x_min, x_max, subtitle):
    c.setFillColor(colors.white)
    c.setStrokeColor(GRID)
    c.roundRect(x0, y0, width, height, 8, fill=1, stroke=1)
    text(c, x0 + 15, y0 + height - 25,
         f"{item['item_id']}   {item['wave']} / {item['form']} / {item['domain']}",
         10, INK, "Helvetica-Bold")
    text(c, x0 + 15, y0 + height - 40, subtitle, 8, MUTED)
    left, bottom = x0 + 48, y0 + 47
    plot_w, plot_h = width - 67, height - 104
    for tick in range(5):
        value = tick / 4
        yy = bottom + value * plot_h
        c.setStrokeColor(GRID)
        c.setLineWidth(0.6)
        c.line(left, yy, left + plot_w, yy)
        text(c, left - 28, yy - 3, f"{value:.2f}" if tick in (0, 2, 4) else "",
             7, MUTED)
    c.setStrokeColor(MUTED)
    c.line(left, bottom, left + plot_w, bottom)
    c.line(left, bottom, left, bottom + plot_h)
    for xx, label in x_labels:
        px = left + (xx - x_min) / (x_max - x_min) * plot_w
        c.line(px, bottom, px, bottom - 3)
        text(c, px - 8, bottom - 16, label, 7, MUTED)
    for option in "ABCD":
        values = series[option]
        c.setStrokeColor(OPTION_COLORS[option])
        c.setLineWidth(1.8)
        path = c.beginPath()
        for index, (xx, yy) in enumerate(values):
            px = left + (xx - x_min) / (x_max - x_min) * plot_w
            py = bottom + max(0, min(1, yy)) * plot_h
            if index == 0:
                path.moveTo(px, py)
            else:
                path.lineTo(px, py)
        c.drawPath(path)
        if len(values) <= 5:
            c.setFillColor(OPTION_COLORS[option])
            for xx, yy in values:
                px = left + (xx - x_min) / (x_max - x_min) * plot_w
                py = bottom + max(0, min(1, yy)) * plot_h
                c.circle(px, py, 2.2, fill=1, stroke=0)
    legend_x = x0 + 110
    for option in "ABCD":
        c.setFillColor(OPTION_COLORS[option])
        c.rect(legend_x, y0 + 15, 12, 3, fill=1, stroke=0)
        text(c, legend_x + 16, y0 + 12, option, 8, INK)
        legend_x += 45


def make_atlas(filename, title, subtitle, explanation, items, series_for_item,
               x_labels, x_min, x_max, panel_subtitle):
    OUTPUT.mkdir(parents=True, exist_ok=True)
    c = canvas.Canvas(str(OUTPUT / filename), pagesize=(PAGE_W, PAGE_H),
                      invariant=1, pageCompression=1)
    c.setTitle(title + " | Synthetic demonstration")
    c.setAuthor("Assessment Analytics Demo")
    c.setSubject("Fictional generated item data")
    cover(c, title, subtitle, explanation, len(items))
    margin, gap = 40, 16
    width = (PAGE_W - 2 * margin - gap) / 2
    height = (PAGE_H - 112 - gap) / 2
    for start in range(0, len(items), 4):
        page = start // 4 + 2
        heading(c, title, subtitle, page)
        for offset, item in enumerate(items[start:start + 4]):
            x = margin + (offset % 2) * (width + gap)
            y = 47 + (1 - offset // 2) * (height + gap)
            chart(c, x, y, width, height, item, series_for_item(item),
                  x_labels, x_min, x_max, panel_subtitle(item))
        c.showPage()
    c.save()


def main():
    items = sorted(read_csv("item_register.csv"), key=lambda row: row["item_id"])
    profiles = defaultdict(lambda: defaultdict(list))
    for row in read_csv("option_profiles.csv"):
        group = int(row["rest_score_group"].split()[-1])
        profiles[row["item_id"]][row["option"]].append(
            (group, float(row["proportion"])))

    def nominal(item):
        a, b, key = float(item["simulation_a"]), float(item["simulation_b"]), item["key"]
        result = {}
        for option in "ABCD":
            result[option] = []
            for index in range(81):
                theta = -3 + index * 6 / 80
                p_key = 1 / (1 + exp(-a * (theta - b)))
                result[option].append((theta, p_key if option == key else (1 - p_key) / 3))
        return result

    make_atlas(
        "nominal_option_curves_demo.pdf", "Illustrative option curves",
        "Simulation input curves, not fitted nominal estimates",
        ["The keyed option follows each item's generated 2PL probability.",
         "The remaining probability is divided equally across three distractors.",
         "These curves illustrate a dashboard view; they are not study findings."],
        items, nominal, [(-3, "-3"), (0, "0"), (3, "+3")], -3, 3,
        lambda item: f"Key {item['key']}  |  Generated a={item['simulation_a']}, b={item['simulation_b']}")

    make_atlas(
        "empirical_option_profiles_demo.pdf", "Generated option profiles",
        "Observed shares within simulated rest score groups",
        ["Each trace summarizes fictional participant responses in five score groups.",
         "All item IDs and responses were produced by the included generator.",
         "Percentages describe the simulation only; no real examinees are included."],
        items, lambda item: {option: sorted(profiles[item["item_id"]][option])
                             for option in "ABCD"},
        [(index, str(index)) for index in range(1, 6)], 1, 5,
        lambda item: f"Key {item['key']}  |  n={item['sample_n']} simulated responses")


if __name__ == "__main__":
    main()
