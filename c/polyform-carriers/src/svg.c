#include "svg.h"
#include <math.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

static void rect(FILE *fp, double x, double y, double w, double h, const char *fill, const char *stroke) {
    fprintf(fp, "<rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\" stroke=\"%s\"/>\n", x, y, w, h, fill, stroke);
}

static void line_(FILE *fp, double x1, double y1, double x2, double y2, const char *stroke, double width) {
    fprintf(fp, "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"%.2f\"/>\n", x1, y1, x2, y2, stroke, width);
}

static void circle_(FILE *fp, double cx, double cy, double r, const char *fill, const char *stroke, double width) {
    fprintf(fp, "<circle cx=\"%.2f\" cy=\"%.2f\" r=\"%.2f\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%.2f\"/>\n", cx, cy, r, fill, stroke, width);
}

static void text_(FILE *fp, double x, double y, const char *txt, int size) {
    fprintf(fp, "<text x=\"%.2f\" y=\"%.2f\" font-family=\"monospace\" font-size=\"%d\">%s</text>\n", x, y, size, txt);
}

bool svg_begin(FILE *fp, int width, int height, const char *view_box) {
    if (!fp) return false;
    fprintf(fp, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    fprintf(fp, "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"%s\">\n", width, height, view_box ? view_box : "0 0 100 100");
    fprintf(fp, "<g>\n");
    return true;
}

void svg_end(FILE *fp) {
    if (!fp) return;
    fprintf(fp, "</g>\n</svg>\n");
}

static void draw_square_cell(FILE *fp, double x, double y, double s, bool extrude) {
    if (extrude) {
        rect(fp, x + s * 0.2, y - s * 0.2, s, s, "#ddd", "black");
        line_(fp, x, y, x + s * 0.2, y - s * 0.2, "black", 1.0);
        line_(fp, x + s, y, x + s * 1.2, y - s * 0.2, "black", 1.0);
        line_(fp, x + s, y + s, x + s * 1.2, y + s * 0.8, "black", 1.0);
        line_(fp, x, y + s, x + s * 0.2, y + s * 0.8, "black", 1.0);
    }
    rect(fp, x, y, s, s, "white", "black");
}

static void draw_hex_cell(FILE *fp, double cx, double cy, double r) {
    fprintf(fp, "<polygon points=\"");
    for (int i = 0; i < 6; ++i) {
        double a = M_PI / 6.0 + i * M_PI / 3.0;
        double x = cx + r * cos(a);
        double y = cy + r * sin(a);
        fprintf(fp, "%.2f,%.2f ", x, y);
    }
    fprintf(fp, "\" fill=\"white\" stroke=\"black\"/>\n");
}

bool svg_write_polyform(FILE *fp, const pf_polyform_t *poly, int cell_size, bool extrude_25d) {
    if (!fp || !poly) return false;
    for (size_t i = 0; i < poly->cell_count; ++i) {
        const pf_cell_t *c = &poly->cells[i];
        double x = 60 + c->x * cell_size;
        double y = 60 + c->y * cell_size;
        if (poly->basis == PF_BASIS_HEXAGON) {
            draw_hex_cell(fp, x, y, cell_size / 2.0);
        } else {
            draw_square_cell(fp, x, y, (double)cell_size, extrude_25d && poly->rank != PF_RANK_PLANAR);
        }
    }
    return true;
}

bool svg_write_beetag(FILE *fp, const pf_beetag_t *tag, int module, int border_modules) {
    if (!fp || !tag) return false;
    int total = 5 + 2 * border_modules;
    rect(fp, 0, 0, total * module, total * module, "black", "black");
    rect(fp, module, module, (total - 2) * module, (total - 2) * module, "white", "none");
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < 5; ++x) {
            rect(fp, (x + border_modules) * module, (y + border_modules) * module, module, module,
                 tag->matrix[y][x] ? "white" : "black", "none");
        }
    }
    return true;
}

bool svg_write_aztec_like(FILE *fp, const pf_codepoint40_t *cp, int layers, int module) {
    if (!fp || !cp) return false;
    const int core = 11;
    int size = core + 4 * layers;
    rect(fp, 0, 0, size * module, size * module, "white", "black");
    int center = size / 2;
    for (int ring = 0; ring < 5; ++ring) {
        int start = center - ring;
        int end = center + ring;
        const char *fill = (ring % 2 == 0) ? "black" : "white";
        rect(fp, start * module, start * module, (end - start + 1) * module, (end - start + 1) * module, fill, "black");
    }
    uint64_t value = pf_codepoint_to_u64(cp);
    int bit_index = 39;
    for (int layer = 0; layer < layers && bit_index >= 0; ++layer) {
        int min = center - 3 - (layer * 2) - 1;
        int max = center + 3 + (layer * 2) + 1;
        for (int x = min; x <= max && bit_index >= 0; ++x) {
            int y = min;
            rect(fp, x * module, y * module, module, module, ((value >> bit_index--) & 1ULL) ? "black" : "white", "none");
        }
        for (int y = min + 1; y <= max && bit_index >= 0; ++y) {
            int x = max;
            rect(fp, x * module, y * module, module, module, ((value >> bit_index--) & 1ULL) ? "black" : "white", "none");
        }
        for (int x = max - 1; x >= min && bit_index >= 0; --x) {
            int y = max;
            rect(fp, x * module, y * module, module, module, ((value >> bit_index--) & 1ULL) ? "black" : "white", "none");
        }
        for (int y = max - 1; y > min && bit_index >= 0; --y) {
            int x = min;
            rect(fp, x * module, y * module, module, module, ((value >> bit_index--) & 1ULL) ? "black" : "white", "none");
        }
    }
    return true;
}

bool svg_write_maxicode_like(FILE *fp, const pf_codepoint40_t *cp, int radius, int rings) {
    if (!fp || !cp) return false;
    (void)cp;
    double cx = 200.0, cy = 200.0;
    for (int i = 0; i < rings; ++i) {
        double r = radius - i * 18.0;
        circle_(fp, cx, cy, r, (i % 2 == 0) ? "white" : "black", "black", 2.0);
    }
    int idx = 0;
    for (int row = -4; row <= 4; ++row) {
        int count = 7 - (row < 0 ? -row : row) / 2;
        double y = cy + row * 26.0;
        for (int col = -count; col <= count; ++col) {
            double x = cx + col * 28.0 + ((row & 1) ? 14.0 : 0.0);
            if (hypot(x - cx, y - cy) < radius * 0.45) continue;
            draw_hex_cell(fp, x, y, 12.0 + ((idx++ % 3) == 0));
        }
    }
    return true;
}

bool svg_write_smith_chart(FILE *fp, int size) {
    if (!fp) return false;
    double c = size / 2.0;
    circle_(fp, c, c, size * 0.45, "white", "black", 2.0);
    line_(fp, size * 0.05, c, size * 0.95, c, "black", 1.5);
    const double rs[] = {0.2, 0.5, 1.0, 2.0, 5.0};
    for (size_t i = 0; i < sizeof(rs)/sizeof(rs[0]); ++i) {
        double r = rs[i];
        double cx = size * (r / (1.0 + r));
        double rr = size / (2.0 * (1.0 + r));
        circle_(fp, cx, c, rr, "none", "#666", 0.75);
    }
    const double xs[] = {0.2, 0.5, 1.0, 2.0, 5.0};
    for (size_t i = 0; i < sizeof(xs)/sizeof(xs[0]); ++i) {
        double x = xs[i];
        double rr = size / (2.0 * x);
        circle_(fp, size * 0.95, c - rr, rr, "none", "#666", 0.75);
        circle_(fp, size * 0.95, c + rr, rr, "none", "#666", 0.75);
    }
    text_(fp, 20, 30, "Smith Chart", 18);
    return true;
}

bool svg_write_genaille_division_rods(FILE *fp, int divisor) {
    if (!fp || divisor < 2) return false;
    int rods = 9;
    int cell_w = 50, cell_h = 36;
    for (int i = 0; i < rods; ++i) {
        for (int row = 0; row < divisor; ++row) {
            int x = 20 + i * cell_w;
            int y = 20 + row * cell_h;
            rect(fp, x, y, cell_w, cell_h, "white", "black");
            line_(fp, x, y + cell_h, x + cell_w, y, "black", 1.0);
            char buf[32];
            snprintf(buf, sizeof(buf), "%d", (i + 1) * (row + 1));
            text_(fp, x + 8, y + 22, buf, 14);
        }
    }
    text_(fp, 20, divisor * cell_h + 50, "Genaille-style division rods", 18);
    return true;
}

bool svg_write_binary_guess_smil(FILE *fp, int min_value, int max_value) {
    if (!fp || min_value >= max_value) return false;
    int width = 800;
    int height = 140;
    rect(fp, 0, 0, width, height, "white", "black");
    int steps = 0;
    int lo = min_value, hi = max_value;
    while (lo <= hi) {
        ++steps;
        int mid = lo + (hi - lo) / 2;
        lo = mid + 1;
    }
    int x = 20;
    lo = min_value; hi = max_value;
    for (int i = 0; i < steps; ++i) {
        int mid = lo + (hi - lo) / 2;
        int w = 90;
        rect(fp, x, 30, w, 50, "#eee", "black");
        fprintf(fp, "<text x=\"%d\" y=\"60\" font-family=\"monospace\" font-size=\"16\">%d</text>\n", x + 30, mid);
        fprintf(fp, "<circle cx=\"%d\" cy=\"100\" r=\"6\" fill=\"black\">\n", x + 45);
        fprintf(fp, "<animate attributeName=\"cy\" values=\"100;90;100\" dur=\"1.2s\" begin=\"%0.1fs\" repeatCount=\"1\"/>\n", i * 0.25);
        fprintf(fp, "</circle>\n");
        x += 100;
        hi = mid - 1;
        if (lo > hi) break;
    }
    text_(fp, 20, 20, "Binary Guess Number Trick", 18);
    return true;
}
