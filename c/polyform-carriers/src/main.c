#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "polyform.h"
#include "svg.h"

static void usage(FILE *fp) {
    fprintf(fp,
        "polyform-cli <command> [args]\n\n"
        "Commands:\n"
        "  polyform-svg <40-bit-hex> <output.svg>\n"
        "  beetag-svg   <identity-1..32767> <output.svg>\n"
        "  aztec-svg    <40-bit-hex> <output.svg>\n"
        "  maxicode-svg <40-bit-hex> <output.svg>\n"
        "  smith-svg    <output.svg>\n"
        "  rods-svg     <divisor> <output.svg>\n"
        "  guess-svg    <min> <max> <output.svg>\n"
        "  inspect      <40-bit-hex>\n");
}

static FILE *open_svg(const char *path, int width, int height) {
    FILE *fp = fopen(path, "w");
    if (!fp) {
        fprintf(stderr, "open %s failed: %s\n", path, strerror(errno));
        return NULL;
    }
    char viewbox[64];
    snprintf(viewbox, sizeof(viewbox), "0 0 %d %d", width, height);
    svg_begin(fp, width, height, viewbox);
    return fp;
}

static int parse_codepoint(const char *s, pf_codepoint40_t *cp) {
    char *end = NULL;
    unsigned long long v = strtoull(s, &end, 16);
    if (!s[0] || (end && *end != '\0')) return 0;
    return pf_codepoint_from_u64((uint64_t)v, cp) ? 1 : 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        usage(stderr);
        return 1;
    }

    if (strcmp(argv[1], "inspect") == 0) {
        if (argc != 3) return 1;
        pf_codepoint40_t cp;
        pf_polyform_t poly;
        if (!parse_codepoint(argv[2], &cp) || !pf_polyform_from_codepoint(&cp, &poly)) {
            fprintf(stderr, "invalid codepoint\n");
            return 1;
        }
        printf("codepoint: 0x%010llX\n", (unsigned long long)pf_codepoint_to_u64(&cp));
        printf("basis: %s\nrank: %s\ngroup: %s\ndegree: %u\n", pf_basis_name(poly.basis), pf_rank_name(poly.rank), pf_group_name(poly.group), poly.degree);
        for (size_t i = 0; i < poly.cell_count; ++i) {
            printf("cell[%zu] = (%d,%d,%d)\n", i, poly.cells[i].x, poly.cells[i].y, poly.cells[i].z);
        }
        return 0;
    }

    if (strcmp(argv[1], "polyform-svg") == 0) {
        if (argc != 4) return 1;
        pf_codepoint40_t cp; pf_polyform_t poly;
        if (!parse_codepoint(argv[2], &cp) || !pf_polyform_from_codepoint(&cp, &poly)) return 1;
        FILE *fp = open_svg(argv[3], 800, 600);
        if (!fp) return 1;
        svg_write_polyform(fp, &poly, 40, true);
        svg_end(fp); fclose(fp); return 0;
    }

    if (strcmp(argv[1], "beetag-svg") == 0) {
        if (argc != 4) return 1;
        long id = strtol(argv[2], NULL, 10);
        pf_beetag_t tag;
        if (!pf_beetag_from_identity((uint16_t)id, &tag)) return 1;
        FILE *fp = open_svg(argv[3], 180, 180);
        if (!fp) return 1;
        svg_write_beetag(fp, &tag, 20, 2);
        svg_end(fp); fclose(fp); return 0;
    }

    if (strcmp(argv[1], "aztec-svg") == 0) {
        if (argc != 4) return 1;
        pf_codepoint40_t cp;
        if (!parse_codepoint(argv[2], &cp)) return 1;
        FILE *fp = open_svg(argv[3], 400, 400);
        if (!fp) return 1;
        svg_write_aztec_like(fp, &cp, 5, 10);
        svg_end(fp); fclose(fp); return 0;
    }

    if (strcmp(argv[1], "maxicode-svg") == 0) {
        if (argc != 4) return 1;
        pf_codepoint40_t cp;
        if (!parse_codepoint(argv[2], &cp)) return 1;
        FILE *fp = open_svg(argv[3], 420, 420);
        if (!fp) return 1;
        svg_write_maxicode_like(fp, &cp, 90, 6);
        svg_end(fp); fclose(fp); return 0;
    }

    if (strcmp(argv[1], "smith-svg") == 0) {
        if (argc != 3) return 1;
        FILE *fp = open_svg(argv[2], 700, 700);
        if (!fp) return 1;
        svg_write_smith_chart(fp, 700);
        svg_end(fp); fclose(fp); return 0;
    }

    if (strcmp(argv[1], "rods-svg") == 0) {
        if (argc != 4) return 1;
        int divisor = atoi(argv[2]);
        FILE *fp = open_svg(argv[3], 560, 360);
        if (!fp) return 1;
        svg_write_genaille_division_rods(fp, divisor);
        svg_end(fp); fclose(fp); return 0;
    }

    if (strcmp(argv[1], "guess-svg") == 0) {
        if (argc != 5) return 1;
        int minv = atoi(argv[2]);
        int maxv = atoi(argv[3]);
        FILE *fp = open_svg(argv[4], 800, 140);
        if (!fp) return 1;
        svg_write_binary_guess_smil(fp, minv, maxv);
        svg_end(fp); fclose(fp); return 0;
        }

    usage(stderr);
    return 1;
}
