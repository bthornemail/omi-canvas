#ifndef SVG_H
#define SVG_H

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include "polyform.h"

bool svg_begin(FILE *fp, int width, int height, const char *view_box);
void svg_end(FILE *fp);

bool svg_write_polyform(FILE *fp, const pf_polyform_t *poly, int cell_size, bool extrude_25d);
bool svg_write_beetag(FILE *fp, const pf_beetag_t *tag, int module, int border_modules);
bool svg_write_aztec_like(FILE *fp, const pf_codepoint40_t *cp, int layers, int module);
bool svg_write_maxicode_like(FILE *fp, const pf_codepoint40_t *cp, int radius, int rings);
bool svg_write_smith_chart(FILE *fp, int size);
bool svg_write_genaille_division_rods(FILE *fp, int divisor);
bool svg_write_binary_guess_smil(FILE *fp, int min_value, int max_value);

#endif
