#ifndef POLYTOPE_CODEPOINT_BRIDGE_H
#define POLYTOPE_CODEPOINT_BRIDGE_H

#include "carrier40.h"
#include <stdint.h>

/*
 * Bridge from polytope registry metadata to deterministic 40-bit codepoint.
 *
 * Mapping (frozen):
 *   basis  = hash_of("simplex"|"cube"|"cross"|"exceptional"|...) mod 32
 *   rank   = dimension mod 32
 *   group  = family group index
 *   degree = family degree/class
 *   path   = witness/template selector (20 bits)
 *
 * This is a stable non-authority projection key.
 * Codepoint is not OMI identity.
 */

/* Derive a 40-bit codepoint from registry fields */
carrier40_t polytope_to_codepoint(
    const char *category,    /* e.g. "simplex", "cube", "cross" */
    int         dimension,   /* rank/dimension */
    int         group_idx,   /* family group */
    int         degree,      /* degree/class */
    uint32_t    path_sel     /* path/template selector (20 bits) */
);

/* Quick hash of category string to 5-bit value */
uint8_t category_to_basis(const char *category);

#endif /* POLYTOPE_CODEPOINT_BRIDGE_H */
