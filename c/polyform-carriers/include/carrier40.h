#ifndef CARRIER40_H
#define CARRIER40_H

#include <stdint.h>
#include <stdbool.h>

/*
 * OMI Polyform Carrier Layer — 40-bit codepoint layout
 *
 * bits 39..35  basis      (32 values)
 * bits 34..30  rank       (32 values)
 * bits 29..25  group      (32 values)
 * bits 24..20  degree     (32 values)
 * bits 19..00  path/witness  (1,048,576 selectors)
 *
 * This is a projection handle, not OMI identity.
 * Rendering is projection. Validation determines authority.
 */

#define CARRIER40_BITS      40u
#define CARRIER40_BYTES     5u
#define CARRIER40_GROUPS    8u
#define CARRIER40_GROUP_BITS 5u
#define CARRIER40_GROUP_MASK 0x1Fu

/* Field positions */
#define CARRIER40_BASIS_SHIFT   35u
#define CARRIER40_RANK_SHIFT    30u
#define CARRIER40_GROUP_SHIFT   25u
#define CARRIER40_DEGREE_SHIFT  20u
#define CARRIER40_PATH_MASK     0xFFFFFu

/* Masks for 5-bit fields */
#define CARRIER40_FIELD_MASK    0x1Fu

typedef struct {
    uint8_t groups5[8];       /* 8 x 5-bit groups = 40 bits */
} carrier40_t;

typedef struct {
    uint8_t basis;            /* 5 bits */
    uint8_t rank;             /* 5 bits */
    uint8_t group;            /* 5 bits */
    uint8_t degree;           /* 5 bits */
    uint32_t path;            /* 20 bits */
} carrier40_fields_t;

/* Convert between 64-bit int and 40-bit codepoint */
carrier40_t carrier40_from_uint64(uint64_t v);
uint64_t    carrier40_to_uint64(carrier40_t c);

/* Pack/unpack field view */
carrier40_t       carrier40_pack(carrier40_fields_t f);
carrier40_fields_t carrier40_unpack(carrier40_t c);

/* Validate: top 24 bits must be zero */
bool carrier40_valid(uint64_t v);

/* Format as 10-char hex string (null-terminated) */
void carrier40_format(carrier40_t c, char out[11]);

/* Parse from 10-char hex string */
bool carrier40_parse(const char hex[11], carrier40_t *out);

#endif /* CARRIER40_H */
