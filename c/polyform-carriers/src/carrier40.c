#include "carrier40.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

carrier40_t carrier40_from_uint64(uint64_t v) {
    carrier40_t c;
    for (int i = 0; i < 8; i++) {
        c.groups5[i] = (uint8_t)((v >> (5 * (7 - i))) & CARRIER40_GROUP_MASK);
    }
    return c;
}

uint64_t carrier40_to_uint64(carrier40_t c) {
    uint64_t v = 0;
    for (int i = 0; i < 8; i++) {
        v = (v << 5) | (c.groups5[i] & CARRIER40_GROUP_MASK);
    }
    return v;
}

carrier40_t carrier40_pack(carrier40_fields_t f) {
    uint64_t v = 0;
    v |= ((uint64_t)(f.basis  & CARRIER40_FIELD_MASK)) << CARRIER40_BASIS_SHIFT;
    v |= ((uint64_t)(f.rank   & CARRIER40_FIELD_MASK)) << CARRIER40_RANK_SHIFT;
    v |= ((uint64_t)(f.group  & CARRIER40_FIELD_MASK)) << CARRIER40_GROUP_SHIFT;
    v |= ((uint64_t)(f.degree & CARRIER40_FIELD_MASK)) << CARRIER40_DEGREE_SHIFT;
    v |= (uint64_t)(f.path & CARRIER40_PATH_MASK);
    return carrier40_from_uint64(v);
}

carrier40_fields_t carrier40_unpack(carrier40_t c) {
    uint64_t v = carrier40_to_uint64(c);
    carrier40_fields_t f;
    f.basis  = (uint8_t)((v >> CARRIER40_BASIS_SHIFT)  & CARRIER40_FIELD_MASK);
    f.rank   = (uint8_t)((v >> CARRIER40_RANK_SHIFT)   & CARRIER40_FIELD_MASK);
    f.group  = (uint8_t)((v >> CARRIER40_GROUP_SHIFT)  & CARRIER40_FIELD_MASK);
    f.degree = (uint8_t)((v >> CARRIER40_DEGREE_SHIFT) & CARRIER40_FIELD_MASK);
    f.path   = (uint32_t)(v & CARRIER40_PATH_MASK);
    return f;
}

bool carrier40_valid(uint64_t v) {
    return (v >> CARRIER40_BITS) == 0;
}

void carrier40_format(carrier40_t c, char out[11]) {
    uint64_t v = carrier40_to_uint64(c);
    snprintf(out, 11, "%010llx", (unsigned long long)v);
}

static int hex_val(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

bool carrier40_parse(const char hex[11], carrier40_t *out) {
    if (!hex) return false;
    uint64_t v = 0;
    for (int i = 0; i < 10; i++) {
        int h = hex_val(hex[i]);
        if (h < 0) return false;
        v = (v << 4) | (uint64_t)h;
    }
    if (!carrier40_valid(v)) return false;
    *out = carrier40_from_uint64(v);
    return true;
}
