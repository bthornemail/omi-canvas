#include "polytope_codepoint_bridge.h"
#include <string.h>

uint8_t category_to_basis(const char *category) {
    if (!category) return 0;
    unsigned long h = 0;
    for (const char *p = category; *p; p++) {
        h = h * 31 + (unsigned char)(*p);
    }
    return (uint8_t)(h & 0x1F);
}

carrier40_t polytope_to_codepoint(
    const char *category,
    int         dimension,
    int         group_idx,
    int         degree,
    uint32_t    path_sel
) {
    carrier40_fields_t f;
    f.basis  = category_to_basis(category);
    f.rank   = (uint8_t)((dimension < 0 ? 0 : dimension) & 0x1F);
    f.group  = (uint8_t)((group_idx < 0 ? 0 : group_idx) & 0x1F);
    f.degree = (uint8_t)((degree < 0 ? 0 : degree) & 0x1F);
    f.path   = path_sel & 0xFFFFF;
    return carrier40_pack(f);
}
