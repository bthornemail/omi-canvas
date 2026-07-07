#include "polyform.h"
#include <string.h>

static unsigned popcount_u8(uint8_t x) {
    unsigned c = 0;
    while (x) {
        c += (x & 1u);
        x >>= 1u;
    }
    return c;
}

bool pf_codepoint_from_u64(uint64_t value, pf_codepoint40_t *out) {
    if (!out || (value >> PF_CODEPOINT_BITS) != 0) {
        return false;
    }
    for (int i = 7; i >= 0; --i) {
        out->groups5[i] = (uint8_t)(value & PF_WORD5_MASK);
        value >>= 5u;
    }
    return true;
}

uint64_t pf_codepoint_to_u64(const pf_codepoint40_t *cp) {
    if (!cp) return 0;
    uint64_t value = 0;
    for (int i = 0; i < 8; ++i) {
        value = (value << 5u) | (uint64_t)(cp->groups5[i] & PF_WORD5_MASK);
    }
    return value;
}

void pf_codepoint_to_bytes5x8(const pf_codepoint40_t *cp, uint8_t out[5]) {
    uint64_t value = pf_codepoint_to_u64(cp);
    for (int i = 4; i >= 0; --i) {
        out[i] = (uint8_t)(value & 0xFFu);
        value >>= 8u;
    }
}

static void append_cell(pf_polyform_t *poly, int x, int y, int z) {
    if (poly->cell_count < PF_MAX_CELLS) {
        poly->cells[poly->cell_count++] = (pf_cell_t){x, y, z};
    }
}

bool pf_polyform_from_codepoint(const pf_codepoint40_t *cp, pf_polyform_t *out) {
    if (!cp || !out) return false;
    memset(out, 0, sizeof(*out));

    uint8_t a = cp->groups5[0];
    uint8_t b = cp->groups5[1];
    uint8_t c = cp->groups5[2];
    uint8_t d = cp->groups5[3];
    uint8_t e = cp->groups5[4];

    out->basis = (pf_basis_t)(a % PF_BASIS_UNKNOWN);
    out->rank  = (pf_rank_t)(b % PF_RANK_UNKNOWN);
    out->group = (pf_group_t)(c % PF_GROUP_UNKNOWN);
    out->degree = (unsigned)((d % 8u) + 1u);

    int x = 0, y = 0, z = 0;
    append_cell(out, x, y, z);

    for (int i = 0; i < 8; ++i) {
        uint8_t g = cp->groups5[i];
        int dx = ((g >> 0) & 1u) ? 1 : -1;
        int dy = ((g >> 1) & 1u) ? 1 : -1;
        int dz = ((g >> 2) & 1u) ? 1 : -1;
        bool use_z = (out->rank == PF_RANK_VOXEL) || ((g >> 3) & 1u);
        unsigned repeats = 1u + ((g >> 4) & 1u);
        for (unsigned r = 0; r < repeats && out->cell_count < out->degree && out->cell_count < PF_MAX_CELLS; ++r) {
            x += dx;
            y += dy;
            if (use_z) z += dz;
            append_cell(out, x, y, z);
        }
    }

    while (out->cell_count < out->degree) {
        append_cell(out, (int)out->cell_count, 0, 0);
    }
    (void)e;
    return true;
}

void pf_polyform_grow_gnomon(pf_polyform_t *poly) {
    if (!poly || poly->cell_count >= PF_MAX_CELLS) return;
    pf_cell_t last = poly->cells[poly->cell_count ? poly->cell_count - 1 : 0];
    append_cell(poly, last.x + 1, last.y, last.z);
    poly->degree = (unsigned)poly->cell_count;
}

bool pf_beetag_from_identity(uint16_t identity, pf_beetag_t *out) {
    if (!out || identity == 0 || identity > 32767) return false;
    memset(out, 0, sizeof(*out));
    out->identity15 = identity;

    uint8_t cols[3] = {0,0,0};
    for (int bit = 0; bit < 15; ++bit) {
        unsigned shifted = (identity >> (14 - bit)) & 1u;
        int row = bit % 5;
        int col = bit / 5;
        out->matrix[row][col] = (uint8_t)shifted;
        cols[col] ^= (uint8_t)shifted;
    }

    uint8_t error5[5];
    error5[0] = cols[0] & 1u;
    error5[1] = cols[1] & 1u;
    error5[2] = cols[2] & 1u;

    uint8_t first3 = 0, last2 = 0;
    for (int row = 0; row < 5; ++row) {
        first3 ^= (out->matrix[row][0] ^ out->matrix[row][1] ^ out->matrix[row][2]);
        last2  ^= (out->matrix[row][1] ^ out->matrix[row][2]);
    }
    error5[3] = first3 & 1u;
    error5[4] = last2 & 1u;

    for (int i = 0; i < 5; ++i) out->matrix[i][3] = error5[i];
    for (int i = 0; i < 5; ++i) out->matrix[i][4] = error5[4 - i];

    uint16_t e10 = 0;
    for (int row = 0; row < 5; ++row) {
        e10 = (uint16_t)((e10 << 1u) | (uint16_t)out->matrix[row][3]);
    }
    for (int row = 0; row < 5; ++row) {
        e10 = (uint16_t)((e10 << 1u) | (uint16_t)out->matrix[row][4]);
    }
    out->error10 = e10;
    return true;
}

int pf_beetag_hamming_distance(const pf_beetag_t *a, const pf_beetag_t *b) {
    if (!a || !b) return -1;
    int dist = 0;
    for (int y = 0; y < 5; ++y) {
        for (int x = 0; x < 5; ++x) {
            dist += (a->matrix[y][x] != b->matrix[y][x]);
        }
    }
    return dist;
}

const char *pf_basis_name(pf_basis_t basis) {
    switch (basis) {
        case PF_BASIS_SQUARE: return "square";
        case PF_BASIS_TRIANGLE: return "triangle";
        case PF_BASIS_HEXAGON: return "hexagon";
        case PF_BASIS_RHOMBUS: return "rhombus";
        case PF_BASIS_OCTAGON_SQUARE: return "octagon-square";
        case PF_BASIS_CIRCLE_ARC: return "circle-arc";
        case PF_BASIS_GOLDEN_TRIANGLE: return "golden-triangle";
        default: return "unknown";
    }
}

const char *pf_rank_name(pf_rank_t rank) {
    switch (rank) {
        case PF_RANK_PLANAR: return "planar";
        case PF_RANK_VOXEL: return "voxel";
        case PF_RANK_SURFACE: return "surface";
        default: return "unknown";
    }
}

const char *pf_group_name(pf_group_t group) {
    switch (group) {
        case PF_GROUP_POLYOMINO: return "polyomino";
        case PF_GROUP_POLYHEX: return "polyhex";
        case PF_GROUP_POLYIAMOND: return "polyiamond";
        case PF_GROUP_POLYCUBE: return "polycube";
        case PF_GROUP_POLYSTICK: return "polystick";
        default: return "unknown";
    }
}
