#ifndef POLYFORM_H
#define POLYFORM_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define PF_MAX_CELLS 256
#define PF_WORD5_MASK 0x1Fu
#define PF_CODEPOINT_BITS 40u

typedef enum {
    PF_BASIS_SQUARE,
    PF_BASIS_TRIANGLE,
    PF_BASIS_HEXAGON,
    PF_BASIS_RHOMBUS,
    PF_BASIS_OCTAGON_SQUARE,
    PF_BASIS_CIRCLE_ARC,
    PF_BASIS_GOLDEN_TRIANGLE,
    PF_BASIS_UNKNOWN
} pf_basis_t;

typedef enum {
    PF_RANK_PLANAR,
    PF_RANK_VOXEL,
    PF_RANK_SURFACE,
    PF_RANK_UNKNOWN
} pf_rank_t;

typedef enum {
    PF_GROUP_POLYOMINO,
    PF_GROUP_POLYHEX,
    PF_GROUP_POLYIAMOND,
    PF_GROUP_POLYCUBE,
    PF_GROUP_POLYSTICK,
    PF_GROUP_UNKNOWN
} pf_group_t;

typedef struct {
    int x;
    int y;
    int z;
} pf_cell_t;

typedef struct {
    pf_basis_t basis;
    pf_rank_t rank;
    pf_group_t group;
    unsigned degree;
    size_t cell_count;
    pf_cell_t cells[PF_MAX_CELLS];
} pf_polyform_t;

typedef struct {
    uint8_t groups5[8];
} pf_codepoint40_t;

typedef struct {
    uint8_t matrix[5][5];
    uint16_t identity15;
    uint16_t error10;
} pf_beetag_t;

bool pf_codepoint_from_u64(uint64_t value, pf_codepoint40_t *out);
uint64_t pf_codepoint_to_u64(const pf_codepoint40_t *cp);
void pf_codepoint_to_bytes5x8(const pf_codepoint40_t *cp, uint8_t out[5]);

bool pf_polyform_from_codepoint(const pf_codepoint40_t *cp, pf_polyform_t *out);
void pf_polyform_grow_gnomon(pf_polyform_t *poly);

bool pf_beetag_from_identity(uint16_t identity, pf_beetag_t *out);
int pf_beetag_hamming_distance(const pf_beetag_t *a, const pf_beetag_t *b);

const char *pf_basis_name(pf_basis_t basis);
const char *pf_rank_name(pf_rank_t rank);
const char *pf_group_name(pf_group_t group);

#endif
