CC ?= cc
CFLAGS ?= -std=c11 -O2 -Wall -Wextra -pedantic
LDFLAGS ?= -lm

SRC := src/main.c src/polyform.c src/svg.c src/carrier40.c src/polytope_codepoint_bridge.c
OBJ := $(SRC:.c=.o)
BIN := polyform-cli
INCLUDES := include/polyform.h include/svg.h include/carrier40.h include/polytope_codepoint_bridge.h

all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) -Iinclude -o $@ $(OBJ) $(LDFLAGS)

src/%.o: src/%.c $(INCLUDES)
	$(CC) $(CFLAGS) -Iinclude -c $< -o $@

examples: $(BIN)
	mkdir -p out
	./$(BIN) polyform-svg 123456789a out/polyform.svg
	./$(BIN) beetag-svg 1234 out/beetag.svg
	./$(BIN) aztec-svg 123456789a out/aztec_like.svg
	./$(BIN) maxicode-svg 123456789a out/maxicode_like.svg
	./$(BIN) smith-svg out/smith_chart.svg
	./$(BIN) rods-svg 7 out/genaille_division_rods.svg
	./$(BIN) guess-svg 1 127 out/binary_guess_number_trick_SMIL.svg

toolbox-svgs: $(BIN)
	mkdir -p out/toolbox
	./$(BIN) polyform-svg 0000000000 out/toolbox/simplex-0d.svg
	./$(BIN) polyform-svg 0800000000 out/toolbox/cube-3d.svg
	./$(BIN) polyform-svg 1000000000 out/toolbox/cross-4d.svg
	./$(BIN) polyform-svg 1800000000 out/toolbox/exceptional-24cell.svg

polytope-svg: $(BIN)
	./$(BIN) polyform-svg $(ID) out/polytope-$(ID).svg

clean:
	rm -f $(BIN) src/*.o out/*.svg out/toolbox/*.svg

.PHONY: all clean examples toolbox-svgs polytope-svg
