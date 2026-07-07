CC ?= cc
CFLAGS ?= -std=c11 -O2 -Wall -Wextra -pedantic
LDFLAGS ?= -lm

SRC := src/main.c src/polyform.c src/svg.c
OBJ := $(SRC:.c=.o)
BIN := polyform-cli

all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $(CFLAGS) -Iinclude -o $@ $(OBJ) $(LDFLAGS)

src/%.o: src/%.c include/polyform.h include/svg.h
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

clean:
	rm -f $(BIN) src/*.o out/*.svg

.PHONY: all clean examples
