# Eficiencia de algoritmos.
# makefile.
# Basado en: http://stackoverflow.com/questions/9787160/makefile-that-compiles-all-cpp-files-in-a-directory-into-separate-executabl
BIN=./bin
# Pseudocode
SRC=./pscode
DATA=./data
PLOT=./plots

all: $(patsubst $(SRC)/%.rb, $(SRC)/%.tex, $(wildcard $(SRC)/*.rb))

$(SRC)/%.tex: $(SRC)/%.rb
	source-highlight -f latexcolor -i $< -o $@
