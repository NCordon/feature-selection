# Eficiencia de algoritmos.
# makefile.
# Basado en: http://stackoverflow.com/questions/9787160/makefile-that-compiles-all-cpp-files-in-a-directory-into-separate-executabl
BIN=./bin
# Pseudocode
SRC=./pscode
DATA=./data
PLOT=./plots
TEX=./tex

all: $(patsubst $(SRC)/%.rb, $(TEX)/%.tex, $(wildcard $(SRC)/*.rb))

$(TEX)/%.tex: $(SRC)/%.rb
	source-highlight -f latexcolor -i $< -o $@
