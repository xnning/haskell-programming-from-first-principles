FORMATIN=-f markdown+lhs
FORMATOUT=-t gfm

OBJ=$(addprefix notes/, $(patsubst %.lhs, %.md, $(notdir $(wildcard lhs/*.lhs))))

.PHONY : all clean

notes/%.md: lhs/%.lhs
	-pandoc $(FORMATIN) $(FORMATOUT) $< | sed 's/sourceCode/haskell/g' > $@

all: $(OBJ)

clean:
	-rm $(OBJ)
