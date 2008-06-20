hc := ghc
hcflags := --make -O2 -fvia-C -optc-O2

examples := words spellchecker

all: $(examples)

words: Words.hs
	$(hc) $(hcflags) -o $@ $^

spellchecker: SpellChecker.hs
	$(hc) $(hcflags) -o $@ $^

clean:
	-rm -f *.hi *.o $(examples)
