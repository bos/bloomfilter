hc := ghc
hcflags := --make -O2

examples := words

all: $(examples)

words: Words.hs
	$(hc) $(hcflags) -o $@ $^

clean:
	-rm -f *.hi *.o $(examples)
