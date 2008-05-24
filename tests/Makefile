count := 1000

all: test-$(count).out

QC: QC.hs QCSupport.hs
	ghc -Wall -O -o $@ --make $<

test-$(count).out: QC
	./QC $(count) | tee test-$(count).out

clean:
	-rm -f QC *.hi *.o test-[0-9]*.out
