count := 1000

all: test-$(count).out

QC: QC.hs QCSupport.hs
	ghc -Wall -O -o $@ --make $<

test-$(count).out: QC
	./QC $(count) | tee test-$(count).out

timing: hash_time HashTime

hash_time: hash_time.c ../cbits/lookup3.c
	$(CC) -I ../cbits -O3 -o $@ $^

HashTime: HashTime.hs
	ghc -O3 -o $@ --make $^

clean:
	-rm -f QC hash_time HashTime *.hi *.o test-[0-9]*.out
