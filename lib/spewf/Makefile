MODULES = \
	src

all docs install:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

clean:
	rm -f *.access *.log
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done
