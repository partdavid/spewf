MODULES = \
	src

all docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

clean:
	rm -f *.log *.access erl_crash.dump
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done
