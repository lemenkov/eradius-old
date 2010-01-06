.PHONY: dict priv src test

all: dict priv src test

dict:
	${MAKE} -C src ../ebin/eradius_dict.beam

priv:
	${MAKE} -C priv

src:
	${MAKE} -C src

test:
	${MAKE} -C test ../ebin/et.beam

clean:
	${MAKE} -C priv $@
	${MAKE} -C src  $@
	${MAKE} -C test $@
