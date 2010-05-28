.PHONY: dict priv src test

all: dict priv src test
	cp eradius.app ./ebin/

dict:
	${MAKE} -C src ../ebin/eradius_dict.beam

priv: dict
	${MAKE} -C priv

src: priv
	${MAKE} -C src

test: priv
	${MAKE} -C test ../ebin/et.beam

clean:
	${MAKE} -C priv $@
	${MAKE} -C src  $@
	${MAKE} -C test $@
