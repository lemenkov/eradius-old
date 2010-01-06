ERL		= erl
ERLC	= erlc

all:
	${MAKE} ERLC=${ERLC} -C src ../ebin/eradius_dict.beam
	${MAKE} ERLC=${ERLC} -C priv
	${MAKE} ERLC=${ERLC} -C src
	${MAKE} ERLC=${ERLC} -C test ../ebin/et.beam

clean:
	${MAKE} -C priv $@
	${MAKE} -C src  $@
	${MAKE} -C test $@
