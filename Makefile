REBAR = $(shell which rebar || echo ./rebar)
REBAR2 = $(shell which rebar || echo ../rebar)
ERLC=erlc -I include
ERL=erl -I include -noshell -pa ebin

.PHONY: ebin
.NOTPARALLEL: compile

# Makefile targets format:
#
# 	target: dependencies
# 	[tab] system command
#
compile: ebin

rel:	dev1 dev2

dev1 dev2: test
	@ mkdir -p rel/estockd
	@ (cd rel && $(REBAR2) generate force=1 target_dir=estockd/$@ overlay_vars=vars/$@_vars.config)

clean:
	@ $(REBAR) clean

test: compile
	@ time $(REBAR) skip_deps=true eunit

ebin:
	@ $(REBAR) compile

