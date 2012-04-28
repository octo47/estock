REBAR = $(shell which rebar || echo ./rebar)
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

rel:	test
	@ $(REBAR) generate

clean:
	@ $(REBAR) clean

test: compile
	@ time $(REBAR) skip_deps=true eunit

ebin:
	@ $(REBAR) compile

