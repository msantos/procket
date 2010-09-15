
REBAR=./rebar
TEMPLATE=rebar.config.tmpl
CONFIG=rebar.config

ARCH=-m$(shell erl -noshell -eval 'io:format("~w", [erlang:system_info(wordsize)*8])' -eval 'halt()')

all: config compile

config:
	@sed 's/@ARCH@/$(ARCH)/' $(TEMPLATE) > $(CONFIG)

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

