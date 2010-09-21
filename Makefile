
REBAR=./rebar

all: dirs compile

dirs:
	@mkdir -p priv/tmp

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

