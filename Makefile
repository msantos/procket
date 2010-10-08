
REBAR=$(shell which rebar || echo ./rebar)

all: $(REBAR) dirs compile

./rebar:
	erl -noshell -s inets start \
		-eval 'httpc:request(get, {"http://hg.basho.com/rebar/downloads/rebar", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

dirs:
	@mkdir -p priv/tmp

compile:
	@$(REBAR) compile

clean:  
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

