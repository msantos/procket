REBAR=$(shell which rebar || echo ./rebar)

all: dirs compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://github.com/downloads/basho/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

dirs:
	-@mkdir -p priv/tmp

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) get-deps

examples: eg
eg:
	@erlc -I deps -o ebin examples/*.erl

setuid: all
	sudo chown root priv/procket
	sudo chmod 4750 priv/procket
