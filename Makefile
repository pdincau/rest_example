PROJECT = rest_example

CT_SUITES = eunit http

DEPS = cowboy
dep_cowboy = pkg://cowboy 0.9.0

include ./erlang.mk

# Extra targets.
.PHONY: autobahn
#
autobahn: clean clean-deps deps app build-tests
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE
