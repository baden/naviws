# See LICENSE for licensing information.

PROJECT = naviws

# Options.
# -Werror
ERLC_OPTS ?= +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec \
	+'{parse_transform, lager_transform}'
# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config
PLT_APPS = crypto public_key ssl

# Dependencies.

# bear not got automaticaly by folsom, bug?
DEPS = lager cowboy jsx jsxn

#dep_cowboy = git git://github.com/ninenines/cowboy.git 2.0.0-pre.1
dep_cowboy = git git://github.com/baden/cowboy.git master
dep_jsx = git git://github.com/baden/jsx.git develop
dep_jsxn = git git://github.com/talentdeficit/jsxn.git v2.1.1

# TEST_DEPS = ct_helper gun
# TEST_DEPS = gun
TEST_DEPS = navidb websocket_client
# dep_ct_helper = git https://github.com/extend/ct_helper.git master
dep_websocket_client = git git://github.com/jeremyong/websocket_client.git master
dep_navidb = git git://github.com/baden/navidb.git refresh2017

include erlang.mk

# Also dialyze the tests.
# DIALYZER_OPTS += --src -r test
test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s naviws -config test/test.config
