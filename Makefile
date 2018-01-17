BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/dugong
PRODRELPATH = _build/prod/rel/dugong
DEV1RELPATH = _build/dev1/rel/dugong
DEV2RELPATH = _build/dev2/rel/dugong
DEV3RELPATH = _build/dev3/rel/dugong
APPNAME = dugong
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../dugong_config
	[ -f $(RELPATH)/../dugong_config/dugong.conf ] || cp $(RELPATH)/etc/dugong.conf  $(RELPATH)/../dugong_config/dugong.conf
	[ -f $(RELPATH)/../dugong_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../dugong_config/advanced.config

console:
	cd $(RELPATH) && ./bin/dugong console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../dugong_config
	[ -f $(PRODRELPATH)/../dugong_config/dugong.conf ] || cp $(PRODRELPATH)/etc/dugong.conf  $(PRODRELPATH)/../dugong_config/dugong.conf
	[ -f $(PRODRELPATH)/../dugong_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../dugong_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/dugong console

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release
	mkdir -p $(DEV1RELPATH)/../dugong_config
	[ -f $(DEV1RELPATH)/../dugong_config/dugong.conf ] || cp $(DEV1RELPATH)/etc/dugong.conf  $(DEV1RELPATH)/../dugong_config/dugong.conf
	[ -f $(DEV1RELPATH)/../dugong_config/advanced.config ] || cp $(DEV1RELPATH)/etc/advanced.config  $(DEV1RELPATH)/../dugong_config/advanced.config

devrel2:
	$(REBAR) as dev2 release
	mkdir -p $(DEV2RELPATH)/../dugong_config
	[ -f $(DEV2RELPATH)/../dugong_config/dugong.conf ] || cp $(DEV2RELPATH)/etc/dugong.conf  $(DEV2RELPATH)/../dugong_config/dugong.conf
	[ -f $(DEV2RELPATH)/../dugong_config/advanced.config ] || cp $(DEV2RELPATH)/etc/advanced.config  $(DEV2RELPATH)/../dugong_config/advanced.config

devrel3:
	$(REBAR) as dev3 release
	mkdir -p $(DEV3RELPATH)/../dugong_config
	[ -f $(DEV3RELPATH)/../dugong_config/dugong.conf ] || cp $(DEV3RELPATH)/etc/dugong.conf  $(DEV3RELPATH)/../dugong_config/dugong.conf
	[ -f $(DEV3RELPATH)/../dugong_config/advanced.config ] || cp $(DEV3RELPATH)/etc/advanced.config  $(DEV3RELPATH)/../dugong_config/advanced.config

devrel: devrel1 devrel2 devrel3

dev1-console:
	$(BASEDIR)/_build/dev1/rel/dugong/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/dugong/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/dugong/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/dugong/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/dugong/bin/$(APPNAME)-admin cluster join dugong1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/dugong/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/dugong/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/dugong/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/dugong/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/dugong/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach

