.PHONY: build
build: default.nix $(wildcard src/**.hs) $(wildcard app/**.hs)
	@ nix build -f $<


.PHONY: update
update: package ?= nixpkgs
update: sources := nix/sources.json
update: rev = $(shell jq -r '.["${package}"].rev[:8]' ${sources})
update: COMMIT_MSG_FILE = ../../../../.git/COMMIT_EDITMSG
update:
	@ niv update ${package}
	@ git add ${sources}
	@ jq '"[nix] ${package}: ${rev} -> \(.["${package}"].rev[:8])"' \
	${sources} | xargs git commit -m
