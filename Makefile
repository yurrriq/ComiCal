cpif ?= | cpif

latexmk_flags = -cd -f -file-line-error -interaction=nonstopmode -shell-escape -synctex=1 -xelatex
ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags  += -gg
endif

NW_SRCS := $(wildcard src/*.nw)

DEFS := $(patsubst src/%.nw,src/%.defs,${NW_SRCS})

SRCS := $(sort $(shell perl -lne '/<<(.+\.[a-z]+)>>=/ && print $$1' ${NW_SRCS}))

srcs: ${SRCS}

${SRCS}: $(wildcard src/*.nw)
	@ notangle -R$@ ${NW_SRCS} ${cpif} $@

src/%.tex: src/%.nw # src/all.defs
	@ noweave -delay -index -latex -n $^ ${cpif} $@

src/%.defs: src/%.nw
	@ nodefs $< >$@

src/all.defs: ${DEFS}
	sort -u $^ ${cpif} $@

docs/all.pdf: src/all.tex $(addsuffix .tex,$(basename ${NW_SRCS})) VERSION
	@ mkdir -p $(@D)
	@ latexmk $(latexmk_flags) -outdir=$(CURDIR)/$(@D) $<
	@ noindex src/all
	@ latexmk $(latexmk_flags) -outdir=$(CURDIR)/$(@D) $<

all: docs/all.pdf

install:
	@ cp -vr docs/* ${PREFIX}

watch:
	@ watchman-make -s 2 -p 'src/**/*' -t srcs all

part ?= patch
.PHONY: bump-version
bump-version:
	@ semver bump ${part} $(file <VERSION) | tr -d '\n' >VERSION
