cpif ?= | cpif

latexmk_flags = -cd -f -file-line-error -interaction=nonstopmode -shell-escape -synctex=1 -xelatex
ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags  += -gg
endif

NW_SRCS := \
src/flake.nw \
src/emacs.nw \
src/haskell.nw

DEFS := $(patsubst src/%.nw,src/%.defs,${NW_SRCS})

SRCS := \
flake.nix \
emacs.el \
package.yaml \
Setup.hs \
app/Main.hs \
lib/ComiCal/App.hs

srcs: ${SRCS}

${SRCS}: $(wildcard src/*.nw)
	notangle -R$@ ${NW_SRCS} ${cpif} $@

src/%.tex: src/%.nw # src/all.defs
	noweave -delay -index -latex -n $^ ${cpif} $@

src/%.defs: src/%.nw
	nodefs $< >$@

src/all.defs: ${DEFS}
	sort -u $^ ${cpif} $@

docs/all.pdf: src/all.tex $(addsuffix .tex,$(basename ${NW_SRCS}))
	@ mkdir -p $(@D)
	latexmk $(latexmk_flags) -outdir=$(CURDIR)/$(@D) $<
	noindex src/all
	latexmk $(latexmk_flags) -outdir=$(CURDIR)/$(@D) $<

all: docs/all.pdf

install:
	@ cp -vr docs/* ${PREFIX}

watch:
	@ watchman-make -s 2 -p 'src/**/*' -t srcs all
