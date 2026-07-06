DUNE_DOCDIR=$(CURDIR)/_build/default/_doc/_html
LOCAL_ALLDOCDIR=$(CURDIR)/doc
LOCAL_BINDIR=$(CURDIR)/bin
LOCAL_DOCDIR=$(CURDIR)/ocamldoc
LOCAL_USRDOCDIR=$(CURDIR)/doc/usr
ifeq ($(OS),Windows_NT)
    NULL_DEVICE := nul
else
    NULL_DEVICE := /dev/null
endif

.PHONY: all build clean doc install kind2-doc test uninstall

all: build

build:
	@dune build -p kind2 @install
	@dune install -p kind2 --sections=bin --prefix . 2> NULL_DEVICE

check:
	@dune build -p kind2 --profile strict @check @install
	@dune install -p kind2 --sections=bin --prefix . 2> NULL_DEVICE

kmoxi:
	@dune build -p kmoxi @install
	@dune install -p kmoxi --sections=bin --prefix . 2> NULL_DEVICE

static:
	@LINKING_MODE=static dune build -p kind2 @install
	@dune install -p kind2 --sections=bin --prefix . 2> NULL_DEVICE

clean:
	@dune clean
	@rm -rf $(LOCAL_BINDIR) $(LOCAL_DOCDIR)

doc:
	make -C $(LOCAL_USRDOCDIR) all
	cp $(LOCAL_USRDOCDIR)/build/pdf/kind2.pdf $(LOCAL_ALLDOCDIR)/user_documentation.pdf

install:
	@opam pin add -n -y kind2 https://github.com/kind2-mc/kind2.git
	@opam depext -y kind2
	@opam install -y kind2

kind2-doc:
	@dune build @doc-private
	@dune build @copy
	@mkdir -p $(LOCAL_DOCDIR)
	@cp -rf $(DUNE_DOCDIR)/* $(LOCAL_DOCDIR)

test: build
	@dune build @runtest
	@cd $(CURDIR)/tests/ && ./run

uninstall:
	@opam remove -y kind2
	@opam unpin kind2
