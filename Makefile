.PHONY: release test build_wrangler wrangler

release:
	dune clean && dune test && dune build bin --profile=release

test:
	clear && OCAMLRUNPARAM=b dune test

build_wrangler:
	dune build bin

wrangler:
	wrangler dev --test-scheduled