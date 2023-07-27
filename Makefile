.PHONY: release test build_wrangler wrangler wrangler_schedule

release:
	dune clean && dune test && dune build bin --profile=release

test:
	dune build && OCAMLRUNPARAM=b dune test

build_wrangler:
	dune build bin

wrangler:
	wrangler dev --test-scheduled

wrangler_schedule:
	curl "http://localhost:8787/__scheduled?cron=*+*+*+*+*"
