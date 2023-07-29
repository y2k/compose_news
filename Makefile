.PHONY: release test build_wrangler wrangler schedule update_base

release:
	dune clean && dune test && dune build bin --profile=release

test:
	dune build && OCAMLRUNPARAM=b dune test

build_wrangler:
	dune build bin

wrangler:
	wrangler dev --test-scheduled

schedule:
	curl "http://localhost:8787/__scheduled?cron=*+*+*+*+*"

update_base:
	docker build -f Dockerfile.Base -t y2khub/compose-news-base . && docker push y2khub/compose-news-base
