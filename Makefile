.PHONY: restore release wrangler schedule build_wrangler test update_base update_wrangler

restore:
	opam install . --deps-only -y

release:
	dune clean && dune build bin --profile=release

wrangler:
	wrangler dev --test-scheduled

schedule:
	curl "http://localhost:8787/__scheduled?cron=*+*+*+*+*"

test:
	dune build && OCAMLRUNPARAM=b && dune test -f

build_wrangler:
	dune build bin

update_base:
	docker build -f Dockerfile.Base -t y2khub/compose-news-base --platform linux/amd64 . && docker push y2khub/compose-news-base

update_wrangler:
	docker build -f Dockerfile.Wrangler -t y2khub/compose-news.wrangler --platform linux/amd64 . && docker push y2khub/compose-news.wrangler
