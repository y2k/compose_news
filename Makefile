OUT_DIR := .github/bin

.PHONY: test
test: build
	@ echo '{"type": "module", "devDependencies": {"wrangler": "^3.107.2"}}' > $(OUT_DIR)/package.json
	@ cd $(OUT_DIR) && yarn && clear
	@ cd .github && node --env-file=.dev.vars bin/test/test.js

.PHONY: run
run:
	@ curl "http://localhost:8787/__scheduled?cron=*+*+*+*+*"

.PHONY: scheduled
scheduled: build
	@ rm -rf $(OUT_DIR)/../.wrangler/state
	@ cd $(OUT_DIR)/.. && \
		wrangler d1 execute COMPOSE_NEWS_DB --file schema.sql && \
		wrangler dev --test-scheduled

.PHONY: build
build:
	@ export OCAMLRUNPARAM=b && \
		clj2js compile -src build.clj -target repl > .github/Makefile
	@ $(MAKE) -f .github/Makefile

.PHONY: clean
clean:
	@ rm -rf $(OUT_DIR)
