# PRELUDE_PATH := $(shell realpath vendor/prelude/js/src/prelude.clj)
# WRANGLER_DIR := .github
# SRC_DIRS := vendor/packages/cf-xmlparser vendor/packages/effects vendor/packages/signals vendor/packages/edn vendor/packages/rec_json src test
OUT_DIR := .github/bin

.PHONY: test
test: build
	@ echo '{"type": "module", "devDependencies": {"wrangler": "^3.99.0"}}' > $(OUT_DIR)/package.json
	@ cd .github/bin && yarn && clear
	@ cd .github && node --env-file=.dev.vars bin/test/test.js

.PHONY: build
build:
	@ export OCAMLRUNPARAM=b && \
		clj2js compile -src build.clj -target repl > .github/build.gen.sh
	@ chmod +x .github/build.gen.sh
	@ .github/build.gen.sh

.PHONY: clean
clean:
	@ rm -rf .github/bin

# Генерация .js файлов из .clj файлов
# .PHONY: build
# build:
# 	@ set -e; find $(SRC_DIRS) -name '*.clj' | while read clj_file; do \
# 		out_file=$(OUT_DIR)/$$(echo $$clj_file | sed 's|\.clj$$|.js|'); \
# 		mkdir -p $$(dirname $$out_file); \
# 		clj2js js $$clj_file $(PRELUDE_PATH) > $$out_file; \
# 	  done

# .PHONY: run
# run: build
# 	@ rm -rf .github/.wrangler/state/v3/kv/be1aaec7df4a4b0ebc08165cc159bd75
# 	@ cd .github && wrangler dev --port 8787 --test-scheduled

# .PHONY: deploy
# deploy: build
# 	@ cd .github && wrangler deploy

# .PHONY: scheduled
# scheduled:
# 	@ curl "http://localhost:8787/__scheduled?cron=*+*+*+*+*"

# .PHONY: test
# test: build
# 	@ echo '{"type": "module", "devDependencies": {"wrangler": "^3.38.0"}}' > .github/bin/package.json
# 	@ cd .github/bin && yarn
# # @ cd .github && clear && node --env-file=.dev.vars bin/test/test.js ../test/samples2/input/log.1.json ../test/samples2/cache
# 	@ cd .github && clear && node --env-file=.dev.vars bin/test/test.js ../test/samples2/input/log.2.json ../test/samples2/cache
# # @ clear && node --env-file=.dev.vars bin/test/test.js log2.txt

# .PHONY: migrate
# migrate:
# 	@ cd $(WRANGLER_DIR) && wrangler d1 execute COMPOSE_NEWS_DB --file=schema.sql

# .PHONY: migrate_prod
# migrate_prod:
# 	@ cd $(WRANGLER_DIR) && wrangler d1 execute COMPOSE_NEWS_DB --file=schema.sql --remote
