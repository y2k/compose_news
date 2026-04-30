OUT_DIR := .github/bin

.PHONY: test
test: TEST_JS = $(patsubst test/%.clj,$(OUT_DIR)/test/%.js,$(wildcard test/*_test.clj))
test: build
	@ printf '{"type": "module", "devDependencies": {"wrangler": "^3.107.2"}}' > $(OUT_DIR)/package.json
	@ cd $(OUT_DIR) && yarn
	@ for file in $(TEST_JS); do \
		cd .github && node --env-file=.dev.vars $${file#'.github/'}; \
	done

.PHONY: build
build:
	@ mkdir -p $(OUT_DIR)
	@ ly2k compile -target eval -src build.clj > .github/Makefile
	@ $(MAKE) -f .github/Makefile > /dev/null

.PHONY: clean
clean:
	@ rm -rf $(OUT_DIR)/src
	@ rm -rf $(OUT_DIR)/test

.PHONY: run
run: build
	@ cd $(OUT_DIR)/.. && wrangler dev --port 8787

.PHONY: deploy
deploy: clean test
	@ cd $(OUT_DIR)/.. && wrangler deploy
