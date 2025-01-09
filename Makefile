OUT_DIR := .github/bin

.PHONY: test
test: build
	@ echo '{"type": "module", "devDependencies": {"wrangler": "^3.99.0"}}' > $(OUT_DIR)/package.json
	@ cd .github/bin && yarn && clear
	@ cd .github && node --env-file=.dev.vars bin/test/test.js

.PHONY: build
build:
	@ export OCAMLRUNPARAM=b && \
		clj2js compile -src build.clj -target repl > .github/Makefile
	@ $(MAKE) -f .github/Makefile

.PHONY: clean
clean:
	@ rm -rf .github/bin
