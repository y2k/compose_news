# AGENTS.md

## Project Shape
- Cloudflare Worker written in `.clj` and compiled to JS with `ly2k`; `src/main.clj` exports the Worker `fetch` handler.
- Source files live in `src/`; tests live in `test/`.
- Generated outputs are `.github/bin/` and `.github/Makefile`; do not edit generated files directly.
- Runtime config is in `.github/wrangler.toml`; local secrets are expected in ignored `.github/.dev.vars`.

## Commands
- `make build` compiles `src/` and `test/`
- `make test` runs tests
- `make run` starts `wrangler dev --port 8787`

## Tests
- Tests use Node's `node:test` and mock `effect-fetch` with `f/with-fetch`; avoid real Telegram/network calls in tests.
- Snapshot expectations are base64-encoded JSON strings in `test/main_test.clj`; `assert-json-snapshot` reports the new base64 as the assertion message when output changes.

## Code Style
- Prefer inline code over extracting helpers when the logic is short, local, and used only once.
- Do not create a new function just to name 1-5 lines of straightforward code; extract only for reuse, large distracting blocks, meaningful domain boundaries, test/error boundaries, or clear readability wins.
- When there is only one call site, default to keeping the code inline; a helper with one call site and a few lines is usually worse unless it hides genuinely complex or domain-specific behavior.
