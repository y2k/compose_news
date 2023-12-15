FROM y2khub/compose-news-base

WORKDIR /app

COPY --chown=opam Makefile /app
COPY --chown=opam dune-project /app
COPY --chown=opam bin /app/bin
COPY --chown=opam lib /app/lib
COPY --chown=opam test /app/test

RUN eval $(opam env) && sudo dune clean && sudo dune build --profile=release

FROM y2khub/compose-news.wrangler

# Run Tests
COPY --from=0 /app/_build/default/test/tests.bc.js .
COPY --from=0 /app/test/samples test/samples
RUN ls -la
RUN node tests.bc.js

# Deploy to Cloudflare
COPY --from=0 /app/_build/default/bin/main.bc.js .

ARG CF_TOKEN

RUN CLOUDFLARE_API_TOKEN=$CF_TOKEN node_modules/.bin/wrangler deploy --node-compat --compatibility-date 2023-07-17 --name compose_news main.bc.js
