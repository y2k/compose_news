FROM ocaml/opam:debian-11-ocaml-5.0

WORKDIR /app

RUN opam install -y dune ppx_deriving base64 re xml-light yojson angstrom js_of_ocaml js_of_ocaml-ppx

COPY Makefile /app
COPY dune-project /app
COPY bin /app/bin
COPY lib /app/lib
COPY test /app/test

USER opam

RUN eval $(opam env) && make release

FROM node:18-alpine3.17

WORKDIR /app

RUN npm install wrangler

COPY --from=0 /app/_build/default/bin/main.bc.js .

ARG CF_TOKEN

RUN CLOUDFLARE_API_TOKEN=$CF_TOKEN && node_modules/.bin/wrangler deploy --node-compat --compatibility-date 2023-07-28 --name compose_news main.bc.js
