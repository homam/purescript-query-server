## Setup

Create a new directory

First clone and build purescript-query-dsl-parser

```
git@github.com:homam/purescript-query-dsl-parser.git
cd purescript-query-dsl-parser
spago install
spago build
```

Then clone and build this repo (it is defined in `packages.dhall`)

```
cd ..

git clone git@github.com:homam/purescript-query-server.git
cd purescript-query-server

yarn 
spago install
spago build
```

Make sure both repo's are in the same parent directory.

## Run

Environment

```
NODE_ENV="production" \
PORT=3089 \
jewel_connection_string="..." \
node index.js
```