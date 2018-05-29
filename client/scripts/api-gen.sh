#!/bin/sh

PROJECT_NAME="stopping-time"
URL_PREFIX="http://localhost:8080"
if [ $NODE_ENV = "production" ]; then
  URL_PREFIX="http://39.108.64.177:80"
fi

set -e

CUR_DIR="$(pwd)"
cd ../server
stack build
stack exec js-gen -- --url-prefix "http://localhost:8080" >"${CUR_DIR}/src/${PROJECT_NAME}/api.js"
cd "${CUR_DIR}"
