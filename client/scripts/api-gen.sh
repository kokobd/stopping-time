#!/bin/sh

PROJECT_NAME="stopping-time"

set -e

CUR_DIR="$(pwd)"
cd ../server
if [ $NODE_ENV = "production" ]; then
  stack build --flag ${PROJECT_NAME}-server:production
else
  stack build
fi
stack exec js-gen -- >"${CUR_DIR}/src/${PROJECT_NAME}/api.js"
cd "${CUR_DIR}"
