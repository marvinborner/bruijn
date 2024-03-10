#!/bin/env bash

./genstd.sh
echo "std done"

./gensamples.sh
echo "samples done"

mkdocs build
echo "wiki done"
