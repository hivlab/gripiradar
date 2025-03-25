#!/bin/bash

docker run --rm \
  -e GRIPIRADAR_ACCESS_KEY=$GRIPIRADAR_ACCESS_KEY \
  -e GRIPIRADAR_SECRET_KEY=$GRIPIRADAR_SECRET_KEY \
  -v $(pwd):/work \
  taavipall/gripiradar \
  Rscript /work/scripts/render.R
rm -rf gripiradar-ut-ee
