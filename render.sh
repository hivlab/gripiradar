#!/bin/bash

docker run --rm \
  -e GRIPIRADAR_ACCESS_KEY=$GRIPIRADAR_ACCESS_KEY \
  -e GRIPIRADAR_SECRET_KEY=$GRIPIRADAR_SECRET_KEY \
  -v $(pwd):/gripiradar \
  taavipall/gripiradar \
  Rscript /gripiradar/scripts/render.R
