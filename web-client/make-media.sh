#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
diff -u \
  <(find media-src -type f -exec sha256sum {} \; | sort) \
  <(grep -v '^#' <<EOF
# https://freesound.org/people/jakebagger/sounds/499782/
e159d06f9aafe92f4dc2902fa0792086919f766ed09e6c02e0d161c468d612a6  media-src/499782__jakebagger__dropping.flac
EOF
)

ffmpeg -ss 0.1 -i media-src/499782__jakebagger__dropping.flac media/place.mp3
