FROM haskell
WORKDIR /app

# Download and build dependencies.
COPY stack.yaml stack.yaml.lock ./
RUN echo "name: bel" > package.yaml
RUN stack setup
# Multiple groups b/c building all at once runs out of memory,
# and for Docker cache re-use as I develop this image over time.
RUN stack build \
  base-prelude \
  bytestring \
  lens \
  megaparsec \
  mtl \
  text \
  transformers
RUN stack build \
  containers \
  directory \
  file-embed \
  filepath \
  haskeline \
  parser-combinators \
  template-haskell \
  time \
  scotty
RUN rm bel.cabal

ENV CHIME_INCLUDE_SLOW_TESTS=1
