FROM haskell

COPY stack.yaml stack.yaml.lock ./
RUN echo "name: bel" > package.yaml
RUN stack setup
RUN stack build bytestring
RUN stack build \
  base \
  base-prelude \
  lens \
  megaparsec \
  mtl \
  text \
  transformers \
  hspec \
  HUnit
RUN stack build \
  containers \
  directory \
  file-embed \
  filepath \
  haskeline \
  parser-combinators \
  template-haskell \
  time
