FROM haskell as builder
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
RUN stack build \
  aeson \
  base64-bytestring \
  zstd
RUN rm bel.cabal

# Build our code.
COPY . .
RUN stack install chime:playground

# Copy into minimal image.
FROM fpco/haskell-scratch:integer-gmp
COPY --from=builder /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libtinfo.so.5
COPY --from=builder /root/.local/bin/playground /playground

# Entrypoint.
ENV PORT 8080
CMD ["/playground"]
