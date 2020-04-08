FROM haskell as builder

# Download and build dependencies.
COPY stack.yaml stack.yaml.lock ./
RUN echo "name: bel" > package.yaml
RUN stack setup
# Two groups b/c building all at once runs out of memory.
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

# Build our code.
COPY . .
RUN stack install chime:playground

# Copy into minimal image.
FROM fpco/haskell-scratch:integer-gmp
WORKDIR /root/
COPY --from=builder /root/.local/bin/playground .
COPY --from=builder /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libtinfo.so.5

# Entrypoint.
ENV PORT 8080
CMD ["/root/playground"]
