FROM haskell as builder
WORKDIR /app

# Download and build dependencies.
COPY stack.yaml stack.yaml.lock ./
RUN echo "name: bel" > package.yaml \
    && mkdir playground \
    && echo "name: playground" > playground/package.yaml
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
RUN rm bel.cabal && rm playground/playground.cabal

RUN apt-get update
RUN apt-get install -y libssl-dev

# Build our code.
COPY . .
RUN stack install ./playground

# Copy into minimal image.
FROM debian
RUN apt update && apt install -y ca-certificates
COPY --from=builder /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libtinfo.so.5
COPY --from=builder /usr/lib/x86_64-linux-gnu/libcrypto.so.1.1 /usr/lib/x86_64-linux-gnu/libcrypto.so.1.1
COPY --from=builder /usr/lib/x86_64-linux-gnu/libssl.so.1.1 /usr/lib/x86_64-linux-gnu/libssl.so.1.1
COPY --from=builder /root/.local/bin/playground /playground

# Entrypoint.
ENV PORT 8080
CMD ["/playground"]
