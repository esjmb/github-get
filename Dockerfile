# in case we ever do any installs
# lets make them non-interactive
ARG DEBIAN_FRONTEND=noninteractive

###### stage 1 - build image with dependencies

# use existing haskell image as our base
FROM fpco/stack-build:lts-16.23 as base-compile-image

WORKDIR /opt/github-get
RUN stack update

# copy the yaml and cabal files
COPY ./github-get.cabal /opt/github-get
COPY ./stack.yaml /opt/github-get

# Docker will cache this command as a layer, savinf us the trouble or rebuilding
# dependencies unless we change fioles above.
RUN stack build --only-dependencies -j4

##### stage 2 - compile the code

FROM base-compile-image as compile-image

COPY . /opt/github-get
# do the build
RUN stack build --system-ghc

##### stage 3 - build small production image

FROM ubuntu:18.04 as runtime-image

ARG DEBIAN_FRONTEND=noninteractive
RUN echo "building runtime-image" && \
    apt-get update && \
    apt-get install -y libssl1.0.0 && \
    apt-get install -y netbase && \
    apt-get install -y ca-certificates
    
RUN mkdir -p /opt/github-get
WORKDIR /opt/github-get
ENTRYPOINT ["/opt/github-get/github-get-exe"]
COPY --from=compile-image /opt/github-get/.stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/github-get-exe/github-get-exe .
CMD [""]