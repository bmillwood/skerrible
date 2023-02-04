FROM alpine:3 AS build
# need curl for cabal update
RUN apk add ghc cabal curl musl-dev libffi-dev zlib-dev
WORKDIR /opt/skerrible
RUN cabal update
COPY ./skerrible.cabal ./skerrible.cabal
RUN cabal build --only-dependencies -j4
COPY LICENSE .
COPY protocol protocol
COPY server server
ARG version
RUN sed -i -re 's/Nothing/Just "'"$version"'"/' server/src/Version.hs
RUN mkdir ./bin
RUN cabal install --install-method=copy --installdir=/opt/skerrible/bin

FROM alpine:3
RUN apk add gmp libffi
WORKDIR /opt/skerrible
COPY web-client/*.js web-client/*.html static-root/
COPY web-client/media/* static-root/media/
COPY --from=build /opt/skerrible/bin .
CMD ["./skerrible-server", "static-root"]
