FROM haskell:9
WORKDIR /opt/skerrible
RUN cabal update
COPY ./skerrible.cabal ./skerrible.cabal
RUN cabal build --only-dependencies -j4
COPY . .
RUN cabal install
CMD ["skerrible-server", "static-root"]
