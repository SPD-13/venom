FROM haskell:8
WORKDIR /app
RUN stack build intero --copy-compiler-tool
RUN cabal update
COPY ./compiler-haskell.cabal /app/compiler-haskell.cabal
RUN cabal install --only-dependencies -j4
COPY . /app
CMD ghci
