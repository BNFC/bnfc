FROM ubuntu:17.10

RUN apt-get update && \
    apt-get -y install --no-install-recommends \
      locales=2.26-0ubuntu2 \
      haskell-platform=2014.2.0.0.debian4 \
      flex=2.6.1-1.3 \
      bison=2:3.0.4.dfsg-1build1 \
      openjdk-8-jdk=8u151-b12-0ubuntu0.17.10.2 \
      jflex=1.6.1-2 \
      build-essential=12.4ubuntu1 \
      ocaml=4.04.0-2ubuntu4 \
      doctest=0.11.3-1 \
      python-pygments=2.2.0+dfsg-1 \
      python-setuptools=36.2.7-2 \
      virtualenv=15.1.0+ds-1 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8

ENV PATH /root/.cabal/bin:$PATH
RUN cabal update

RUN mkdir -p /bnfc/source && \
    mkdir -p /bnfc/testing

# install dependencies
WORKDIR /bnfc/source
COPY source/LICENSE LICENSE
COPY source/Setup.lhs Setup.lhs
COPY source/BNFC.cabal BNFC.cabal
RUN cabal install --only-dependencies --enable-tests

WORKDIR /bnfc/testing
COPY testing/LICENSE LICENSE
COPY testing/Setup.hs Setup.hs
COPY testing/bnfc-system-tests.cabal bnfc-system-tests.cabal
RUN cabal install --only-dependencies
RUN cabal install hlint

# install bnfc
WORKDIR /bnfc/source
COPY source/src src
COPY source/test test
COPY source/runtime runtime

RUN  cabal configure --enable-tests && \
     cabal build && \
     cabal test && \
     cabal install

# setup system tests
COPY examples /bnfc/examples
WORKDIR /bnfc/testing
COPY testing/data data
ENV CLASSPATH /bnfc/testing/data/javatools.jar:$CLASSPATH
ENV CLASSPATH /bnfc/testing/data/java-cup-11b-runtime.jar:$CLASSPATH
COPY testing/regression-tests regression-tests
COPY testing/src src
COPY testing/Main.hs Main.hs

# run system tests
CMD cabal configure --enable-tests && \
    cabal build && \
    cabal run
