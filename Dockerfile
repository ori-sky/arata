FROM flitter/init

RUN apt-get update &&\
    apt-get install -y make wget git autoconf automake libtool libgmp-dev ncurses-dev g++ llvm bzip2 zlib1g-dev &&\
    wget https://www.haskell.org/platform/download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz -O /haskell.tgz &&\
    tar xf /haskell.tgz -C / &&\
    rm /haskell.tgz &&\
    cd / && bash /usr/local/haskell/ghc-7.8.3-x86_64/bin/activate-hs &&\
    cabal update && cabal install cabal-install

# Cache the dependencies discretely
ADD arata.cabal /arata/arata.cabal
RUN cd /arata && cabal update && cabal install --only-dependencies

ADD . /arata/src
RUN cd /arata/src && cabal install &&\
    cp dist/* -vrf .. &&\
    rm -rf /arata/src /arata/arata.cabal

ADD target/docker/arata /etc/service/arata/run

CMD /sbin/my_init
