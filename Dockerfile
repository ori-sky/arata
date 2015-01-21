FROM flitter/init

RUN apt-get update &&\
    apt-get install -y make ghc cabal-install

RUN cabal update

# Cache the dependencies discretely
ADD arata.cabal /arata/arata.cabal
RUN cd /arata && cabal install --only-dependencies

ADD . /arata/src
RUN cd /arata/src && cabal install &&\
    cp dist/* -vrf .. &&\
    rm -rf /arata/src /arata/arata.cabal

ADD target/docker/arata /etc/service/arata/run

CMD /sbin/my_init
