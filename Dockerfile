FROM flitter/init

RUN apt-get update &&\
    apt-get install -y ghc cabal-install make

RUN cabal update &&\
    cabal install cabal-install &&\
    cabal install containers mtl ixset connection configfile acid-state

ADD . /arata

RUN cd /arata && make build

ADD target/docker/arata /etc/service/arata/run

CMD /sbin/my_init
