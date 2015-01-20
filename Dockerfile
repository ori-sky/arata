FROM flitter/init

RUN apt-get update &&\
    apt-get install -y ghc cabal-install make

RUN cabal update &&\
    cabal install connection &&\
    cabal install acid-state &&\
    cabal install configfile &&\
    cabal install mtl &&\
    cabal install containers &&\
    cabal install ixset

ADD . /arata

RUN cd /arata && make build

ADD target/docker/arata /etc/service/arata/run

CMD /sbin/my_init
