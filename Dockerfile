FROM flitter/init

RUN apt-get update &&\
    apt-get install -y make ghc cabal-install zlib1g-dev

RUN cabal update && cabal install cabal-install &&\
    mv /root/.cabal/bin/cabal /usr/bin/cabal

# Cache the dependencies discretely
ADD arata.cabal /arata/arata.cabal
ADD Makefile /arata/Makefile

RUN cd /arata && make deps

ADD . /arata

RUN cd /arata && make install && cabal install

ADD target/docker/arata /etc/service/arata/run

ENV HOME /root

CMD /sbin/my_init
