FROM flitter/init

RUN apt-get update &&\
    apt-get install -y make ghc cabal-install

RUN cabal update
RUN cd /arata && cabal install

ADD dist /arata
ADD target/docker/arata /etc/service/arata/run

CMD /sbin/my_init
