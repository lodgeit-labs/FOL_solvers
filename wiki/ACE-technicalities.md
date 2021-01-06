# installation:
```
sudo apt install docker.io
docker build  -f  Dockerfile.txt  .
docker run  -p 9077:9077 -it <hash>
```
https://github.com/AceWiki/AceWiki/blob/master/docker/plain/Dockerfile

## updated version:
```
FROM ubuntu:latest
WORKDIR /opt/application
RUN apt-get -y update
RUN apt-get install -y wget git ant
RUN apt-get install -y swi-prolog swi-prolog-java
RUN apt-get install -y openjdk-8-jdk
RUN git clone --depth=1 https://github.com/Attempto/APE.git
WORKDIR /opt/application/APE
RUN make build
WORKDIR /opt/application
RUN git clone --depth=1 https://github.com/AceWiki/AceWiki
WORKDIR /opt/application/AceWiki
RUN ant createwebapps
RUN mv ../APE/ape.exe .
RUN wget -O jetty-runner.jar http://repo2.maven.org/maven2/org/mortbay/jetty/jetty-runner/8.1.9.v20130131/jetty-runner-8.1.9.v20130131.jar
#ENV LD_PRELOAD /usr/lib/swi-prolog/lib/amd64/libjpl.so
#ENV LD_LIBRARY_PATH /usr/lib/jvm/default-java/jre/lib/amd64:/usr/lib/jvm/default-java/jre/lib/amd64/server
EXPOSE 9077
CMD LD_PRELOAD=/usr/lib/libswipl.so LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/amd64/libjpl.so java -Xmx400m -Xss4m -Djava.library.path=/usr/lib/swi-prolog/lib/amd64/ -Djava.awt.headless=true -jar jetty-runner.jar --port 9077 --jar /usr/lib/swi-prolog/lib/jpl.jar acewiki.war
```

## even better version
```
# this should run acewiki without apparent errors.
# java7 is only available in this old ubuntu release.
# the old jetty throws errors with newer java.
# newer jetty does not find some xmls or whatever.
# the then-distro-current swipl's jpl throws an error, 7.6.4 from ppa works

# adding new words in aceeditor sometimes works, but mostly not. This is also the case in the official demo instance. Acewiki supports a smaller range of grammar.

but adding new words still doesn't work in aceeditor, only in the wiki. It works on their demo server.


FROM ubuntu:14.04

RUN apt-get -y update
CMD apt-get install software-properties-common
CMD apt-add-repository -y ppa:swi-prolog/stable
CMD  apt-get -y update

RUN apt-get install -y openjdk-7-jdk
RUN apt-get install -y wget git ant swi-prolog swi-prolog-java

WORKDIR /opt/application
RUN git clone --depth=1 https://github.com/Attempto/APE.git
WORKDIR /opt/application/APE
RUN make build
WORKDIR /opt/application
RUN git clone --depth=1 https://github.com/AceWiki/AceWiki
WORKDIR /opt/application/AceWiki
RUN ant createwebapps
RUN cp ../APE/ape.exe .
RUN wget -O jetty-runner.jar http://repo2.maven.org/maven2/org/mortbay/jetty/jetty-runner/8.1.9.v20130131/jetty-runner-8.1.9.v20130131.jar
EXPOSE 9077

#RUN apt-get install -y ssh
#EXPOSE 22

CMD /bin/bash
#CMD LD_PRELOAD=/usr/lib/libswipl.so LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/amd64/ java -Xmx400m -Xss4m -Djava.library.path=/usr/lib/swi-prolog/lib/amd64/ -Djava.awt.headless=true -jar jetty-runner.jar   --port 9077 --jar /usr/lib/swi-prolog/lib/jpl.jar acewiki.war
```


```Multilingual ACE wiki Grammar is editable provided that the GF source files have been downloaded into the AceWiki data-directory. See the servlet definition for more information.```
```Configuration of an editable wiki. The only difference is that
      the grammar directory on the GF service is a subdirectory of /tmp
      (because currently only such subdirectories are editable).
      
      Additionally we need to pull all the GF grammar source files into
      the AceWiki data directory. Future versions of AceWiki-GF might do
      this automatically. For now you can use a Python script from
      https://github.com/Kaljurand/GF-Utils
      
      python copy_gf_to_data.py \
               -s http://cloud.grammaticalframework.org:80 \
               -d /tmp/gfse.123456789/ \
               -e "gf" \
               -o path_to_acewiki_data/data/gf__Grammar/
               
      where the out-directory name matches the ontology-parameter.
```
