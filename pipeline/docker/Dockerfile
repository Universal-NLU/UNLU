FROM ubuntu:20.04
ARG GLUESEMREPO="https://github.com/RnDevelover/GlueSemWorkbench_v2.git"
ARG GLUESEMBRANCH="natural-deduction"
ARG MAVENVER="3.8.7"
ARG MAVENURL="https://dlcdn.apache.org/maven/maven-3/$MAVENVER/binaries/apache-maven-$MAVENVER-bin.tar.gz"
ARG MAVENPATH="apache-maven-$MAVENVER"
ENV TZ=Europe/Oslo
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
   && echo $TZ > /etc/timezone \
   && mkdir /unlu \
   && mkdir /unlu/bin
RUN apt update 
COPY src /unlu/src
COPY docker /unlu/docker
COPY run_pipeline.sh /unlu/
RUN apt install --yes --no-install-recommends "openjdk-8-jdk" "python3.8" "python3-regex" "locales" "curl" "git" "ca-certificates" \
   && cd /unlu \
   && curl "$MAVENURL" --output "/unlu/$MAVENPATH-bin.tar.gz" \
   && tar -xzvf "$MAVENPATH-bin.tar.gz" \
   && rm "$MAVENPATH-bin.tar.gz" \
   && git clone "$GLUESEMREPO" \
   && cd /unlu/GlueSemWorkbench_v2 \
   && git fetch \
   && git switch "$GLUESEMBRANCH" \
   && chmod +x "/unlu/$MAVENPATH/bin/mvn" \
   && "/unlu/$MAVENPATH/bin/mvn" package \
   && chown -R root:root /unlu \
   && mv /unlu/GlueSemWorkbench_v2/target/GlueSemWorkbench-1.0-SNAPSHOT-jar-with-dependencies.jar /unlu/bin/glueSemWorkbench2.jar \
   && rm -rf /unlu/GlueSemWorkbench_v2 \
   && rm -rf "/unlu/$MAVENPATH" \
   && rm -f "/unlu/$MAVENPATH" \
   && rm -rf /root/.m2 \
   && cd /unlu/src/ud2drs \
   && curl -sSL https://get.haskellstack.org/ | sh \
   && stack install \
   && chown -R root:root /unlu \
   && rm -rf /usr/local/bin/stack \
   && rm -rf /root/.stack \
   && apt purge --yes "curl" "git" "ca-certificates" "curl" "g++" "gcc" "libc6-dev" "libffi-dev" "libgmp-dev" "make" "xz-utils" "zlib1g-dev" "gnupg" "netbase" \
   && apt autoremove --yes \
   && apt clean \
   && apt install --yes --no-install-recommends "openjdk-8-jdk"  \
   && locale-gen "en_US.UTF-8" \
   && chmod +x /unlu/run_pipeline.sh \
   && chmod +x /unlu/docker/container_pipeline_starter.sh \
   && chown -R root:root /unlu 
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV PATH="/root/.local/bin:${PATH}"
WORKDIR /unlu
