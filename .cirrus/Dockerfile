FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.5_8_1.9.0_3.3.0

RUN apt-get update && apt-get install -y nodejs npm
RUN npm i -g --force node
RUN npm install -g npm
RUN apt-get remove -y npm
RUN apt-get install -y npm
