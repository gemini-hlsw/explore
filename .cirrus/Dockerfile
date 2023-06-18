FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.5_8_1.9.0_3.3.0

RUN touch ~/.bashrc && chmod +x ~/.bashrc
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash 
RUN . ~/.nvm/nvm.sh && source ~/.bashrc && nvm install node
