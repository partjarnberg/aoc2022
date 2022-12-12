FROM ghcr.io/graalvm/graalvm-ce:ol9-java17-22.3.0 as BASE
WORKDIR /opt/graalvm
RUN gu install native-image
COPY src/main/java .
RUN javac *.java && native-image "--static" App

FROM alpine
COPY --from=BASE /opt/graalvm/app app
COPY input.txt .
CMD ["./app"]