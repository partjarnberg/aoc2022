FROM bellsoft/liberica-native-image-kit-container:jdk-17-nik-22.2-musl as BASE
WORKDIR /src
COPY ./ .
RUN ./gradlew nativeCompile

FROM alpine
COPY --from=BASE /src/build/native/nativeCompile/my-app my-app
COPY input.txt .
CMD ["./my-app"]