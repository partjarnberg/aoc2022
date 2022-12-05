FROM erlang:25.1.2.0-alpine as BASE
WORKDIR /src
COPY ["./src/main.erl", "./input.txt", "./"]
RUN erlc main.erl
CMD erl -noshell -s main start -s init stop