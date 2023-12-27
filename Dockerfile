FROM debian:bullseye-slim as builder
RUN apt-get update
RUN apt-get install opam pkg-config libpcre3-dev -y 
#rlwrap
WORKDIR /usr/src/app2
COPY . .
RUN ls
RUN opam init -a --disable-sandboxing
RUN opam update
RUN opam install dune reason pcre --yes
RUN eval $(opam env) && dune build lisp.exe
#CMD rlwrap /usr/src/app2/_build/default/lisp.exe

FROM debian:bullseye-slim
RUN apt-get update
RUN apt-get install rlwrap -y
WORKDIR /usr/src/app
COPY --from=builder /usr/src/app2 /usr/src/app
CMD rlwrap /usr/src/app/_build/default/lisp.exe
