FROM debian:bullseye-slim

ENV PORT=4242

COPY ../build/yano-bin /usr/local/bin/yano-bin

EXPOSE $PORT

CMD ["/usr/local/bin/yano-bin"]
