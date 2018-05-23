FROM samdoshi/haskell-stack
ADD . /tmp/openapi-test
WORKDIR /tmp/openapi-test
RUN stack install
ENTRYPOINT ["/root/.local/bin/openapi-test-exe"]
CMD ["--feedsdir", "./rss-sample-files"]
