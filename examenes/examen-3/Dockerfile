FROM pandoc/latex:3.1.1
WORKDIR /data
RUN apk update && apk add texlive-full
RUN tlmgr install algorithmicx algorithms automata forest \
                  lipsum pgf ragged2e rsfs xcolor
ENTRYPOINT ["/data/resources/entrypoint.sh"]
