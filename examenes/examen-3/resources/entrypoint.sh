#!/bin/sh
tlmgr update --self
mkdir temp
mkdir out
pdflatex -aux-directory=temp -output-directory=out main.tex
cp out/main.pdf .
rm -rf temp out
