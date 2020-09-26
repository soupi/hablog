#!/bin/zsh

rm -f .stack-work/install/**/bin/hablog;
rm -r hablog;
stack clean;
stack build;
mkdir -p hablog;
cp -r static hablog;
cp -r _pages hablog;
cp -r _posts hablog;
BIN=`echo .stack-work/install/**/hablog`
cp $BIN hablog;
tar czvf hablog.tar.gz hablog;
