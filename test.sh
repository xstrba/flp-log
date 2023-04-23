#!/bin/bash

##########################################
# Hamiltonovská kružnice                 #
#                                        #
# @file test.sh                          #
# @author Boris Štrbák (xstrba05)        #
#                                        #
# Run tests flp22-log                    #
##########################################

for inFile in ./test_in/*
do
    outFile="${inFile/test_in/test_out}"
    if [[ -f $outFile ]]; then
        echo ""
        echo Running test "$inFile"
        diff "${outFile}" <(./flp22-log < ${inFile}) && echo "Test OK"
    fi;
done