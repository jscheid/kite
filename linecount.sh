#!/bin/sh

find . -name \*.el | grep -v misc | grep -v tests | xargs wc -l
