#!/usr/bin/env python

#
#  Extract completions from CSS 2 plain text spec
#
#  http://www.w3.org/TR/2011/REC-CSS2-20110607/css2.txt
#

import re, sys

css2 = sys.stdin.read()

reject_re = re.compile("[<>\\[\\]]")

for match in re.finditer("'(?P<property>[a-zA-Z-]+)'\\s+Value:\\s+(?P<values>.*?)Initial", css2, re.MULTILINE|re.DOTALL):
    if match.group(1) != 'property-name':
        print "(%s . [%s])" % (match.group(1), " ".join([ option.strip() for option in match.group(2).split("|") if not reject_re.search(option) and len(option) > 0 ]))

