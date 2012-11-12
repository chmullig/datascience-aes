#!/usr/bin/env python 
import csv
import sys

filename = sys.argv[1]

csvfile = csv.reader(open(filename))
ok = True

headers = csvfile.next()
if len(headers) != 4:
    ok = False
    print "Incorrect headers! %s" % ",".join(headers)
print "Assuming %s is id, %s is set, %s is weight, %s is predictedgrade" % (headers[0], headers[1], headers[2], headers[3])

i = 2
for row in csvfile:
    if row[3] == "" or not (int(row[3]) in range(12)):
        ok = False
        print "oh shit, issue with row %s:   %s" % (i, ','.join(row))
    i += 1
print "..."       
if ok:
    print "Everything is ok, submit away baby!"
else:
    print "OH GOD PANIC IT'S ALL GONE TO HELL!" 