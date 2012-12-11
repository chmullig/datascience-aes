#!/usr/bin/env python 
import csv
import sys

origFile = "test.tsv"
origLength = len(open(origFile, 'rU').readlines())

filename = sys.argv[1]

csvfile = csv.reader(open(filename))
ok = True

extremes = [
    None,
    (2, 12),
    (0, 4),
    (0, 3),
    (0, 3),
    (0, 4),
]

headers = csvfile.next()
if len(headers) != 4:
    ok = False
    print "Incorrect headers! %s" % ",".join(headers)
print "Assuming %s is id, %s is set, %s is weight, %s is predictedgrade" % (headers[0], headers[1], headers[2], headers[3])

i = 2
for row in csvfile:
    pred = int(row[3])
    if row[3] == "" or not (pred in range(12)):
        ok = False
        print "oh shit, issue with row %s:   %s" % (i, ','.join(row))
    setN = int(row[1])
    if pred < extremes[setN][0]:
        ok = False
        print "row %s has a score too low!   %s" % (i, ','.join(row)) 
    elif pred > extremes[setN][1]:
        ok = False
        print "row %s has a score too high!   %s" % (i, ','.join(row)) 
    i += 1

print "Found %s rows. %s\n" % (i-1, "same" if i-1 == origLength else "DIFFERENT!")

if i-1 != origLength:
    ok = False
if ok:
    print "Everything is ok, submit away baby!"
    sys.exit(1)
else:
    print "OH GOD PANIC IT'S ALL GONE TO HELL!" 
