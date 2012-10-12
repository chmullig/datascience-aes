import csv
import nltk
import sys
import os
import os.path
import syllables
from nltk.corpus import wordnet
import enchant
import argparse
import random

syl = syllables.cmusyllables()
syl.Load()

enchantDict = enchant.Dict("en_US")

PUNCTUATION = set(('.', ',', '"', "'", '`', ':', ';', '!', '~', '-', '=', '+', '?',
    '(', ')', '[', ']', '{', '}', '<', '>', '*', '^', '%', '_', '|', "@",
    '\xe2', '\x80', '\x98', '\xe2', '\x80', '\x99', '\xe2', '\x80', '\x9c', '\xe2', '\x80', '\x9d'
    ))

parser = argparse.ArgumentParser()
parser.add_argument('--max', '-n', type=int, help="Maximum number of lines before bailing")
parser.add_argument('--sample', '-s', type=float, help="Sample S*100% of the rows")
parser.add_argument('inputFilename')
args = parser.parse_args()
maxRows = args.max
sample = args.sample
inputFilename = args.inputFilename

input = csv.reader(open(inputFilename), delimiter="\t")
header = input.next()

keys = ["essay_id", "essay_set", "essay", "rater1_domain1", "rater2_domain1", "domain1_score",
    "num_chars", "num_sents", "num_words", "num_syl", "num_correctly_spelled", "fk_grade_level"]
outputFilename = os.path.splitext(os.path.basename(inputFilename))[0] + "_tagged.csv"
output = csv.DictWriter(open(outputFilename, "w"), keys)
output.writerow(dict(zip(keys, keys)))


i = 0
for row in input:
    if sample and random.random() > sample:
        continue
    result = dict(zip(
        ["essay_id", "essay_set", "essay", "rater1_domain1", "rater2_domain1", "domain1_score"],
        (row[0], row[1], row[2], row[3], row[4], row[6])))
    text = row[2].strip().decode('cp1252')
    sys.stdout.write("\r %s#%s" % (row[1], row[0]))
    sys.stdout.flush()

    result["num_chars"] = len(text)

    sents = nltk.sent_tokenize(text)
    num_sents = len(sents)
    result["num_sents"] = num_sents

    words_in_sentances = [nltk.word_tokenize(sentance) for sentance in sents]
    words = []
    for sent in words_in_sentances:
        for word in sent:
            if word not in PUNCTUATION:
                words.append(word)
            elif not all(char in PUNCTUATION for char in word):
                words.append(word)
    num_words = len(words)
    result["num_words"] = num_words

    num_correctly_spelled = 0
    for word in words:
        try:
            if enchantDict.check(word):
                num_correctly_spelled += 1
        except enchant.errors.Error:
            print "can't spell check", word
    result["num_correctly_spelled"] = num_correctly_spelled


    num_syl = 0
    for word in words:
        num_syl += syl.SyllableCount(word)
    result["num_syl"] = num_syl

    fk_grade_level = (0.39 * (num_words / num_sents)) \
        + (11.8 * (num_syl / num_words)) - 15.59
    result["fk_grade_level"] = fk_grade_level

    output.writerow(result)

    print text
    print sents
    print words_in_sentances
    print words

    i+= 1
    if maxRows and i >= maxRows:
        break

print
