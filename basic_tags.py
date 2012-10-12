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
import re

syl = syllables.cmusyllables()
syl.Load()

enchantDict = enchant.Dict("en_US")

PUNCTUATION = set(('.', ',', '"', "'", '`', ':', ';', '!', '~', '-', '=', '+', '?',
    '(', ')', '[', ']', '{', '}', '<', '>', '*', '^', '%', '_', '|', "@", "`",
    '\xe2', '\x80', '\x98', '\xe2', '\x80', '\x99', '\xe2', '\x80', '\x9c', '\xe2', '\x80', '\x9d'
    ))

CONTRACTIONS = set(("'s", "wo", "n't", "'re", "'m", "'ve", "'ll", "isn"))
websites = ("myspace", "facebook", "youtube", "e-mail", "google", "hand-eye", "eye-hand", "webcam", "microsoft", "caps1", "yahoo")
SPECIAL_WORDS = set(("e-mail", "hand-eye", "eye-hand", "webcam", "skype", "america"))
SPECIAL_WORDS.update(websites)
SPECIAL_WORDS.update(x + ".com" for x in websites)
SPECIAL_WORDS.update("www." + x + ".com" for x in websites)

NER_re = re.compile(r"""(?:organization|caps|date|percent|person|money|location|num|month|time)\d+$""")


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
    "num_chars", "num_sents", "num_words", "num_syl", "num_correctly_spelled", "fk_grade_level",
    "starts_with_dear"]
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
    sys.stdout.write("\r %s#%s" % (row[1], row[0]))
    sys.stdout.flush()

    text = row[2].strip().decode('cp1252').lower()

    result["num_chars"] = len(text)

    sents = nltk.sent_tokenize(text)
    num_sents = len(sents)
    result["num_sents"] = num_sents

    words_in_sentances = [nltk.word_tokenize(sentance) for sentance in sents]
    words = []
    for sent in words_in_sentances:
        for word in sent:
            if word not in PUNCTUATION and not all(char in PUNCTUATION for char in word):
                words.append(word)
    num_words = len(words)
    result["num_words"] = num_words


    num_correctly_spelled = 0
    for word in words:
        try:
            if enchantDict.check(word) or NER_re.match(word) or word in CONTRACTIONS or word in SPECIAL_WORDS:
                num_correctly_spelled += 1
            # else:
            #     print word.encode('utf-8')
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

    if words[0] == 'dear':
        result["starts_with_dear"] = 1
    else:
        result["starts_with_dear"] = 0

    output.writerow(result)

    # print text
    # print sents
    # print words_in_sentances
    # print words

    i+= 1
    if maxRows and i >= maxRows:
        break

print
