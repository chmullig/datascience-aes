import csv
import nltk
import sys
import os
import os.path
import syllables
from nltk.corpus import wordnet

syl = syllables.cmusyllables()
syl.Load()

PUNCTUATION = set(('.', ',', '"', "'", '`', ':', ';', '!', '~', '-', '=', '+',
    '(', ')', '[', ']', '{', '}', '<', '>', '*', '^', '%', '_', '|',
    '\xe2', '\x80', '\x98', '\xe2', '\x80', '\x99', '\xe2', '\x80', '\x9c', '\xe2', '\x80', '\x9d'
    ))


inputFilename = sys.argv[1]
input = csv.reader(open(inputFilename), delimiter="\t")
header = input.next()

keys = ["essay_id", "essay_set", "essay", "rater1_domain1", "rater2_domain1", "domain1_score",
    "num_chars", "num_sents", "num_words", "num_syl", "num_correctly_spelled"]
outputFilename = os.path.splitext(os.path.basename(inputFilename))[0] + "_tagged.csv"
output = csv.DictWriter(open(outputFilename, "w"), keys)
output.writerow(dict(zip(keys, keys)))

for row in input:
    result = dict(zip(
        ["essay_id", "essay_set", "essay", "rater1_domain1", "rater2_domain1", "domain1_score"],
        (row[0], row[1], row[2], row[3], row[4], row[5])))
    text = row[2].strip()
    sys.stdout.write("\r %s#%s" % (row[1], row[0]))
    sys.stdout.flush()

    result["num_chars"] = len(text)

    sents = nltk.sent_tokenize(text)
    result["num_sents"] = len(sents)

    words_in_sentances = [nltk.word_tokenize(sentance) for sentance in sents]
    words = []
    for sent in words_in_sentances:
        for word in sent:
            if word not in PUNCTUATION:
                words.append(word)
            if not all(char in PUNCTUATION for char in word):
                words.append(word)
    result["num_words"] = len(words)

    num_correctly_spelled = 0
    for word in words:
        if wordnet.synsets(word):
            num_correctly_spelled += 1
    result["num_correctly_spelled"] = num_correctly_spelled


    num_syl = 0
    for word in words:
        num_syl += syl.SyllableCount(word)
    result["num_syl"] = num_syl

    output.writerow(result)



