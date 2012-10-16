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
import collections
import itertools
from pos_dict import pos_dict
pos_cnt_all = collections.Counter()

syl = syllables.cmusyllables()
syl.Load()

enchantDict = enchant.Dict("en_US")

PUNCTUATION = set(('.', ',', '"', "'", '`', ':', ';', '!', '~', '-', '=', '+', '?',
    '(', ')', '[', ']', '{', '}', '<', '>', '*', '^', '%', '_', '|', "@", "`",
    '\xe2', '\x80', '\x98', '\xe2', '\x80', '\x99', '\xe2', '\x80', '\x9c', '\xe2', '\x80', '\x9d'
    ))

CONTRACTIONS = set(("'s", "wo", "n't", "'re", "'m", "'ve", "'ll", "isn"))
websites = ("myspace", "facebook", "youtube", "e-mail", "google", "hand-eye", "eye-hand", "webcam", "microsoft", "caps1", "yahoo", "wikipedia")
SPECIAL_WORDS = set(("e-mail", "hand-eye", "eye-hand", "webcam", "webcams", "skype", "powerpoint", "english", "america", "american", "netbook"))
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


input = csv.reader(open(inputFilename, "rU"), delimiter="\t")
header = input.next()

keys = ["essay_id", "essay_set", "essay", "rater1_domain1", "rater2_domain1", "domain1_score",
    "num_chars", "num_sents", "num_words", "num_syl", "sentance_length", "num_correctly_spelled", "fk_grade_level",
    "starts_with_dear", "distinct_words", "end_with_preposition",
    "num_nouns", "num_verbs", "num_adjectives", "num_adverbs", "num_conjunctions", "num_prepositions",
    "num_superlatives"]
outputFilename = os.path.splitext(os.path.basename(inputFilename))[0] + "_tagged.csv"
output = csv.DictWriter(open(outputFilename, "w"), keys)
output.writerow(dict(zip(keys, keys)))


i = 0
for row in input:
    if sample and random.random() > sample:
        continue
    result = dict(zip(
        ["essay_id", "essay_set", "essay", "rater1_domain1", "rater2_domain1", "domain1_score"],
        (row[0], row[1], row[2], row[3], row[4], row[5])))
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

    result["sentance_length"] = num_words / float(num_sents)


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

    result["distinct_words"] = len(set(words))

    #Part of Speech tagging
    tagged_sentences = [nltk.pos_tag(sent) for sent in words_in_sentances]
    pos_cnt = collections.Counter()
    for word, pos in itertools.chain(*tagged_sentences):
        pos_cnt[pos] += 1
        pos_cnt_all[pos] += 1

    #flag ending in a preposition
    result["end_with_preposition"] = 0
    for sent in tagged_sentences:
        if sent[-1][1] == "IN":
            result["end_with_preposition"] += 1

    result["num_nouns"] = sum(pos_cnt[key] for key in ("NN", "NNP", "NNS"))
    result["num_verbs"] = sum(pos_cnt[key] for key in ("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"))
    result["num_adjectives"] = sum(pos_cnt[key] for key in ("JJ", "JJR", "JJS"))
    result["num_adverbs"] = sum(pos_cnt[key] for key in ("RB", "RBR", "RBS"))
    result["num_conjunctions"] = sum(pos_cnt[key] for key in ("CC"))
    result["num_prepositions"] = sum(pos_cnt[key] for key in ("IN"))
    result["num_superlatives"] = sum(pos_cnt[key] for key in ("JJS", "RBS"))

    #TODO:
    #flag for foreign words
    #flag coordinating conjunctions
    #flag prepositions
    #flag adverbs/adjectives and superlatives
    #number of 

    
    # print text
    # print sents
    # print words_in_sentances
    # print words
    # print tagged_sentences
    

    output.writerow(result)

    i+= 1
    if maxRows and i >= maxRows:
        break

print
#print "\n".join("%s (%s): %s" % (pos, pos_dict.get(pos), cnt) for (pos, cnt) in sorted(pos_cnt_all.items()))
