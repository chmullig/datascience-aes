import csv
import nltk
import sys
import os
import os.path
import time
import syllables
from nltk.corpus import wordnet
import enchant
import argparse
import random
import re
import collections
import itertools
import multiprocessing
import cPickle

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
NERs = ["person", "organization", "location", "date", "time", "money", "percent", "caps", "num", "month"]

keys = ["id", "set", "essay", "rate1", "rate2", "grade",]
keys.extend("ner_%s" % x for x in NERs)

def processRow(row):
    result = dict(zip(
        ["id", "set", "essay", "rate1", "rate2", "grade",],
        row))
    sys.stdout.write("\r %s#%s" % (row[1], row[0]))
    sys.stdout.flush()

    text_asis = row[2].decode('cp1252', 'ignore')
    text = row[2].strip().decode('cp1252', 'ignore').lower()

    #frequencies of NER
    for ner in NERs:
        matches = re.findall(r"@%s\d+\b" % ner.upper(), text_asis)
        result["ner_%s" % ner] = len(matches)

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

    return result

class Worker(multiprocessing.Process):
    """
    Process the input queue of CSV rows with processRow(row), putting
    the output on a separate output queue. When it encounters None it knows the
    queue is depleted and it should quit, but first it puts a None on the output
    so the output processor knows it's done.
    """
    def __init__(self, input_queue, result_queue):
        multiprocessing.Process.__init__(self)
        self.input_queue = input_queue
        self.result_queue = result_queue

    def run(self):
        while True:
            row = self.input_queue.get()
            if row is None:
                self.result_queue.put(None)
                break
            else:
                result = processRow(row)
                self.result_queue.put(result)

class OutputWorker(multiprocessing.Process):
    """
    Processes the output queue and writes the dictionaries to a CSV. Looks for
    n_workers occurrences of None on the queue to indicate that it's done and
    should quit.
    """
    def __init__(self, result_queue, out_csv, n_workers, outfile):
        multiprocessing.Process.__init__(self)
        self.result_queue = result_queue
        self.out_csv = out_csv
        self.n_done = 0
        self.n_workers = n_workers
        self.allrows = []
        self.outfile = outfile

    def run(self):
        while True:
            result = self.result_queue.get()
            if result is None:
                self.n_done += 1
                if self.n_done == self.n_workers:
                    self.outfile.flush()
                    self.outfile.close()
                    cPickle.dump(self.allrows, open("something.pickle", "wb"), cPickle.HIGHEST_PROTOCOL)
                    print #clear the output line since it's time to quit
                    break
            else:
                self.out_csv.writerow(result)
                self.allrows.append(result)


def main():
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

    outputFilename = os.path.splitext(os.path.basename(inputFilename))[0] + "_ner.csv"
    outfile = open(outputFilename, "w")
    output = csv.DictWriter(outfile, keys)
    output.writerow(dict(zip(keys, keys)))
    outfile.flush()

    input_queue = multiprocessing.Queue(20)
    result_queue = multiprocessing.Queue()
    n_workers = multiprocessing.cpu_count()
    workers = []
    for i in range(n_workers):
        worker = Worker(input_queue, result_queue)
        worker.start()
        workers.append(worker)

    output_worker = OutputWorker(result_queue, output, n_workers, outfile)
    output_worker.start()
    workers.append(output_worker)

    for i, row in enumerate(input):
        if not (sample and random.random() > sample):
            input_queue.put(row)
        if maxRows and i >= maxRows:
            break

    for i in range(n_workers):
        input_queue.put(None)
    # while not input_queue.empty() or not result_queue.empty():
    #     time.sleep(5)

    # for worker in workers:
    #     worker.terminate()


if __name__ == "__main__":
    main()
