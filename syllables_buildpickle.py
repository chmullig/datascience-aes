#!/usr/bin/env python
 
from curses.ascii import isdigit
from nltk.corpus import cmudict
try:
    import cPickle as pickle
except:
    import pickle
 
#-----
# Create a shared dictionary key's on the word with the value as a list of
# possible syllable counts
 
GzzCMUDict = cmudict.dict()
 
GdcSyllableCount = {}
 
def CreatePickle(AlgQuiet=False):
 
    def SyllableCount(AszWord):
        """return the max syllable count in the case of multiple pronunciations"""
 
        #http://groups.google.com/group/nltk-users/msg/81e70cb6704dc01e?pli=1
 
        return [len([y for y in x if isdigit(y[-1])]) for x in GzzCMUDict[AszWord.lower()]]
 
    # try:
    #     LhaInputFile = open('cmudict','r+')
    # except:
    #     print "Could not open the cmudict file"
    #     raise IOError
 
    try:
        for LszLine in cmudict.words():
 
            LszWord = LszLine.split(' ')[0].lower()
 
            LliSyllableList = SyllableCount(LszWord)
 
            if LszWord not in GdcSyllableCount:
                GdcSyllableCount[LszWord] = sorted(LliSyllableList)
                if not AlgQuiet:
                    print "%-20s added %s" % (LszWord, LliSyllableList)
            else:
                if not AlgQuiet:
                    print "  -Word (%s) found twice. First count was %s, second was %s" % (LszWord, GdcSyllableCount[LszWord], LliSyllableList)
    except:
        print "An error was encountered processing the file."
        raise IOError
 
    try:
        #-----
        # Now write the dictionary away to a new pickle file
 
        LhaOutputFile = open('cmusyllables.pickle','w')
 
        if not AlgQuiet:
            print "Finished processing input file\n\nNow dumping pickle file\n"
        pickle.dump(GdcSyllableCount, LhaOutputFile,-1)
 
        if not AlgQuiet:
            print "Pickle file cmusyllables.pickle has been created."
    except:
        print "An error was encountered writing the pickle file."
        raise IOError
 
def main():
    #-----
    # Open the CMU file and for each entry create a dict with the resulting
    # number of syallbles
 
    CreatePickle()
 
if __name__ == '__main__':
    main()