try:
    import cPickle as pickle
except:
    import pickle
 
import re
 
class cmusyllables(object):
 
    def __init__(self):
 
        #-----
        # Record the mode of the syllable count - manual / lookup
 
        self.szMode = None
 
        self.dcSyllableCount = None
 
        #-----
        # New structures for the SyllableCount3 routine
 
        self.dcSyllable3WordCache = {}
 
        self.liSyllable3SubSyllables = [
            'cial',
            'tia',
            'cius',
            'cious',
            'uiet',
            'gious',
            'geous',
            'priest',
            'giu',
            'dge',
            'ion',
            'iou',
            'sia$',
            '.che$',
            '.ched$',
            '.abe$',
            '.ace$',
            '.ade$',
            '.age$',
            '.aged$',
            '.ake$',
            '.ale$',
            '.aled$',
            '.ales$',
            '.ane$',
            '.ame$',
            '.ape$',
            '.are$',
            '.ase$',
            '.ashed$',
            '.asque$',
            '.ate$',
            '.ave$',
            '.azed$',
            '.awe$',
            '.aze$',
            '.aped$',
            '.athe$',
            '.athes$',
            '.ece$',
            '.ese$',
            '.esque$',
            '.esques$',
            '.eze$',
            '.gue$',
            '.ibe$',
            '.ice$',
            '.ide$',
            '.ife$',
            '.ike$',
            '.ile$',
            '.ime$',
            '.ine$',
            '.ipe$',
            '.iped$',
            '.ire$',
            '.ise$',
            '.ished$',
            '.ite$',
            '.ive$',
            '.ize$',
            '.obe$',
            '.ode$',
            '.oke$',
            '.ole$',
            '.ome$',
            '.one$',
            '.ope$',
            '.oque$',
            '.ore$',
            '.ose$',
            '.osque$',
            '.osques$',
            '.ote$',
            '.ove$',
            '.pped$',
            '.sse$',
            '.ssed$',
            '.ste$',
            '.ube$',
            '.uce$',
            '.ude$',
            '.uge$',
            '.uke$',
            '.ule$',
            '.ules$',
            '.uled$',
            '.ume$',
            '.une$',
            '.upe$',
            '.ure$',
            '.use$',
            '.ushed$',
            '.ute$',
            '.ved$',
            '.we$',
            '.wes$',
            '.wed$',
            '.yse$',
            '.yze$',
            '.rse$',
            '.red$',
            '.rce$',
            '.rde$',
            '.ily$',
            '.ely$',
            '.des$',
            '.gged$',
            '.kes$',
            '.ced$',
            '.ked$',
            '.med$',
            '.mes$',
            '.ned$',
            '.[sz]ed$',
            '.nce$',
            '.rles$',
            '.nes$',
            '.pes$',
            '.tes$',
            '.res$',
            '.ves$',
            'ere$'
        ]
 
        #global $split_array;
        self.liSyllable3AddSyllables  = [
            'ia',
            'riet',
            'dien',
            'ien',
            'iet',
            'iu',
            'iest',
            'io',
            'ii',
            'ily',
            '.oala$',
            '.iara$',
            '.ying$',
            '.earest',
            '.arer',
            '.aress',
            '.eate$',
            '.eation$',
            '[aeiouym]bl$',
            '[aeiou]{3}',
            '^mc','ism',
            '^mc','asm',
            '([^aeiouy])\1l$',
            '[^l]lien',
            '^coa[dglx].',
            '[^gq]ua[^auieo]',
            'dnt$'
        ]
 
        #-----
        # Create a list of the compiled regex
 
        self.liSyllable3RESubSyllables = []
        self.liSyllable3REAddSyllables = []
 
        for LszRegEx in self.liSyllable3AddSyllables:
            LreRegEx = re.compile(LszRegEx)
            self.liSyllable3REAddSyllables.append(LreRegEx)
 
        for LszRegEx in self.liSyllable3SubSyllables:
            LreRegEx = re.compile(LszRegEx)
            self.liSyllable3RESubSyllables.append(LreRegEx)
 
    def Load(self, AszFile = 'cmusyllables.pickle'):
        try:
            LhaPickleFile = open(AszFile,'rb')
 
            self.dcSyllableCount =  pickle.load(LhaPickleFile)
            #print "LOADED SYLLABLES"
        except:
            return( False )
 
        return( True )
 
    def GetRawDict(self):
        return(self.dcSyllableCount)
 
    def NonCMUSyllableCount(self, AszWord):
 
        #LszWord = self._normalize_word( AszWord.lower() )
        LszWord = AszWord
 
        #-----
        # If we've already seen this before then return the syllables
 
        if LszWord in self.dcSyllable3WordCache:
            return(self.dcSyllable3WordCache[LszWord])
 
        #-----
        #Split into parts on vowels and vowel sounds
 
        LliWordParts = re.split(r'[^aeiouy]+', LszWord)
 
        #-----
        # Combine the valid parts of the word
 
        LliValidWordParts = []
 
        for LszValue in LliWordParts:
            if LszValue <> '':
                LliValidWordParts.append(LszValue)
 
        LinSyllables = 0
 
        #-----
        # Loop through the compiled regexs looking for matches
 
        for LreSylRE in self.liSyllable3RESubSyllables:
            LinMatch = 0 if LreSylRE.search(LszWord) is None else 1
            LinSyllables -= LinMatch
 
        for LreSylRE in self.liSyllable3REAddSyllables:
            LinMatch = 0 if LreSylRE.search(LszWord) is None else 1
            LinSyllables += LinMatch
 
        #-----
        # Now compute the syllable count by the number of vowels
 
        LinSyllables += len(LliValidWordParts)
 
        #-----
        # If we've not found any there must be at least 1
 
        LinSyllables = 1 if LinSyllables == 0 else LinSyllables
 
        #----
        # Record this result in the word cache
 
        self.dcSyllable3WordCache[LszWord] = LinSyllables
 
        #-----
        # Return the result
 
        return(LinSyllables)
 
    def SyllableCount(self, AszWord, AszMode = 'max', AlgFallBack=True):
 
        if AszMode.lower() not in ['min','max','ave','raw']:
            LszMode = 'max'
        else:
            LszMode = AszMode
 
        LszWord = AszWord.lower()
 
        if len(LszWord) == 0 or LszWord not in self.dcSyllableCount:
            self.szMode = None
            if len(LszWord) == 0 or not AlgFallBack:
                if AszMode in ['min','max']:
                    return(0)
                elif AszMode in ['ave']:
                    return(0.0)
                elif AszMode in ['raw']:
                    return([])
            else:
                LliSyllableList = list((self.NonCMUSyllableCount(LszWord),))
                self.szMode = 'manual'
        else:
            LliSyllableList = self.dcSyllableCount[LszWord]
            self.szMode = 'lookup'
 
        if LszMode == 'min':
            return(min(LliSyllableList))
        elif LszMode == 'max':
            return(max(LliSyllableList))
        elif LszMode == 'ave':
            return(float(float(sum(LliSyllableList))/float(len(LliSyllableList))))
        elif LszMode == 'raw':
            return(LliSyllableList)
        else:
            return(None)
 
    def GetSyllableMode(self):
        #-----
        # Return either None, manual or lookup depending on how the last
        # syllable count was arrived at
 
        return(self.szMode)
 
def main():
 
    LzzSyllableCounter = cmusyllables()
 
    LzzSyllableCounter.Load()
 
    LliList = ['','theatre','productized','productised','pumblechook','everything','altogether','particular','opportunity','everybody','cooeed','cueing']
 
    for LszWord in LliList:
        print "'%s' has max(%d), min(%d), ave(%3.2f), raw(%s) syllables - Calculated by (%s)" % (LszWord,LzzSyllableCounter.SyllableCount(LszWord), LzzSyllableCounter.SyllableCount(LszWord, AszMode='min'),LzzSyllableCounter.SyllableCount(LszWord, AszMode='ave'),LzzSyllableCounter.SyllableCount(LszWord, AszMode='raw'),LzzSyllableCounter.GetSyllableMode())
 
if __name__ == '__main__':
    main()