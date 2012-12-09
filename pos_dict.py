"""
Source: http://www.mozart-oz.org/mogul/doc/lager/brill-tagger/penn.html
"""
pos_dict = {
    "CC"   : "coordinating conjunction",    #and
    "CD"   : "cardinal number",     #1, third
    "DT"   : "determiner",  #the
    "EX"   : "existential", # there  there is
    "FW"   : "foreign word", #    d'hoevre
    "IN"   : "preposition/subordinating conjunction", #   in, of, like
    "JJ"   : "adjective",   #green
    "JJR"  : "adjective, comparative",  #greener
    "JJS"  : "adjective, superlative",  #greenest
    "LS"   : "list marker",     #1)
    "MD"   : "modal", #could, will
    "NN"   : "noun, singular or mass",  #table
    "NNS"  : "noun plural",     #tables
    "NNP"  : "proper noun, singular",   #John
    "NNPS"     : "proper noun, plural",     #Vikings
    "PDT"  : "predeterminer", #both the boys
    "POS"  : "possessive ending", #friend's
    "PRP"  : "personal pronoun", #I, he, it
    "PRP$" : "possessive pronoun", #my, his
    "RB"   : "adverb", #  however, usually, naturally, here, good
    "RBR"  : "adverb, comparative",     #better
    "RBS"  : "adverb, superlative",     #best
    "RP"   : "particle", #    give up
    "TO"   : "to", # to go, to him
    "UH"   : "interjection",    #uhhuhhuhh
    "VB"   : "verb, base form",     #take
    "VBD"  : "verb, past tense",    #took
    "VBG"  : "verb, gerund/present participle",     #taking
    "VBN"  : "verb, past participle",   #taken
    "VBP"  : "verb, sing. present, non-3d",     #take
    "VBZ"  : "verb, 3rd person sing. present",  #takes
    "WDT"  : "wh-determiner",   #which
    "WP"   : "wh-pronoun  who,", #what
    "WP$" : "possessive wh-pronoun",   #whose
    "WRB"  : "wh-abverb   where,", #when
}

"""
Crappy Source: http://www.ims.uni-stuttgart.de/projekte/CorpusWorkbench/CQP-HTMLDemo/PennTreebankTS.html
"""
pos_dict_crappy = {
    "CC" :  "Coordinating conjunction",
    "CD" :  "Cardinal number",
    "DT" :  "Determiner",
    "EX" :  "Existential there",
    "FW" :  "Foreign word",
    "IN" :  "Preposition or subordinating conjunction",
    "JJ" :  "Adjective",
    "JJR" : "Adjective, comparative",
    "JJS" : "Adjective, superlative",
    "LS" :  "List item marker",
    "MD" :  "Modal",
    "NN" :  "Noun, singular or mass",
    "NNS" : "Noun, plural",
    "NP" :  "Proper noun, singular",
    "NPS" : "Proper noun, plural",
    "PDT" : "Predeterminer",
    "POS" : "Possessive ending",
    "PP" :  "Personal pronoun",
    "PP$" : "Possessive pronoun",
    "RB" :  "Adverb",
    "RBR" : "Adverb, comparative",
    "RBS" : "Adverb, superlative",
    "RP" :  "Particle",
    "SYM" : "Symbol",
    "TO" :  "to",
    "UH" :  "Interjection",
    "VB" :  "Verb, base form",
    "VBD" : "Verb, past tense",
    "VBG" : "Verb, gerund or present participle",
    "VBN" : "Verb, past participle",
    "VBP" : "Verb, non-3rd person singular present",
    "VBZ" : "Verb, 3rd person singular present",
    "WDT" : "Wh-determiner",
    "WP" :  "Wh-pronoun",
    "WP$" : "Possessive wh-pronoun",
    "WRB" : "Wh-adverb",
}

if __name__ == "__main__":
    for code, explanation in sorted(pos_dict.items()):
        print code + ": " + explanation
