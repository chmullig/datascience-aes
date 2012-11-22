import pandas
from sklearn.decomposition import RandomizedPCA
from sklearn.feature_extraction.text import TfidfVectorizer
import sys
import os.path
try:
    trainfile = sys.argv[1]
    testfile = sys.argv[2]
    ncomponents = int(sys.argv[3])
except IndexError:
    print "Please specify trainingfile.csv testingfile.csv NumComponents"
    sys.exit(1)

traindf = pandas.read_csv(trainfile)
testdf = pandas.read_csv(testfile)
columns=["tfidfpca_%s" % x for x in xrange(ncomponents)]

trainCleanEssay = traindf.essay.str.decode('cp1252', 'ignore')
testCleanEssay = testdf.essay.str.decode('cp1252', 'ignore')

vectorizer = TfidfVectorizer(ngram_range=(1,2))
trainvec = vectorizer.fit_transform(trainCleanEssay)
testvec = vectorizer.transform(testCleanEssay)

pca = RandomizedPCA(n_components=ncomponents)
pca.fit(trainvec)
trainpca = pca.transform(trainvec)
trainpcadf = pandas.DataFrame(trainpca, columns=columns)
testpca = pca.transform(testvec)
testpcadf = pandas.DataFrame(testpca, columns=columns)

traindf = traindf.combine_first(trainpcadf)
testdf = testdf.combine_first(testpcadf)

nf = lambda x: os.path.splitext(os.path.basename(x))[0] + "_tfidf.csv"
traindf.to_csv(nf(trainfile))
testdf.to_csv(nf(testfile))

print "+".join(columns)
