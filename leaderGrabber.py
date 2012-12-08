#!/usr/bin/env python
import mechanize
import zipfile
from subprocess import call

from config import *
#expect to find the following defined in config: 
# email, password,
# url (eg = "http://inclass.kaggle.com/c/columbia-university-introduction-to-data-science-fall-2012/publicleaderboarddata.zip"),
# filename (eg = "columbia-university-introduction-to-data-science-fall-2012_public_leaderboard.csv")

def download_leaderboard():
    print '   -> Starting mechanize browser'
    br = mechanize.Browser()
    br.open("http://kaggle.com/account/login")

    br.select_form(nr=0)
    br["UserName"] = email
    br["Password"] = password
    loginResponse = br.submit()

    #download the leaderboard zip
    ltempfile, headers = br.retrieve(url)

    lzip = zipfile.ZipFile(ltempfile)
    csv = open(filename, "w")
    csv.write(lzip.read(filename))
    csv.close()
    print '   -> Downloaded'

def makegraph():
    print '   -> creating graph with R'
    r = call(["Rscript", "makegraph.R"])
    r = call(["Rscript", "makegraph_closeup.R"])
    resize = call(["sips", "--out", "datascience_leaderboard-150x150.png", "-Z", "150", "datascience_leaderboard.png"])
    resize = call(["sips", "--out", "datascience_leaderboard-640x640.png", "-Z", "640", "datascience_leaderboard.png"])
    print '   -> Graph ready'

def uploadgraph():
    print '   -> Uploading graph'
    scp = call(['scp', 'datascience_leaderboard.png',
                        'datascience_leaderboard-150x150.png',
                        'datascience_leaderboard-640x640.png',
                        'datascience_leaderboard_closeup.png',
                        'chmullig.com:webapps/chmullig/wp-content/uploads/2012/11/'])


if __name__ == '__main__':
    download_leaderboard()
    makegraph()
    uploadgraph()
    print '   -> Done'
