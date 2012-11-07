from config import *
#expect to find the following defined in config: 
# email, password,
# url (eg = "http://inclass.kaggle.com/c/columbia-university-introduction-to-data-science-fall-2012/publicleaderboarddata.zip"),
# filename (eg = "columbia-university-introduction-to-data-science-fall-2012_public_leaderboard.csv")

import mechanize
import zipfile

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


