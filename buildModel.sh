#!/bin/bash
python basic_tags.py train.tsv
python basic_tags.py test.tsv
python add_tfidf.py train.tsv test.tsv 50
Rscript buildModel.py
python validateSubmission.py testing_predicted_lm.csv
python validateSubmission.py testing_predicted_rf.csv
