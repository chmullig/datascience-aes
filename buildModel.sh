#!/bin/bash
python basic_tags.py train.tsv
#python basic_tags.py test.tsv
#python add_tfidf.py train_tagged.csv test_tagged.csv 50
Rscript basicModel.R
python validateSubmission.py testing_predicted_lm.csv
python validateSubmission.py testing_predicted_rf.csv
python validateSubmission.py testing_predicted_rfa.csv
python validateSubmission.py testing_predicted_gbm.csv
python validateSubmission.py testing_predicted_gbma.csv
