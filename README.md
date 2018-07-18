# DM_Project

This project was built built for the course INSE-6180. It is developed in R.

There are three file mainfile.R, which is th emain file and program starts here.
Other two files are the feature files, ngram.R and POS.R.
ngram file calculates ngram probability from the traing data and supplies it to the mailfile, and the POS file calculates probability for the parts of speech and supplies it to the mainfile. In the mainfile these probabailities were used to create a Naive-Bayes classifier to classify the tweets as positive-truthful, positive-deceptive, negative-truthful or negative-deceptive.

Our data was taken from Large Movie Review Dataset (LMRD) for the analysis. It consisted of positive, negative, truthful and deeptive tweets taken from the twitter API.
