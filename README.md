This is one of Gaurav Bansal's attempt at the Kaggle San Francisco Crime Classification contest. It uses a Random Forest on all predictors to classify the type of crime occurring at a given time and location in the city of San Francisco, CA. In the contest, contestants are given 12 years of crime reports across the city. Even numbered weeks are included in the training data and contestants must predict crime classification for odd numbered weeks.

Contestants' classification predictions are evaluated using the multi-class logarithmic loss method. This particular Random Forest algorithm by Gaurav results in a score of 4.04069. However, Gaurav has improved this algorithm to improve his score. 

The model was created in R. The R code is in the file 'SFcrime.R'. The training set is in 'train.csv' and the test data is in 'test.csv'. The model output is in 'solution.csv'.

A full description of the contest can be found at https://www.kaggle.com/c/sf-crime
