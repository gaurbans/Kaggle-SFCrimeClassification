This is one of Gaurav Bansal's attempts at the Kaggle San Francisco Crime Classification contest. It uses a linear discriminant analysis algorithm to classify the type of crime occurring at a given time and location in the city of San Francisco, CA. In the contest, contestants are given 12 years of crime reports across the city. Even numbered weeks are included in the training data and contestants must predict crime classification for odd numbered weeks.

The model was created in R. The R code is in the file 'SFcrime.R'. A full description of the contest and data downloads can be found at https://www.kaggle.com/c/sf-crime. You can see a version of this same code on the Kaggle website at https://www.kaggle.com/gaurbans/sf-crime/lda-model-in-r. 

Contestants' classification predictions are evaluated using the multi-class logarithmic loss method. This particular LDA algorithm by Gaurav results in a score of 2.598. 
