# HarvardX-PH125.9x_Movielens_Capstone

The purpose of this project is to use a publicly available large data set, to create a recommendation system. For this particular project we will be using movie databases and ratings given from individual users. By understanding how a user has previously rated certain movies, one can build a model to predict how the user would rate other movies. By determining which movies a user might rate highly, recommendations can be made. 

In this project we will train a machine learning algorithm to predict user ratings from a training data set (called the edx data set) and then test the algorithm on a validation data set. 
The goal will be to predict a users rating in the validation set that closely matches the users actual rating for a given movie. Successful prediction will be evaluated by Root Mean Square Error (```RMSE```). This metric serves to measure the difference between the predicted and actual values. The RMSE will be evaluated for several different models, with a lower RMSE indicating a more accurate prediction algorithm.
