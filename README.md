# Black Box Optimization Project

This is our group's solution for the function approximation and multi-objective optimization challenge from the Data Analytics II course.
In the first stage, two unknown functions needed to be approximated by requesting a limited number of points from an API. Secondly, a pareto-front over both approximated functions is constructed.

## Requirements

* httr
* jsonlite
* plotly
* dplyr
* mlr
* ecr
* MOEADr
* ggplot2
* tidyr
* kernlab

## Usage

As the original API is no longer provided, the api-functions in Base.R need to be adapted before use.

## Details

After an intial semi-random latin hypercube sampling of 200 points for each function, the next points are requested based on where the uncertainty in the current best approximation is highest. To determine this uncertainty, each point is assessed via leave-one-out resampling. A kNN-learner is trained to predict the uncertainty of a larger grid of points in the function space. The 10 points with the highest uncertainty-values are requested from the API and the process begins again until the budget is exhausted. The approximation of the function based on the available points is in each iteration done with 5 different learners: SVM, XGBoost, Random Forest, kNN, ANN. 

The final pareto-front is constructed in the Optimization.R-script with a combination of the results from the NSGA-II and MOEA/D algorithms.