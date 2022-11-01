# REDUFVE

The purpose of this project is to nowcast homocide mortality in the US.

This project utilizes a Stacked Ensemble methodology in order to process individual level predictions per data source such as Google Trends queries and
then ensembling each into the final Ensemble. 

Each data source has a number of different variable selection techniques implemented as well as utilizing three different models for each. 

The final ensemble has a handful of different modeling approaches in order to obtain the most accurate predictions based primary on a RMSE performance metric.

One would run each individual data source code, conduct variable selection and model eveluation for each, and then compiling into the final stacked ensemble where 
further ensemble model eveluation would be conducted. 

Training data ranges from 2016-2017, validation was conducted on 2018, and final testing performed on 2019.
