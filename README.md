# STA304-Final-Project
Ning Xie, ID:1005120691


##Title

The Key Factors That Influence the Evaluation of Apartment Buildings In Toronto
Author: Ning Xie
Date: Dec. 16th, 2020

#Link to a Github repo

https://github.com/andreaxie2054/STA304-Final-Project

##Abstract

The result of the MLR model shows that the condition of all facilities is the key to determine the evaluation of an apartment building. Every one level up of the condition of a facility will result in an increase of 1 point in the score of the building.

##Keywords

Toronto apartments, Apartment Building Evaluation, Apartment Building Standards(ABS) program, MLR model

##Introduction

Statistical analysis is essential to government management. By analyzing various polls and observational data collected, the government can formulate corresponding policies or make some adjustments to society according to the analysis results. From the perspective of the government, observational data is more realistic and reliable compared to the experimental design data.

In this project, we want to detect the factors that influence the evaluation of the apartment buildings in Toronto. We use the Apartment Building Evaluation data from The City of Toronto¡¯s Open Data for our data analysis. The dataset contains the evaluation scores about apartment buildings in Toronto that are registered in the Apartment Building Standards(ABS) program. The ABS program ensures building owners and operators comply with building maintenance standards. It makes sure people that rent homes have clean and secure residences. [1]

An MLR model will be used to investigate the relationship between the evaluation scores and various facilities in apartment buildings. In the Methodology section, we make data sampling, clean the data and make the multiple linear regression model. The result of the data modelling is given in the Result section, followed by the conclusion in the Discussion section and the references.


##Methodology

#Data Cleaning

The raw data we use is the Apartment Building Evaluation data from The City of Toronto¡¯s Open Data Portal. This dataset is about the condition of apartment buildings in Toronto. It contains features such as the condition of the balcony guards, the overall score, the ward that the building is located in, and so on. There are 35984 NAs in the dataset. After removing rows with NAs, we randomly select 150 samples from the clean raw dataset. 

library(tidyverse)
apartment_raw <- read.csv('Apartment Building Evaluation.csv')

#check the number of NAs and remove rows with NAs
sum(is.na(apartment_raw))
apartment_raw <- na.omit(apartment_raw)
apartment_raw

#Data Sampling

set.seed(7788)
apa_sample <- sample_n(apartment_raw, size = 150)
apa_sample

#Variable Correlation

Before modelling, we need to check the correlation between the variable SCORE and all the other variables to make sure they have a linear relationship. Categorical variables like PROPERTY_TYPE and numerical variables that are not related to facilities like YEAR_REGISTERED are excluded from the correlation checking.

set.seed(7788)
quanti_apa <- subset(apa_sample, select = -c(X_id, CONFIRMED_STOREYS, CONFIRMED_UNITS, EVALUATION_COMPLETED_ON, NO_OF_AREAS_EVALUATED, PROPERTY_TYPE, RESULTS_OF_SCORE, RSN, SITE_ADDRESS, WARD, YEAR_BUILT, YEAR_REGISTERED))

apa_cor <- cor(quanti_apa, quanti_apa$SCORE, use = 'pairwise.complete.obs')
apa_cor

From the correlation summary, the correlation between SCORE and most of the facilities related variables is on average 0.5 to 0.65. Two variables with a relatively lower correlation are PARKING_AREA and GRAFFITI. The correlation value is 0.4689386 and 0.3492569, respectively.

#Model

Through the correlation checking, the linear relationship between the variable SCORE and other facilities related variables is confirmed. Thus, we make an MLR model investigate how each facility affects the evaluation of the apartment buildings in Toronto.

set.seed(7788)
apa_mlr <- lm(SCORE~BALCONY_GUARDS+ELEVATORS+ENTRANCE_DOORS_WINDOWS+ENTRANCE_LOBBY+EXTERIOR_CLADDING+EXTERIOR_GROUNDS+EXTERIOR_WALKWAYS+GARBAGE_BIN_STORAGE_AREA+GARBAGE_CHUTE_ROOMS+GRAFFITI+INTERIOR_LIGHTING_LEVELS+INTERIOR_WALL_CEILING_FLOOR+INTERNAL_GUARDS_HANDRAILS+LAUNDRY_ROOMS+OTHER_FACILITIES+PARKING_AREA+SECURITY+STAIRWELLS+STORAGE_AREAS_LOCKERS+WATER_PEN_EXT_BLDG_ELEMENTS, data = apa_sample)
summary(apa_mlr)

The fitted line of the score of the apartment buildings in Toronto is:
$$\hat{y}=-0.06769+1.00006BALCONY_GUARDS+1.00927ELEVATORS+1.00988ENTRANCE_DOORS_WINDOWS+1.00506ENTRANCE_LOBBY+0.99801EXTERIOR_CLADDING+1.01333EXTERIOR_GROUNDS+1.00743EXTERIOR_WALKWAYS+1.00406GARBAGE_BIN_STORAGE_AREA+0.98846GARBAGE_CHUTE_ROOMS+0.99912GRAFFITI+1.00285INTERIOR_LIGHTING_LEVELS+0.99170INTERIOR_WALL_CEILING_FLOOR+1.00058INTERNAL_GUARDS_HANDRAILS+1.00811LAUNDRY_ROOMS+1.00256OTHER_FACILITIES+0.99058PARKING_AREA+0.98769SECURITY+0.98294STAIRWELLS+1.02108STORAGE_AREAS_LOCKERS+0.99404WATER_PEN_EXT_BLDG_ELEMENTS$$

where y hat is the response variable SCORE(the evaluation score of the apartment building). BALCONY_GUARDS represents the condition of the balcony guards in a building, ELEVATORS represents the condition of the elevators in a building, ENTRANCE_DOORS_WINDOWS represents the condition of the entrance doors and windows in a building, ENTRANCE_LOBBY represents the condition of the entrance or lobby in a building, EXTERIOR_CLADDING represents the condition of the exterior cladding on a building, EXTERIOR_GROUNDS represents the condition of the exterior grounds of a building, EXTERIOR_WALKWAYS represents the condition of the exterior walkways of a building, GARBAGE_BIN_STORAGE_AREA represents the condition of the garbage bin storage area in a building, GARBAGE_CHUTE_ROOMS represents the condition of the chute rooms in a building, GRAFFITI represents the severity of graffiti in a building, INTERIOR_LIGHTING_LEVELS represents the condition of internal lighting levels in a building, INTERIOR_WALL_CEILING_FLOOR represents the condition of internal walls, ceilings and floors in a building, INTERNAL_GUARDS_HANDRAILS represents the condition of the internal guards and handrails in a building, LAUNDRY_ROOMS represents the condition of the laundry room(s) in a building, OTHER_FACILITIES represents the condition of other facilities in a building, PARKING_AREA represents the condition of the parking areas of a building, SECURITY represents the condition of the security in a building, STAIRWELLS represents the condition of the stairwells in a building, STORAGE_AREAS_LOCKERS represents the condition of the storage areas/lockers in a building, and WATER_PEN_EXT_BLDG_ELEMENTS represents the condition of water penetration of external building elements of a building. All conditions are rated from 1 to 5, with 1 being the worst condition whereas 5 being the best condition.

From the summary, the F-statistic of the whole model is 68880 and the p-value is smaller than 2.2*10^-16, which is smaller than 0.05. Thus, the p-values and t-test results are significant. Based on the p-value of each predictor, all predictors are significant.

#Diagnostic Plots

par(mfrow = c(2, 2))
plot(apa_mlr)

##Results

According to the MLR model summary, the score of a building would be -0.06769 if all the predictors are 0, that is, if there are no facilities in the building. However, since the lowest score can only be 0, the intercept of the MLR model has no intrinsic meaning. As for the predictors, since the coefficient of every predictor is approximately 1, we assume every 1 unit increase in each predictor will result in an average 1 unit increase in the response variable. That is to say, as the condition of every facility improves one level up, the score of the apartment building will increase 1 point on average.

From the diagnostic plots, the first Residuals vs Fitted plot shows the relationship between fitted values and residuals of the model. Since the trend of the plot is overall horizontal, there is no linear relationship between fitted values and residuals, thus the assumption of linear regression is satisfied.

The second plot is the Normal Q-Q plot, which we use to detect the normal distribution of standard errors. It shows that the errors are normally distributed since most of the points match the dotted line in the plot. Thus, the normal error MLR assumption is satisfied.

For the third plot Scale-Location plot, we use it to check the assumption of constant variance. Since the points in the plot are not equally spread, the assumption of constant variance is not satisfied.

For the fourth plot Standard residuals vs Leverage, we use it to check influential points. Most of the points are close to the line except one influential point.

##Discussion

#Summary

In order to investigate the relationship between the evaluation score of a building and the condition of its facilities, we first clean the data we get from The City of Toronto¡¯s Open Data Portal by removing all the NAs. After the data sampling, we check the correlation between the score and other facilities to ensure there exists a linear relationship.  By summarising the MLR model, it is found that the score will increase 1 point on average as each facility gets one level up of its condition. In the end, the diagnostic plots show that the assumption of linear regression and the normal error MLR are satisfied. Whereas the assumption of constant variance is violated and there is an influential point.

#Conclusions

By checking the p-value of each facility-related variable, it is confirmed that all explanatory variables in the MLR model are significant. Thus, variables BALCONY_GUARDS, ELEVATORS, ENTRANCE_DOORS_WINDOWS, ENTRANCE_LOBBY, EXTERIOR_CLADDING, EXTERIOR_GROUNDS, EXTERIOR_WALKWAYS, GARBAGE_BIN_STORAGE_AREA, GARBAGE_CHUTE_ROOMS, GRAFFITI, INTERIOR_LIGHTING_LEVELS, INTERIOR_WALL_CEILING_FLOOR, INTERNAL_GUARDS_HANDRAILS, LAUNDRY_ROOMS, OTHER_FACILITIES, PARKING_AREA, SECURITY, STAIRWELLS, STORAGE_AREAS_LOCKERS, WATER_PEN_EXT_BLDG_ELEMENTS are all key factors that influence the score of a building. Since the coefficient of all explanatory variables is approximately 1¡À0.02, we conclude that as the condition of every facility increases one level up, the score of a building will increase 1 point on average. The government may determine whether to make a full building audit or not based on this score, whereas the renter may be aware of which apartment is a better residence.

#Weakness & Next Steps

Some weaknesses should be considered throughout the analysis. During the data cleaning, a large number of NAs(35984 NAs) are found from the raw dataset. We remove all rows with NAs to make the analysis. However, the NAs may contain some useful information for our analysis, and deleting them may affect the accuracy of our result. Thus, for the next step, we can continue studying how to make the balance between NAs and other data, or try to find a way to reduce the impact of NAs on our final result.

We also found an influential point by plotting the diagnostic plot. Since removing the influential point will lead to a large impact on the fitted line, we can try to fix the influential point to normal in the future study.

##Reference

[1]dataset: https://open.toronto.ca/dataset/apartment-building-evaluation/, accessed on Dec. 16th, 2020

[2]Software: R, package: tidyverse



