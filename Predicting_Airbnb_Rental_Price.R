# packages
install.packages("naniar")
library(naniar)
library(rpart)
library(dplyr)
library(tidyr)
install.packages("randomForest")
library(randomForest)
library(caret)
library(forcats)
install.packages('gbm')
library(gbm)

# Read data 
setwd('"~/Downloads/')
data = read.csv("analysisData.csv", comment.char="#")
scoringData = read.csv("~/Downloads/scoringData.csv")


#imputation
data <- impute_below_if(data, is.numeric)


# factor collapse on scoringData
neighbourhood_collapsed <- fct_collapse(
  factor(scoringData$neighbourhood_cleansed), 
                                        'Todt Hill' = c('Todt Hill', 'Lighthouse Hill'), 
                                        'Rockaway Beach' = c('Rockaway Beach', 'Breezy Point')
)

property_type_collapsed <- fct_collapse(
  factor(scoringData$property_type), 
                                    Other = c('Castle', 'Dome house') 
)


# modifying scoringData to reflect factor collapse 
scoringData$neighbourhood_cleansed <- factor(neighbourhood_collapsed)
scoringData$property_type <- factor(property_type_collapsed)


# factorizing columns
data$neighbourhood_cleansed <- factor(data$neighbourhood_cleansed)
data$property_type <- factor(data$property_type)


# parsing amenities 

amenities_count_train <-sapply(strsplit(data$amenities, ","), length)
amenities_count_test <- sapply(strsplit(scoringData$amenities, ","), length)

# modifying data to reflect amenities count

data$amenities_count <- amenities_count_train
scoringData$amenities_count <- amenities_count_test


## MODELING WORK START

# construct a simple model
model = lm(price~minimum_nights+review_scores_accuracy,data) #sample we started with


model2 = lm(price~minimum_nights+review_scores_accuracy+bedrooms,data) #first modification

#came from forward search using variables: minimum_nights+review_scores_accuracy+bedrooms+bathrooms+guests_included+city+zipcode+property_type
#Note some variables we cut out later because they were giving problems that we can try to fix later
model3 =lm(price ~ bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights, data) 


#model2 tree
model4 = rpart(price~minimum_nights+review_scores_accuracy+bedrooms,data)


#model3 tree
model5 = rpart(price ~ bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights, data)


model6 = rpart(price ~bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights + neighbourhood_cleansed + property_type  + square_feet + availability_365 + calculated_host_listings_count_entire_homes + amenities_count + amenities_count, data)


model7 = lm(price ~ bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights + amenities_count + neighbourhood_cleansed + property_type, data)


forest = randomForest(price ~bedrooms + guests_included + bathrooms + review_scores_accuracy + minimum_nights + property_type  + square_feet + availability_365 + calculated_host_listings_count_entire_homes + amenities_count, data=data , ntree = 100)



boost = gbm(price ~bedrooms + guests_included + host_listings_count + host_total_listings_count + extra_people + maximum_nights + minimum_minimum_nights + maximum_minimum_nights + minimum_maximum_nights + maximum_maximum_nights + review_scores_location + availability_30 + availability_60 + availability_90 +number_of_reviews + number_of_reviews_ltm + minimum_nights_avg_ntm + maximum_nights_avg_ntm + review_scores_communication + review_scores_checkin + review_scores_cleanliness + review_scores_rating + review_scores_value + bathrooms + review_scores_accuracy + minimum_nights + neighbourhood_cleansed + property_type  + square_feet + availability_365 + calculated_host_listings_count_entire_homes + amenities_count,
            data=train,
            distribution="gaussian",
            n.trees = 1000,
            interaction.depth = 11,
            shrinkage = 0.02)

summary(boost)


# apply model to generate predictions
pred = predict(boost,newdata=scoringData, n.trees = 500)
pred2 = predict(model2, newdata=scoringData) #newdata = ScoringData this is for submission
pred3 = predict(model3, newdata=scoringData)
pred4 = predict(model4, newdata=scoringData)
pred5 = predict(model5, newdata=scoringData)
pred6 = predict(model6, newdata=scoringData)
pred7 = predict(model7, newdata=scoringData)
pred_boost <- predict(boost, newdata=scoringData, n.trees = 1000)
pred_forest = predict(forest,newdata=scoringData)
  

# rmse calculations

pred_train = predict(model)
pred_train_2 = predict(model2) 
pred_train_3 = predict(model3)
pred_train_4 = predict(model4)
pred_train_5 = predict(model5)
pred_train_6 = predict(model6)
pred_train_7 = predict(model7)
pred_train_boost = predict(boost,n.trees = 1000)
pred_train_forest = predict(forest)


rmse = sqrt(mean((pred_train-data$price)^2)); rmse
rmse2 = sqrt(mean((pred_train_2-data$price)^2)); rmse2
rmse3= sqrt(mean((pred_train_3-data$price)^2)); rmse3
rmse4 = sqrt(mean((pred_train_4-data$price)^2)); rmse4
rmse5 = sqrt(mean((pred_train_5-data$price)^2)); rmse5
rmse6 = sqrt(mean((pred_train_6-data$price)^2)); rmse6
rmse7 = sqrt(mean((pred_train_7-data$price)^2)); rmse6
rmse_boost = sqrt(mean((pred_train_boost-data$price)^2)); rmse_boost
rmse_forest = sqrt(mean((pred_train_forest-data$price)^2)); rmse_forest


## MODELING WORK END


# split and model performance test for best model

split = sample(1:nrow(data),0.7*nrow(data))
train = data[split,]
test = data[-split,]

boost_train = gbm(price ~bedrooms + guests_included + host_listings_count + host_total_listings_count + extra_people + maximum_nights + minimum_minimum_nights + maximum_minimum_nights + minimum_maximum_nights + maximum_maximum_nights + review_scores_location + availability_30 + availability_60 + availability_90 +number_of_reviews + number_of_reviews_ltm + minimum_nights_avg_ntm + maximum_nights_avg_ntm + review_scores_communication + review_scores_checkin + review_scores_cleanliness + review_scores_rating + review_scores_value + bathrooms + review_scores_accuracy + minimum_nights + neighbourhood_cleansed + property_type  + square_feet + availability_365 + calculated_host_listings_count_entire_homes + amenities_count,
            data=train,
            distribution="gaussian",
            n.trees = 1000,
            interaction.depth = 13,
            shrinkage = 0.03)
pred_boost_test = predict(boost_train, newdata = test, n.trees = 1000)
rmse_boost_test = sqrt(mean((pred_boost_test-test$price)^2)); rmse_boost_test



# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred_boost)
write.csv(submissionFile, '~/Downloads/AAFM1_Tahabee_Kaggle_submission12',row.names = F)
