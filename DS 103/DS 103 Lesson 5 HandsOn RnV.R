## Loading libraries
library("corpcor")
library("GPArotation")
library("psych")
library("IDPmisc")

## Load data
data = read.csv('/Users/britfathi/Downloads/studentSurvey/studentSurvey.csv')

## Look at data. Running in RStudio revealed there were 87 columns, with NA values
head(data)

## From the CodeBook, I can see there are 50 questions/items/areas 
## therefore the data we want is from maybe column 30 on. From the table, 
## I can see I can subset columns 31-44 for the Areas. 
dataAreas = data[,31:42]

## Cleaning the data
dataArea2 = NaRV.omit(dataAreas)

## Make a matrix of the correlation values
dataMatrix = cor(dataArea2)

## View dataMatrix rounded to two decimal places
View(round(dataMatrix,2))
## All correlation values are less than .7

## Run Barlett on our non-Matrix data
cortest.bartlett(dataArea2)
## chisq is 2807, p.value is 0, df is 66

## Get the determinant of the matrix
det(dataMatrix)
## Got 0.00218 > .00001 so I can move onto the model

model = principal(dataArea2, nfactor = 12, rotate = 'none')
model
## Fit based upon off diagonal values = 1 that has the right Eigen values

plot(model$values, type = 'b')

## Second pass
model2 = principal(dataArea2, nfactor = 1, rotate = "none")
model2

## Examine residuals
residuals = factor.residuals(dataMatrix, model2$loadings)
residuals = as.matrix(residuals[upper.tri(residuals)])
largeResid2 = abs(residuals) > .05
sum(largeResid2)

sum(largeResid2/nrow(residuals))

alpha(dataArea2)



