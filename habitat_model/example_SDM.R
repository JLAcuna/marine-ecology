##########################################################
#     HABITAT MODEL-IMBRSEA MARINE ECOLOGY COURSE        #
#           By Jose Luis Acuna-09/11/2021                #
##########################################################

##########################################################
#          uploads the required libraries                #
##########################################################

# "ggplot2" to create graphics
library(ggplot2)

# "car" to estimate collinearity with vif()
library(car)

# "InformationValue" with Functions that Aid Accuracy Improvement
# for Binary Classification Models. We use function optimalCutoff()
# (https://cran.r-project.org/web/packages/InformationValue/vignettes/InformationValue.html)
library(InformationValue)


# "caret" contains functions to failitate the creation of predictive models.
# (https://topepo.github.io/caret/). We use function confusionMatrix()
library(caret)




##########################################################
#          opens and prepares data for analysis          #
##########################################################

# opens a text file with the data as dataframe "ParasiteCod"
ParasiteCod <- read.csv("https://raw.githubusercontent.com/JLAcuna/marine-ecology/main/habitat_model/ParasiteCod.csv", sep=";")

# eliminates rows where there are missing data (appearing as "NA")
# and visualizes the dataframe
ParasiteCod <- na.omit(ParasiteCod)

# converts Area and Year to factors
ParasiteCod$Area <- factor(ParasiteCod$Area)
ParasiteCod$Year <- factor(ParasiteCod$Year)

# presents the first few lines of the dataframe
head(ParasiteCod)



##########################################################
#           Plots histogram of prevalence                #
##########################################################

# creates histograms of prevalence for each combination of year and area
# using "ggplot2). Note that this is done by creating an object "h" which 
# is updated by adding successive layers (i.e. h <- h+new_layer)

# LAYER 0: declares the graphics object "h".  the first layer defines the
# dataframe and the variable which will be plotted    
h <- ggplot(ParasiteCod, aes(Prevalence))

# LAYER 1: updates the plot as an histogram
h<-h+geom_bar() 

# LAYER 2: sets x-axis break at 0 and 1
h<-h+scale_x_continuous(breaks=c(0,1))

# LAYER 3: partitions the figures in one panel for each combination
# of year and region
h<-h+facet_grid(Year~Area)   

# plots the graphic
h

##########################################################
#          Plots Prevalence vs. Depth          #
##########################################################
 

# Creates graphics object "p" to plot Prevalence vs Length
p <- ggplot(ParasiteCod, aes(x=Depth, y=Prevalence))

# uses scatter plot with hollow circles
p<-p+geom_point(shape=1)

# Adds a linear regression line (by default includes 95% confidence region)
p<-p+geom_smooth(method=lm)

# sets label with units in the x-axis
p<-p+xlab("depth (m)")

# sets label with units in the y-axis
p<-p+ylab("probability infected")           

# plots the graph
p



##########################################################
#              Performs logistic regression              #
##########################################################


# fits the logistic model to the data
Depth_model <- glm(Prevalence ~ Depth, data = ParasiteCod, family = binomial)

# prints the summary
summary(Depth_model)



##########################################################
#                   Represents model                     #
##########################################################

# creates a new variable "Depth" with depth values from -500 to 1000
# in steps of 1
Depth<-data.frame(Depth=seq(-500, 1000,1))


# for each of the above depths, calculates the predictions according to 
# the logistic model "Depth_model" and stores the values in variable
# "probabilities"
probabilities<-predict(Depth_model, Depth, type="response")


# creates dataframe "predictions" by joining variables "Depth" 
# and "probabilities"
predictions<-cbind(Depth, probabilities)


# creates a plot of "probabilities" vs "Depth" in dataframe "predictions" 
l<-ggplot(predictions, aes(Depth, probabilities))


# updates the plot as a line plot
l<-l+geom_line()


# prints the plot
l



##########################################################
#     Represents model but for the actual depth range    #
##########################################################


# reduces the prediction dataframe to the actual depth range
predictions<-subset(predictions, Depth>min(ParasiteCod$Depth) & Depth<max(ParasiteCod$Depth))

# removes the "smooth" layer with the linear regression in figure 3
p$layers[[2]] <- NULL

# adds the logit regression to the scatterplot in figure 4
p<-p+geom_line(data=predictions, aes(y=probabilities), size=1)


# plots the graph
p


##################################################################
#Builds models based on different hypothesis and selects the best#
##################################################################


# model 1: all factors matter. prevalence in each area should change depending
# on time since introduction. Cod length (i.e., age) has an effect due to time
# exposed to parasites, but this effect is stronger when temperatures are higher
model1 <- glm(Prevalence ~ Area*Year+Depth*Length, data = ParasiteCod, family = binomial)


# summarizes model1 results
summary(model1)


# model 2: as model 1, but no interaction between Depth and Length
model2 <- glm(Prevalence ~ Area*Year+Depth+Length, data = ParasiteCod, family = binomial)


# summarizes model2 results
summary(model2)


# model 3: as model 2, but without Length. 
model3 <- glm(Prevalence ~ Area*Year+Depth, data = ParasiteCod, family = binomial)


# summarizes model2 results
summary(model3)


# model 4: as model 3, but without Length and Depth. 
model4 <- glm(Prevalence ~ Area*Year, data = ParasiteCod, family = binomial)


# summarizes model4 results
summary(model4)


# compares the four models through the Akaike Information Criterion
AICc(model1, model2, model3, model4)


##########################################################
#        Checks models 2 and  3 for collinearity         #
##########################################################

# calculates variance inflation factor for terms in model 3. Values larger
# than 10 indicate collinearity problems, leading to inflated variance in
# parameter estimates
vif(model2)
vif(model3)



##########################################################
#          Builds a confusion matrix for model 2         #
##########################################################

# A simple introduction to confusion matrices in which the following
# analysis is based is given here:
# https://www.statology.org/confusion-matrix-in-r/
# A more complete account here:
# here: https://en.wikipedia.org/wiki/Receiver_operating_characteristic

# to generate a random sequence of logical values "TRUE" and "FALSE",
#we first set a seed
set.seed(1)


# We sample randomly with replacement a vector ("TRUE", "FALSE") to generate
# a list of 1191 values. The "TRUE" rows are meant to select the training rows
# in the ParasiteCod dataframe, and are sampled with a probability of 0.7.
# In other words, we want to extract 70% of the rows from ParasiteCod, which
# will be used to train the model. The "FALSE" values are to select the
# rows which will be used to test the model (30%).
sample <- sample(c(TRUE, FALSE), nrow(ParasiteCod), replace=TRUE, prob=c(0.7,0.3))


# We then create a training dataset by selecting those rows in ParasiteCod
# which correspond to a "TRUE" in vector "sample"
train <- ParasiteCod[sample, ]


# We now create a testing dataset by selecting those rows in ParasiteCod
# which correspond to a "FALSE" in vector "sample"
test <- ParasiteCod[!sample, ]


#We now fit model 2 using the training dataset, and call it "model"
model <- glm(Prevalence ~ Area*Year+Depth+Length, data = train, family = binomial)


# Just a look at the summary, to make sure the model parameters are similar to
# those of model 2
summary(model)


# now we use the model to make predictions of prevalence for the test dataset
# and store the predictions in "predicted"
predicted <- predict(model, test, type="response")


#create confusion matrix
conf<-confusionMatrix(test$Prevalence, predicted)


# prints confusion matrix
conf


# calculates the accuracy (true predictions/all predictions)
(conf[1,1]+conf[2,2])/(conf[1,1]+conf[1,2]+conf[2,1]+conf[2,2])
