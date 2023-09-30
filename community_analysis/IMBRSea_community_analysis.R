##########################################################
#    COMMUNITY ANALYSIS-IMBRSEA MARINE ECOLOGY COURSE    #
#           By Jose Luis Acuna-09/10/2023                #
##########################################################

##########################################################
#  Classification analysis: Hierarchical clustering      #
##########################################################

# To run a single line of code you just place the cursor in that line and click
# Ctrl+Enter. 

# First we open the comma-separated ekofisk-species.csv file,
# and create an R dataframe called ekofisk-species, with species as column names and
# samples as row names. The following code downloads the data from a cloud platform 
# called GITHUB, where I have uploaded it. This is enough to follow the tutorials:
# Note that header = TRUE indicates that the first line contains the names of the
# variables (columns) while row.names=1 indicates that the first column contains the
# sample names (cases, rows).

ekofisk.species = read.csv("https://raw.githubusercontent.com/JLAcuna/marine-ecology/main/community_analysis/ekofisk-species.csv", header = TRUE, row.names = 1) 


# However, when you want to analyze your data, you will need to upload them
# from your computer (suposse your file is in "C:/my/file/directory/ekofisk-species.csv").
# In that case, you will need to run the line below after setting your correct
# file directory. I have outcommented the line because we are not using it now,
# but you will need it when analyzing the practical dataset 

# ekofisk.species <- read.csv("C:/my/file/directory/ekofisk-species.csv", header = TRUE, row.names = 1)


# shows the dataframe:
View(ekofisk.species)


# installs package vegan for calculation of a matrix of bray-curtis
# simmilarities between samples and other community analyses. A good manual
# is available at
# https://www.mooreecology.com/uploads/2/4/2/1/24213970/vegantutor.pdf
# this step is not necessary if you have vegan already installed. This is 
# why I have it commented out.

# install.packages("vegan") 


# loads vegan in the current session:
library(vegan) 


# creates a semimatrix with the bray-curtis simmilarities among all possible
# pairs of samples:
d <- vegdist(ekofisk.species, "bray") 


# visualizes the semimatrix:
d 


# performs cluster analysis using the average distance between groups
# of samples:
ekofisk.cluster <- hclust(d, method="average") 


# plots the result of the analysis, which is a dendrogram of simmilarities
# between samples:
plot(ekofisk.cluster) 


# plots the cluster again, but "hang" sets the fraction of the plot height by
# which labels should hang below the rest of the plot and "cex" sets the size
# of the labels text:
plot(ekofisk.cluster, hang = -1, cex = 0.6) 


# plots two blue squares arround the two first branches of the dendrogram:
rect.hclust(ekofisk.cluster, k = 2, border = "blue") 


# plots five red squares arround the five main branches:
rect.hclust(ekofisk.cluster, k = 5, border = "red") 


# creates the coded variable 'two.branches' where samples belonging to
# one branch are coded as 1 and samples belonging to the other branch
# is coded as 2:
two.branches<-cutree(ekofisk.cluster, 2) 


# shows the factor two.branches, with the code corresponding to each sample:
two.branches


# performs SIMPER between groups defined in factor "two.branches":
ekofisk.simper<-simper(ekofisk.species, two.branches) 


# reports the summary of SIMPER:
summary(ekofisk.simper, ordered = TRUE, digits = 3)  


##########################################################
#Ordination analysis: Non-Metric Multidimensional Scaling#
##########################################################

# Performs NMDS on the 'ekofisk.species' dataset to produce a 2D representation
ekofisk.NMDS<-metaMDS(ekofisk.species,k=2) 


############## a simple plot of the NMDS#########################

# quick, dirty plot of the NMDS, default in vegan
# red crosses are sample positions. Circles are species
plot(ekofisk.NMDS)

############## a fancier plot of the NMDS#########################


# We first extract the NMDS scores (x and y coordinates in the 
# NMDS plot) to a new dataframe called "NMDS.scores"
NMDS.scores = as.data.frame(scores(ekofisk.NMDS$species))


# We now visualize NMDS.scores
View(NMDS.scores)


# We want to relate the result of the NMDS with the dendrogram branches
# For this, we create a factor with codes for each of the five cluster groups
five.branches<-cutree(ekofisk.cluster, 5)


# visualize "five.branches"
View(five.branches)


# plots the NMDS scores without symbols. To make symbols dissapear
# we set their size to 0 using 'cex = 0'. Try using other symbol sizes
# to see them
plot(NMDS.scores$MDS1, # this is the value of NMDS axis 1
     NMDS.scores$MDS2, # this is the value of NMDS axis 2
     cex = 0) # this sets symbol size to zero, so symbols dissapear


# this adds the text labels to the plot. Because NMDS1 and NMDS2 are in
# dataframe "NMDS.scores", you have to call them as "NMDS.scores$NMDS1"
# and "NMDS.scores$NMDS2"
text(NMDS.scores$MDS1, NMDS.scores$MDS2, # sets the x and y axes 
     labels=rownames(ekofisk.species), # uses ekofisk sample names
     cex=0.6, # sets text size of the labels
     col = five.branches)# sets the color according to cluster groups


# this adds a legend with the group names and their corresponding colors
legend(x=-1.9,y=1, # position of the legend
       c("group 1","group 2", "group 3", "group 4", "group 5"), # group names
       cex=.7, # character size
       col = c("red", "black", "green", "deepskyblue1", "cyan"), # symbol color
       pch=19) # symbol type


##########################################################
#                      Diversity                         #
##########################################################


# this stores the number of species present in each sample in a variable called "S"
S<-specnumber(ekofisk.species) 


# this shows variable "S"
View(S)


# this stores the Shannon diversity indexfor each sample in a variable called "H"
H <- diversity(ekofisk.species)  


# this shows "H"
View(H)


# converts variable "ekofisk.groups" into a factor, and reorders the groups
#vin a sequence from closer to further from the center of the oil rig
five.branches <- factor(five.branches , levels=c("2", "1", "3", "4", "5")) 


# shows "five.branches"
View(five.branches)


# creates a boxplot for the number of species, according to the five groups
# discriminated using the dendrogram
boxplot(S~five.branches) 


# creates a boxplot for the Shannon diversity index, according to the five
# groups discriminated using the dendrogram
boxplot(H~five.branches)

