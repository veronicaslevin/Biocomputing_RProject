#SLEVIN+WOLF BIOCOMPUTING R PROJECT
#USAGE: Complete Biocomputing R Project (prompt below)

#PART ONE: You have been contracted by the CDC to evalute the dynamics of an emerging disease outbreak and potential response strategy to the outbreak
#We are interested in answering 2 questions
#Q1: In which country (X or Y) did the disease outbreak likely begin?
#Q2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?

#Biology Lesson About Microsatellite Analysis
#If one or more of the markers are present in a patients sample, the patient was infected
#The disease is caused by a bacteria that is transmitted through air. Short gen time of bacteria causes rapid spread -> bacteria is evolving along transmission path
#Gene targeted by the microsatellite screen encodes the main protein shown to form an immunological response by the patient and so differences in which
#markers are present would indicate differences in the protein = different responses by a patients immune system
#if different markers are present -> evolving bacteria -> short gen time -> rapid spread

#Details About Data Provided
#Each country screens a large number of patients with symptoms daily. Data is contained in text files with "screening_NNN".txt where NNN = day of year
#Each file contains twelve columns- gender,age and ten columns for microsatellite markers (1=present, 0=absent)

#Primary goal: Answer both questions
#Addition goal: Provide code that could be used for future analyses by the CDC

#Code Requirements:
#2 scripts
#Script 1 (supportingFunctions.R): will contain a number of custom functions created to accomplish various data handling or summary tasks
#Script 2 (analysis.R): will use the source () function to load the function defined in Script 1, compile all data into a single csv file, process the data included in the entire data set 
#in order to answer the two questions above and provide graphical evidence for your answers. Use comments to explain the rational and how the graphical evidence supports your answer
#to the two questions

#SUDO RM -R .GIT
#################################################################################################################################################
rm(list = ls())#clean environment before run

#SETWD
setwd("~/Desktop/BIOCOMPUTING/R")

#LOAD LIBRARIES
library(ggplot2)
library(cowplot)

#LOAD FUNCTIONS
source("~/Desktop/BIOCOMPUTING/R/supportingFunction.R") #run all lines supporting functions script previous to data analysis steps

#Q1:In which country did the disease outbreak likely begin?
#ANSWER: Country X, there are 0 markers discovered in country Y until day 139 (GRAPH 1)

#DATA ANALYSIS

#Creating a data frame for all markers in Country X
markers_X<- dFX[, 3:12]
totalXmarkers<-data.frame("Markers"= rowSums(markers_X), "Day"=dFX$dayofYear )#data frame tracking markers by day of year for Country X

#Creating a data frame for all markers in Country Y
markers_Y<- dFY[, 3:12]
totalYmarkers<-data.frame("Markers"=rowSums(markers_Y), "Day"=dFY$dayofYear)#data frame tracking markers by day of year for Country Y 

#Finalizing marker data frames for country X and Y
SumsX<- c(rep(0, length(unique(totalXmarkers$Day))))
for(i in 120:175){
  with(totalXmarkers, sum(totalXmarkers$Markers[Day==i]))-> SumsX[i]}
SumsX[120:175]->SumsX

SumsY<- c(rep(0, 175))
for(i in 1:length(SumsY)){
  with(totalYmarkers, sum(totalYmarkers$Markers[Day==i]))-> SumsY[i]}
SumsY[120:175]->SumsY


#GRAPH 1: Displays marker values over time for both countries. The goal of this graph was to
#exemplify that rapid change and evolution of the bacteria throughout both countries. The graph
#is able to therefore display severity of spread and infection rates as a proxy of marker detection. 
Sums_xy<- data.frame("Day"=120:175,"X"=SumsX, "Y"=SumsY)
ggplot(Sums_xy, aes(x=Day))+
  geom_line(aes(y=X), color="blue")+ 
  geom_line(aes(y=Y), color="red")+ 
  theme_classic()+ 
  labs(y="Total Markers Detected")


#Q2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#ANSWER: NO 

#DATA ANALYSIS 

#Creation of cumulative sums for marker detections for day 120 and day 175 for each country
allmarks120X<- dFX[(dFX$dayofYear==120), 3:12]
sumx120<- c(rep(0, 10))
for(i in 1:ncol(allmarks120X)){
  sum(allmarks120X[,i])-> sumx120[i] #sum of marker data for day 120 in country X
}

allmarks175X<- dFX[(dFX$dayofYear==175), 3:12]
sumx175<- c(rep(0, 10))
for(i in 1:ncol(allmarks175X)){
  sum(allmarks175X[,i])-> sumx175[i]#sum of marker data for day 175 in country X
}

allmarks120Y<- dFY[(dFY$dayofYear==120), 3:12]
sumy120<- c(rep(0, 10))
for(i in 1:ncol(allmarks120Y)){
  sum(allmarks120Y[,i])-> sumy120[i]#sum of marker data for day 120 in country Y
}

allmarks175Y<- dFY[(dFY$dayofYear==175), 3:12]
sumy175<- c(rep(0, 10))
for(i in 1:ncol(allmarks175Y)){
  sum(allmarks175Y[,i])-> sumy175[i]#sum of marker data for day 175 in country Y
}


#GRAPHS FOR QUESTION 2

##Graph 1 shows the summary of marker expression/sums at day 120 in both countries. 
XYsums120<- data.frame("Markers"=rep(1:10, 2), "Sums"=c(sumx120, sumy120), "Identity"= c(rep("sumx120",10), rep("sumy120",10)))
ggplot(XYsums120, aes(fill=Identity, x=Markers,y=Sums))+ 
  geom_col(position="dodge")

##Graph 2 shows the summary of marker expression/sums at day 175 in both countries 
XYsums175<- data.frame("Markers"=rep(1:10, 2), "Sums"=c(sumx175, sumy175), "Identity"= c(rep("sumx175",10), rep("sumy175",10)))
ggplot(XYsums175, aes(fill=Identity, x=Markers,y=Sums))+ 
  geom_col(position="dodge")

##Graph 3 shows the summary of all markers (day 120 and day 175) in both countries
XYsums<- data.frame("Markers"=rep(1:10, 4), "Sums"=c(sumx120, sumy120, sumx175, sumy175), "Identity"= c(rep("sumx120",10), rep("sumy120",10), rep("sumx175",10), rep("sumy175",10)))
ggplot(XYsums, aes(fill=Identity, x=Markers, y=Sums))+ 
  geom_col(position="dodge")

#The goal of these graphs (1-3) was to show the sums of marker detections (by individual marker) to show the evolution of the bacteria throughout time, 
#and the feasibility for a single vaccine to help out both countries. With these graphs, we determined that due to the differences in markers being expressed,
#a vaccine created in Country Y would not be likely to work for Country X due to the differentiation of the disease (seen in 
#change of marker expression) through time. 

#OVERALL SUMMARY OF DATA
#Functions created to report back to user a summary table of key information regarding the outbreak for both countries
summaryY(dFY)
summaryX(dFX)
