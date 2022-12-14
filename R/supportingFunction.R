#Usage: Supporting Functions to R Project Script 2022
setwd("~/Desktop/BIOCOMPUTING/R")


#FUNCTION 1: FILE CONVERSION FUNCTION

#######FILE CONVERSION .TXT (TAB/SPACE DELIMITED) TO .CSV#######
setwd("~/Desktop/BIOCOMPUTING/R/countryY")

fileConversion<-function(filename) {
  textFile<-read.table(filename,header=TRUE,sep = "\t","")#read in .txt file
  csvFile<-write.csv(filename,row.names=TRUE,col.names=TRUE)#rewrite that file as a .csv file
  return(csvFile) #return newly written .csv to user
}

#FUNCTION 2: LOAD FILES + COMPILE DATA FROM EACH COUNTRY FOLDER

#load in data from countryX folder
setwd("~/Desktop/BIOCOMPUTING/R/countryX")#orient R to folder

screen_files<-list.files("~/Desktop/BIOCOMPUTING/R/countryX")#list all files in folder for Country X
dFX<-data.frame()#create empty data frame for files in Country X

for (i in 1:length(screen_files)) { #for loop to run through screen_files and put them into the empty data frame
  currentFile = read.csv(screen_files[i])
  currentFile$country<-"X"#add country 
  currentFile$dayofYear<-(i+119)#add dayofYear
  dFX=rbind(dFX,currentFile)#bind file information to empty data frame
}

#load in data from countryY folder
setwd("~/Desktop/BIOCOMPUTING/R/countryY")#orient R to folder

countryY<-list.files("~/Desktop/BIOCOMPUTING/R/countryY", (pattern = "\\.csv$"))#list all files in folder for Country Y
dFY<- data.frame()#create empty data frame for files in Country Y

for (i in 1:length(countryY)){#for loop to run through screen_files and put them into the empty data frame
  currentFile= read.csv(countryY[i])
  currentFile$country="Y"#add country 
  currentFile$dayofYear<-(i+119)#add dayofYear
  dFY<- rbind(dFY, currentFile)#bind file information to empty data frame
}

#FUNCTION 3: DEALING WITH NAs

NAcheckXY<-function(dFX,dFY) {
  NAcheck<-is.na(dFX,dFY)#check for NAs in files
  NAcount<-sum(NAcheck)#report back to user number of NAs
  print(NAcount)
}


#FUNCTION 4: SUMMARIZE ALL DATA (# of screens, %infected, # of male/female patients, min/maxAge)

############SET UP FOR SUMMARY FUNCTIONS###############
#PERCENT INFECTED PATIENTS X+Y
pIdFX<-dFX[,c(3:12)] #consolidating marker information into new data frame to determine country X infection rates
for (i in 1:length(pIdFX)) {
  dFX$infected<-rowSums(pIdFX)#sum of all rows in percent infected data frame added to country X data frame
}

pIdFY<-dFY[,c(3:12)] #consolidating marker information into new data frame to determine country Y infection rates
for (i in 1:length(pIdFY)) {
  dFY$infected<-rowSums(pIdFY)#sum of all rows in percent infected data frame added to country Y data frame
}

########SUMMARY FUNCTIONS############
summarydFX<-data.frame()#create empty data frame for values
summaryX<-function(dFX) {#Country X summary function
  numScreens<-sum(nrow(dFX))#sum of screen numbers
  percentInfected=sum(dFX$infected>"1")/numScreens*100#calculating percent infected with number of infected patients (marker detections>1)/number of screens
  numMale<-sum(dFX$gender=="male")#sum of male patients in Country X
  numFemale<-sum(dFX$gender=="female")#sum of female patients in Country X
  minAge<-min(dFX$age)#min age in Country X
  maxAge<-max(dFX$age)#max age in Country X
  summarydFX<-data.frame(Country="X",numScreens=numScreens,percentInfected=percentInfected,numMale=numMale,numFemale=numFemale,minAge=minAge,maxAge=maxAge)
  return(summarydFX)#All values placed into new data frame that appears to user when function is run
}

summarydFY<-data.frame()#create empty data frame for values
summaryY<-function(dFY) {#Country Y summary function
  numScreens<-sum(nrow(dFY))#sum of screen numbers
  percentInfected=sum(dFY$infected>"1")/numScreens*100#calculating percent infected with number of infected patients (marker detections>1)/number of screens
  numMale<-sum(dFY$gender=="male")#sum of male patients in Country Y
  numFemale<-sum(dFY$gender=="female")#sum of female patients in Country Y
  minAge<-min(dFY$age)#min age in Country Y
  maxAge<-max(dFY$age)#max age in Country Y
  summarydFY<-data.frame(Country="Y",numScreens=numScreens,percentInfected=percentInfected,numMale=numMale,numFemale=numFemale,minAge=minAge,maxAge=maxAge)
  return(summarydFY)#All values placed into new data frame that appears to user when function is run
}

