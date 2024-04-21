# checking which directory we're in - we want to access /Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/
getwd() 

# if you're not in the correct folder, use the following and replace everything before /Scanlan Kansas Risk Prediction Research
# setwd("C:.../Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/")

dir() # checking the contents of the current directory path - you should see CSV files
fileList = dir() # saving this list of file names from current directory path

file = 1 # starting value of iteration
fileContents = list() # creating an empty list called fileContents to store the contents of each file

for (file in 1:length(fileList)){ # this loop iterates over each file in fileList, from first index to last index in fileList
  
  dataFile = read.csv(file = fileList[file]) # reads CSV file from current index (1 through ...) from fileList and stores data in dataFile
  fileContents[[file]] = names(dataFile) # extracts column names from dataFile and stores them in fileContents at the current index (1 through ...)
  names(fileContents)[file] = fileList[file] # sets the name of the current element in fileContents to be the name of the current file from fileList

  }

fileContents # checking the structure of the list 

# saves file names with respective column names list to a file named fileContents.RData
save(fileContents, file = "../../preprocessing/fileContents.RData") # double period backs out of the folder we're in
