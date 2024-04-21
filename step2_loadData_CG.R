# checking which directory we're in - we want to access /Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/
getwd() 

# if you're not in the correct folder, use the following and replace everything before /Scanlan Kansas Risk Prediction Research
# setwd("C:.../Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/")

# path to access fileContents in the future
load(file = "../../preprocessing/fileContents.RData")

# access list of CSV files from folder path
fileList = dir(pattern = "\\.csv$") # back slashes are used to escape the period and permits the function to match file names that end with ".csv"
fileList

# forloop to read each file and create a separate data frame (takes a good minute to run)
for (fileName in fileList) { # for every file name in the file list
  
  # create a valid R variable name from the file name (remove the .csv part and other adjustments as needed)
  varName = gsub(".csv", "", fileName) # if there is a CSV extension in the file name, substitute a blank ("")
  varName = gsub("[^a-zA-Z0-9_]", "", varName) # if there are invalid characters in the column names, replace with blank ("")
  
  # read the file and assign it to a variable with that name
  assign(varName, read.csv(fileName))
  
}

# saving workspace (all objects in the global environment)
save.image(file = "../../preprocessing/step2_loadData.RData")
