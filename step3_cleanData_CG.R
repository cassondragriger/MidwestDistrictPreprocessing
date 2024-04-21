rm(list=ls())

# checking which directory we're in - we want to access /Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/
getwd() 

# if you're not in the correct folder (KCKPS At Risk Reports), use the following and replace ... with your own path before /Scanlan Kansas Risk Prediction Research
setwd("C:/Users/cgriger/Dropbox/Academia/IOWA/NASP/Replicability and Tutorial of School Readiness Profiles and Growth/Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/")

# loading CSV files from the saved environment (step2_loadData.RData)
load(file = "../../preprocessing/step2_loadData.RData")

# packages
if (!require("readxl")) install.packages("readxl")
library(readxl)

################################################################################
################### PREPROCESSING DATA FOR MERGE ###############################
################################################################################

# Buildings.csv [DONE]
################################################################################

unique(Buildings$school_type) # 7 types
unique(Buildings$school_name) # 120 types

L2_Buildings = subset(Buildings, select = -c(district_name)) # all "Kansas City Public Schools"

# SA.csv [DONE]
################################################################################

preL0_DID_SA = DID_SA # preserving original data file

n = unique(preL0_DID_SA$state_student_number) # n = 23042
n = unique(preL0_DID_SA$acct_bldg) # n = 51

names(preL0_DID_SA)[names(preL0_DID_SA) == 'state_id'] = 'state_student_number' # standardize column names; keep full label until all merges are complete!
names(preL0_DID_SA)[names(preL0_DID_SA) == 'comprehensive_race'] = 'compRace' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'primary_exceptionality_code'] = 'xcept1' # for mplus 8-character; exception from general ed requirements due to disability or no disability 
names(preL0_DID_SA)[names(preL0_DID_SA) == 'secondary_exceptionality_code'] = 'xcept2' # for mplus 8-character; exception from general ed requirements due to disability or no disability 
names(preL0_DID_SA)[names(preL0_DID_SA) == 'residence_of_homeless_student_while_homeless'] = 'homeless' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'test_type'] = 'typeKAP' # for mplus 8-character; KAP is their state standardized test
names(preL0_DID_SA)[names(preL0_DID_SA) == 'total_questions'] = 'numqKAP' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'total_responses'] = 'respKAP' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'scale_score'] = 'scaleKAP' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'performance_category'] = 'catKAP' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'hispanic_ethnicity'] = 'hispanic' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'last_entry_date'] = 'lstEntry' # for mplus 8-character
names(preL0_DID_SA)[names(preL0_DID_SA) == 'last_district_entry_date'] = 'disEntry' # for mplus 8-character

# change date format of last_entry_date and last_district_entry_date from MM/DD/YY H:MM to MM/DD/YYYY
preL0_DID_SA$lstEntry = as.Date(preL0_DID_SA$lstEntry, format="%m/%d/%Y")
preL0_DID_SA$disEntry = as.Date(preL0_DID_SA$disEntry, format="%m/%d/%Y")
preL0_DID_SA$diffDay = as.numeric(preL0_DID_SA$lstEntry - preL0_DID_SA$disEntry)
preL0_DID_SA$diffYear = preL0_DID_SA$diffDay/365 # hundreths scale, not 12
  
L0_DID_SA = subset(preL0_DID_SA, select = -c(subject, # all 1's
                                                  acct_org, # all D0500's
                                                  unnamed, # all NA's
                                                  acct_status_building, # accountability status; Zach said don't worry about this
                                                  acct_status_district, # accountability status; Zach said don't worry about this
                                                  acct_status_state, # accountability status; Zach said don't worry about this
                                                  primary_language, # all NA's
                                                  testid, # all NA's
                                                  reclass, # all NA's
                                                  military_connected_student_indicator)) # all 0's

colnames(L0_DID_SA) # checking remaining columns
summary(L0_DID_SA) # checking ranges for continuous variables
unique_values_list = lapply(L0_DID_SA, unique) # view unique values of each column
unique(L0_DID_SA$homeless) # checking categories
n = unique(L0_DID_SA$state_student_number) # n = 23042

# StudentDetails.csv [DONE]
################################################################################

# checking and removing NA's
na_count = sum(is.na(DID_StudentDetails$state_student_number)) # n = 79; matches to duplicates
preL0_DID_StudentDetails = DID_StudentDetails[!is.na(DID_StudentDetails$state_student_number), ] # 347150 - 79 = 347071; checking the math, there were 79 NA's to remove

# create unique_id
preL0_DID_StudentDetails$ss = paste(preL0_DID_StudentDetails$state_student_number, preL0_DID_StudentDetails$school_year, sep = "_") 
preL0_DID_StudentDetails$unique_id = paste(preL0_DID_StudentDetails$ss, preL0_DID_StudentDetails$school_code, sep = "_") 

# assign priorities to lunch_codes
preL0_DID_StudentDetails$lunch_priority = match(preL0_DID_StudentDetails$lunch_code, c('F', 'R', 'S', ''), nomatch = length(c('F', 'R', 'S', '')) + 1)

# sort data by unique_id and lunch_priority
preL0_DID_StudentDetails = preL0_DID_StudentDetails[order(preL0_DID_StudentDetails$unique_id, preL0_DID_StudentDetails$lunch_priority), ]

# remove duplicates while keeping the highest priority row for each unique_id
preCleanL0_DID_StudentDetails = preL0_DID_StudentDetails[!duplicated(preL0_DID_StudentDetails$unique_id), ]

L0_DID_StudentDetails = subset(preCleanL0_DID_StudentDetails, select = -c(class_of,
                                                                          student_email,
                                                                          ss)) #  

L0_DID_StudentDetails$closed = as.integer(grepl("^z|^x", L0_DID_StudentDetails$school_name))

names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'date_of_birth'] = 'DOB'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'ethnicity'] = 'ethnic'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'grade_level'] = 'grade'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'Iep'] = 'IEP'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'low_income'] = 'lowINC'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'disability'] = 'disabl'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'local_student_number'] = 'localID'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'lunch_code'] = 'lunch'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'unique_id'] = 'uniqID'
names(L0_DID_StudentDetails)[names(L0_DID_StudentDetails) == 'lunch_priority'] = 'lunchPRI'

################################################################################
# MPLUS LABELING FUNCTION ######################################################
################################################################################

# function to create abbreviations
abbreviate_label = function(label) { 
  
  # split the label by non-alphanumeric characters and take the first letter of each word
  parts = unlist(strsplit(label, "[^A-Za-z0-9]"))
  abbreviation = paste0(substr(parts, 1, 1), collapse = "")
  
  # if the abbreviation is too short, add additional characters from the first word
  if (nchar(abbreviation) < 4) {
    abbreviation = paste0(abbreviation, substr(parts[1], 2, 4 - nchar(abbreviation)))
  }
  
  # if the abbreviation is too long, truncate it to 8 characters
  abbreviation = substr(abbreviation, 1, 4)
  
  return(toupper(abbreviation)) # convert to uppercase for consistency
  
}

# Enrollment.csv [DONE]
################################################################################

unique_values_list = lapply(Enrollment, unique) 

preL1_Enrollment = Enrollment # preserving original data file

names(preL1_Enrollment)[names(preL1_Enrollment) == 'state_student_id'] = 'state_student_number' # for mplus 8-character
names(preL1_Enrollment)[names(preL1_Enrollment) == 'entry_building_code'] = 'bldgCode' # for mplus 8-character
names(preL1_Enrollment)[names(preL1_Enrollment) == 'withdrawal_code'] = 'codeWD' # for mplus 8-character
names(preL1_Enrollment)[names(preL1_Enrollment) == 'entry_date'] = 'entry' # for mplus 8-character
names(preL1_Enrollment)[names(preL1_Enrollment) == 'withdrawal_date'] = 'wdEntry' # for mplus 8-character
names(preL1_Enrollment)[names(preL1_Enrollment) == 'district_enroll_date'] = 'disEnroll' # for mplus 8-character

preL1_Enrollment$WD = 1# setting binary indicator for reshape function
preL1_Enrollment$codeWD[preL1_Enrollment$codeWD == ""] = 0 # replace blanks in withdrawal codes with 0's

# checking for duplicates; n = 25135 unique duplicates of student + year!
preL1_Enrollment$unique_id = paste(preL1_Enrollment$state_student_number, preL1_Enrollment$school_year, sep = "_")
duplicates = preL1_Enrollment[duplicated(preL1_Enrollment$unique_id) | duplicated(preL1_Enrollment$unique_id, fromLast = TRUE), ]
n = unique(duplicates$unique_id)

L1_Enrollment = subset(preL1_Enrollment, select = -c(entry_code, # all 1's
                                               entry_desc, # all NA's
                                               withdrawal_desc, # text descriptions, best for qual analysis
                                               withdrawal_building_code, # few random words in one cell
                                               us_enroll_date)) # all NA's

colnames(L1_Enrollment) # checking remaining columns
categoriesWD = unique(L1_Enrollment$codeWD) # pulling all possible codes for withdrawal

aggL1_Enrollment = aggregate(cbind(count = codeWD) ~ state_student_number + school_year + bldgCode + codeWD, 
                      data = L1_Enrollment, 
                      FUN = length)

WideL1_Enrollment = reshape(aggL1_Enrollment, 
                        idvar = c("state_student_number","school_year", "bldgCode"), 
                        timevar = "codeWD",
                        direction = "wide",
                        sep = "")

enroll_new_names = gsub("count", "WD", names(WideL1_Enrollment)) # replacing colnames that have "count" in their string with "WD"
names(WideL1_Enrollment) = enroll_new_names # applying the new names to the dataframe

WideL1_Enrollment[ , 3:31] = lapply(WideL1_Enrollment[ , 3:31], function(x) { x[is.na(x)] = NA; return(x) }) # fills in 0's for dummy coded withdrawal reasons

################################################################################
######################### MERGING WITHDRAWAL ###################################
################################################################################

# RELABEL WD TYPES OF keyWithdrawal ############################################

# keyWithdrawal = read_excel(file.choose()) # use if read_excel is not working
keyWithdrawal = read_excel("C:/Users/cgriger/Dropbox/Academia/IOWA/NASP/Replicability and Tutorial of School Readiness Profiles and Growth/Scanlan Kansas Risk Prediction Research/supporting documents/withdrawal codes/WithdrawalTypeCategorize.xlsx")

# creating a list of column names and removing first column name from list
WD_types = colnames(keyWithdrawal)[-c(1:2)]

# apply the abbreviation function to each discipline type
abbreviated_WD = sapply(WD_types, abbreviate_label)
print(abbreviated_WD)

# check for duplicates and adjust manually if needed
duplicates_WD = duplicated(abbreviated_WD)
print(duplicates_WD)

# replacing the column names of keyDiscipline (except the first 2 cols) with the new labels
colnames(keyWithdrawal)[-c(1:2)] = paste0("cat", abbreviated_WD)

# ensuring that the keyWithdrawal matrix has the correct number of rows (WD type x WD category)
key_matrixWD = as.matrix(keyWithdrawal[, -c(1:2)])  # transpose and exclude the 'WD_code' column

# PROCESSING COLUMNS OF DATA ###################################################

# ordering WD's in similar way to key (in tens numerical order; not ones place)
WD_ordering = keyWithdrawal$codeWD
WD_new_order = c(WD_ordering, setdiff(names(WideL1_Enrollment), WD_ordering))
WideL1_Enrollment = WideL1_Enrollment[WD_new_order]

# identifying the range of columns in pre_mainData based on the first and last column names
first_indexWD = which(names(WideL1_Enrollment) == "WD0")
last_indexWD = which(names(WideL1_Enrollment) == "WD99")

# select the range of columns from the first to the last by index
selected_main_df2 = WideL1_Enrollment[, c(first_indexWD:last_indexWD)]
colnames(selected_main_df2)

# convert all the relevant columns to numeric
selected_main_df2[] = lapply(selected_main_df2, function(x) as.numeric(as.character(x)))

# checking range of values
print(summary(selected_main_df2))
print(summary(key_matrixWD))
str(selected_main_df2)
str(key_matrixWD)

# PRESENCE OF WITHDRAWAL (0/1) & SUMMED WITHDRAWAL (COUNT) ####################

# ensure dimensions match for matrix multiplication
selected_main_df2[is.na(selected_main_df2)] = 0
key_matrixWD[is.na(key_matrixWD)] = 0

if (ncol(selected_main_df2) == nrow(key_matrixWD)) {
  
  # calculating presence of withdrawals
  category_matrixWD = as.matrix(selected_main_df2) %*% key_matrixWD
  category_matrixWD[category_matrixWD > 0] = 1
  
  # calculating summed disciplines
  summed_category_matrixWD = as.matrix(selected_main_df2) %*% key_matrixWD
  
  # combining both matrices with original pre_mainData
  mainWideL1_Enrollment = cbind(WideL1_Enrollment, as.data.frame(category_matrixWD), as.data.frame(summed_category_matrixWD))
  
  # renaming columns for presence of disciplines
  colnames(mainWideL1_Enrollment)[(ncol(WideL1_Enrollment) + 1):(ncol(WideL1_Enrollment) + ncol(category_matrixWD))] = colnames(keyWithdrawal)[-c(1:2)]
  
  # renaming columns for summed disciplines
  new_column_names2 = paste0("s", colnames(keyWithdrawal)[-c(1:2)])
  
  # correcting the indices for the summed disciplines columns
  colnames(mainWideL1_Enrollment)[(ncol(WideL1_Enrollment) + ncol(category_matrixWD) + 1):(ncol(WideL1_Enrollment) + ncol(category_matrixWD) * 2)] = new_column_names2
  
} else {
  stop("Dimension mismatch for matrix multiplication.")
}

# check to see if there are any duplicate column names within main data
duplicates_main= duplicated(colnames(mainWideL1_Enrollment))
print(duplicates_main) # good to go! (:

# Discipline.csv [DONE]
################################################################################

unique_values_list = lapply(Discipline, unique) # view unique values of each column

aggL1_Discipline= aggregate(cbind(count = Discipline$discipline_type) ~ state_student_number + school_year + discipline_type, 
                                    data = Discipline, 
                                    FUN = length)

categoriesDiscipline = unique(aggL1_Discipline$discipline_type) # pulling all possible codes for discipline

# checking for duplicates; n = 8,420 unique duplicates of student + year!
aggL1_Discipline$unique_id = paste(aggL1_Discipline$state_student_number, aggL1_Discipline$school_year, sep = "_")
duplicates = aggL1_Discipline[duplicated(aggL1_Discipline$unique_id) | duplicated(aggL1_Discipline$unique_id, fromLast = TRUE), ]
n = unique(duplicates$unique_id)

WideL1_Discipline = reshape(aggL1_Discipline, 
                               idvar = c("state_student_number","school_year"), 
                               timevar = "discipline_type",
                               v.names = "count",
                               direction = "wide",
                               sep = "_")

colnames(WideL1_Discipline)
unique_values_list = lapply(WideL1_Discipline, unique)

# calculating a total for discipline referrals
WideL1_Discipline[is.na(WideL1_Discipline)] = 0 # setting NA's to 0 to represent 0 discipline referrals and allow a sum of disciplines
start_col = which(names(WideL1_Discipline) == "count_Academic Dishon Cheat Forgery") # first column of discipline types
end_col = which(names(WideL1_Discipline) == "count_Violation of General School Rules and/or School Disruption") # last column of discipline types
WideL1_Discipline$totDisc = rowSums(WideL1_Discipline[, start_col:end_col]) # summing all discipline types to achieve a total count

################################################################################
############ CREATING/APPLYING ABBREVIATED DISCIPLINE LABELS ###################
################################################################################

# reading in "manually created key" with discipline categories
keyDiscipline = read_excel("C:/Users/cgriger/Dropbox/Academia/IOWA/NASP/Replicability and Tutorial of School Readiness Profiles and Growth/Scanlan Kansas Risk Prediction Research/supporting documents/discipline codes/DisciplineTypeCategorize.xlsx")

# PROCESSING COLUMNS OF ALL DISCIPLINE DATA ####################################

# replacing column name
names(keyDiscipline)[names(keyDiscipline) == "Discipline Type"] = "discType"

# removing "count_" label from discipline types in mainData
names(WideL1_Discipline) = gsub("count_", "", names(WideL1_Discipline))

# saving list of discipline types
discipline_types = keyDiscipline$discType

# apply the abbreviation function to each discipline type
abbreviated_types = sapply(discipline_types, abbreviate_label)
print(abbreviated_types)

# RELABEL DISCIPLINE TYPES OF WideL1_Discipline 

# modify the colnames of mainData to reflect the same as the key
start_index = which(names(WideL1_Discipline) == "Academic Dishon Cheat Forgery")
end_index = which(names(WideL1_Discipline) == "Violation of General School Rules and/or School Disruption")

if (length(abbreviated_types) != (end_index - start_index + 1)) {
  stop("The number of new column names does not match the number of columns to rename.")
}

# relabeling mainData columns with corresponding labels to key
names(WideL1_Discipline)[start_index:end_index] = abbreviated_types

# RELABEL DISCIPLINE TYPES OF keyDiscipline 

# check for duplicates and adjust manually if needed
duplicates_types = duplicated(abbreviated_types)
print(duplicates_types)

# replacing the row names of keyDiscipline (except the first one) with the new labels
keyDiscipline$discType = abbreviated_types

# RELABEL DISCIPLINE CATEGORIES OF keyDiscipline 

# establishing MPlus-friendly labels for new discipline categories
discipline_categories = as.list(names(keyDiscipline)[-1])

# apply the abbreviation function to each discipline categories
abbreviated_categories = sapply(discipline_categories, abbreviate_label)

# checking for duplicates in abbreviations; only one is for TRU (i.e., truancy)
duplicates_categories = intersect(abbreviated_types, abbreviated_categories)

# adding "cat" to avoid confusion with discipline types and categories
cat_abbreviated_categories = paste0("cat", abbreviated_categories)

# replacing the column names of keyDiscipline (except the first one) with the new labels
colnames(keyDiscipline)[-1] = cat_abbreviated_categories

# ensuring that the keyDiscipline matrix has the correct number of rows
key_matrixDP = as.matrix(keyDiscipline[, -1])  # transpose and exclude the 'discType' column

# PREPARING WideL1_Discipline

# identifying the range of columns in pre_mainData based on the first and last column names
first_index = which(names(WideL1_Discipline) == "ADCF")
last_index = which(names(WideL1_Discipline) == "VOGS")

# making temp data frame to select the range of columns from the first to the last by index
selected_main_df1 = WideL1_Discipline[, c(first_index:last_index)]

# convert all the relevant columns to numeric
selected_main_df1[] = lapply(selected_main_df1, function(x) as.numeric(as.character(x)))

# checking range of values
print(summary(selected_main_df1))
print(summary(key_matrixDP))
str(selected_main_df1)
str(key_matrixDP)

# PRESENCE OF DISCIPLINES (0/1) & SUMMED DISCIPLINES (COUNT) 

# ensure dimensions match for matrix multiplication
selected_main_df1[is.na(selected_main_df1)] = 0
key_matrixDP[is.na(key_matrixDP)] = 0

if (ncol(selected_main_df1) == nrow(key_matrixDP)) {
  
  # calculating presence of disciplines
  category_matrix = as.matrix(selected_main_df1) %*% key_matrixDP
  category_matrix[category_matrix > 0] = 1
  
  # calculating summed disciplines
  summed_category_matrix = as.matrix(selected_main_df1) %*% key_matrixDP
  
  # combining both matrices with original pre_mainData
  mainWideL1_Discipline = cbind(WideL1_Discipline, as.data.frame(category_matrix), as.data.frame(summed_category_matrix))
  
  # renaming columns for presence of disciplines
  colnames(mainWideL1_Discipline)[(ncol(WideL1_Discipline) + 1):(ncol(WideL1_Discipline) + ncol(category_matrix))] = colnames(keyDiscipline)[-1]
  
  # renaming columns for summed disciplines
  new_column_names = paste0("s", colnames(keyDiscipline)[-1])
  
  # correcting the indices for the summed disciplines columns
  colnames(mainWideL1_Discipline)[(ncol(WideL1_Discipline) + ncol(category_matrix) + 1):(ncol(WideL1_Discipline) + ncol(category_matrix) * 2)] = new_column_names
  
} else {
  stop("Dimension mismatch for matrix multiplication.")
}

# AttendanceRate.csv [DONE]
################################################################################

unique_values_list = lapply(AttendanceRate, unique) # view unique values of each column

preL1_AttendanceRate = AttendanceRate # preserving original data file

n = unique(preL1_AttendanceRate$state_student_number) # n = 37050

names(preL1_AttendanceRate)[names(preL1_AttendanceRate) == 'membership_days'] = 'daysTot' # for mplus 8-character
names(preL1_AttendanceRate)[names(preL1_AttendanceRate) == 'days_absent'] = 'daysAbs' # for mplus 8-character
names(preL1_AttendanceRate)[names(preL1_AttendanceRate) == 'days_present'] = 'daysPres' # for mplus 8-character

WideL1_AttendanceRate = subset(preL1_AttendanceRate, select = -run_date) # date that data was pulled

colnames(WideL1_AttendanceRate) # checking remaining columns
summary(WideL1_AttendanceRate) # checking ranges for continuous variables
n = unique(WideL1_AttendanceRate$state_student_number) # n = 37050

# FullDayAttendance.csv [DONE]
################################################################################

unique_values_list = lapply(FullDayAttendance, unique) # view unique values of each column
unique(FullDayAttendance$absence_code)
unique(FullDayAttendance$absence_desc)

FullDayAttendance$absence_desc[FullDayAttendance$absence_desc == "HS"] = "Health Services" # replace blanks in withdrawal codes with 0's

aggL1_FullDayAttendance = aggregate(cbind(count = FullDayAttendance$absence_code) ~ state_student_number + school_year + absence_code, 
                                    data = FullDayAttendance, 
                                    FUN = length)

aggL1_FullDayAttendance$absence_code[aggL1_FullDayAttendance$absence_code == ""] = "UNKWN"

WideL1_FullDayAttendance = reshape(aggL1_FullDayAttendance, 
                                   idvar = c("state_student_number","school_year"), 
                                   timevar = "absence_code",
                                   v.names = "count",
                                   direction = "wide",
                                   sep = "")

abs_new_names = gsub("count", "abs", names(WideL1_FullDayAttendance)) # replacing colnames that have "count_" in their string with "WD"
names(WideL1_FullDayAttendance) = abs_new_names # applying the new names to the dataframe

WideL1_FullDayAttendance[ , 3:14] = lapply(WideL1_FullDayAttendance[ , 3:14], function(x) { x[is.na(x)] = NA; return(x) }) # fills in 0's for dummy coded withdrawal reasons

# PeriodAttendance 1.csv [DONE]
################################################################################

unique_values_list = lapply(PeriodAttendance1, unique)
unique(PeriodAttendance1$absence_code) # checking columns values
unique(PeriodAttendance1$absence_desc) # checking column values

PeriodAttendance1$absence_desc[PeriodAttendance1$absence_desc == "HS"] = "Health Services" # replace blanks in withdrawal codes with 0's

aggL1_PeriodAttendance = aggregate(cbind(count = PeriodAttendance1$absence_desc) ~ state_student_number + school_year + absence_code, 
                                data = PeriodAttendance1, 
                                FUN = length)

aggL1_PeriodAttendance$absence_code[aggL1_PeriodAttendance$absence_code == ""] = "UNKWN"

WideL1_PeriodAttendance = reshape(aggL1_PeriodAttendance, 
                              idvar = c("state_student_number","school_year"), 
                              timevar = "absence_code",
                              v.names = "count",
                              direction = "wide",
                              sep = "")

abs_new_names = gsub("count", "per", names(WideL1_PeriodAttendance)) # replacing colnames that have "count_" in their string with "WD"
names(WideL1_PeriodAttendance) = abs_new_names # applying the new names to the dataframe

WideL1_PeriodAttendance[ , 3:14] = lapply(WideL1_PeriodAttendance[ , 3:14], function(x) { x[is.na(x)] = NA; return(x) }) # fills in 0's for dummy coded withdrawal reasons

# KELPA.csv [DONE]
################################################################################

preL1_KELPA = KELPA  # preserving original data file

names(preL1_KELPA)[names(preL1_KELPA) == 'value'] = 'KELP'
names(preL1_KELPA)[names(preL1_KELPA) == 'esol_code'] = 'esol'

preL1_KELPA$grade = preL1_KELPA$grade_code - 5 # for grade-levels, minus 5 to achieve true grade
preL1_KELPA$grade_code = NULL

# shortening domain values (renamed as KELPA to prep for reshape)
unique(KELPA$domain)
preL1_KELPA$domain[preL1_KELPA$domain == "Listening.Performance.Level"] = "LPL"
preL1_KELPA$domain[preL1_KELPA$domain == "Reading.Performance.Level"] = "RPL"
preL1_KELPA$domain[preL1_KELPA$domain == "Speaking.Performance.Level"] = "SPL"
preL1_KELPA$domain[preL1_KELPA$domain == "Write.Performance.Level"] = "WPL"
preL1_KELPA$domain[preL1_KELPA$domain == "Proficiency.Determination"] = "PD"

WideL1_KELPA = reshape(preL1_KELPA, 
                       idvar = c("state_student_number",
                                 "school_code",
                                 "grade",
                                 "esol",
                                 "school_year"), 
                       timevar = "domain", 
                       direction = "wide",
                       sep = "_")

# FBMath_aMath.csv (school_year; state_id; test_term; test_metric; attribute)
################################################################################

preL1_FBMath_aMath = FBMath_aMath  # preserving original data file

names(preL1_FBMath_aMath)[names(preL1_FBMath_aMath) == 'value'] = 'FBm'
names(preL1_FBMath_aMath)[names(preL1_FBMath_aMath) == 'state_id'] = 'state_student_number'

unique(preL1_FBMath_aMath$test_metric)
preL1_FBMath_aMath$test_metric[preL1_FBMath_aMath$test_metric == 'Test Theta'] = 'Thet'
preL1_FBMath_aMath$test_metric[preL1_FBMath_aMath$test_metric == 'Percentile at School'] = 'pSCH'
preL1_FBMath_aMath$test_metric[preL1_FBMath_aMath$test_metric == 'Percentile at LEA'] = 'pLEA'
preL1_FBMath_aMath$test_metric[preL1_FBMath_aMath$test_metric == 'Percentile at Nation'] = 'pNAT'
preL1_FBMath_aMath$test_metric[preL1_FBMath_aMath$test_metric == 'Risk Level'] = 'rLVL'
preL1_FBMath_aMath$test_metric[preL1_FBMath_aMath$test_metric == 'Final Date'] = 'Fin'

# checking for duplicates; n = 172 duplicates
preL1_FBMath_aMath$unique_id = paste(preL1_FBMath_aMath$state_student_number,preL1_FBMath_aMath$test_metric, sep = "_") # n = 88304 rows; creating a unique_id to identify duplicates of student + test metric
duplicates_FBaMath = preL1_FBMath_aMath[duplicated(preL1_FBMath_aMath$unique_id) | duplicated(preL1_FBMath_aMath$unique_id, fromLast = TRUE), ] # n = 172 rows
n = unique(duplicates_FBaMath$unique_id) # 48 unique cases; small amount of occurrences but it's fixable
na_count = sum(is.na(preL1_FBMath_aMath$state_student_number)) # n = 88; matches to duplicates
na_dup_count = sum(is.na(duplicates_FBaMath$state_student_number)) # n = 88; matches to main data frame

# checking and removing NA's
duplicates_minus_NAs = duplicates_FBaMath[!is.na(duplicates_FBaMath$state_student_number), ] # 172 - 88 = 84; checking the math, there were 88 NA's to remove
preCleanL1_FBMath_aMath = preL1_FBMath_aMath[!is.na(preL1_FBMath_aMath$state_student_number), ] # 88216

# loop through all duplicates and append "b" to the second occurrence
unique_id_dupes = duplicated(preCleanL1_FBMath_aMath$unique_id) | duplicated(preCleanL1_FBMath_aMath$unique_id, fromLast = TRUE) # 88216 (88304 - 88 NA's); logical (TRUE/FALSE) bc it's faster (:
for(i in which(unique_id_dupes)){
  
  if(i > 1 && preCleanL1_FBMath_aMath$unique_id[i] == preCleanL1_FBMath_aMath$unique_id[i - 1]){   # check if the previous row is a duplicate of the same unique_id
    
    preCleanL1_FBMath_aMath$state_student_number[i] = paste0( "dupe", preCleanL1_FBMath_aMath$state_student_number[i]) # if the second cell is found to be a duplicate of the first cell, replace the second cell's state_student_number with "b" + their number
    
  }
}

L1_FBMath_aMath = subset(preCleanL1_FBMath_aMath, select = -c(assessment, # all aMath
                                                              test_term, # all Fall
                                                              sub_test, # all CHECK
                                                              attribute, # redundant; combined information from other columns
                                                              unique_id)) # causes issues in reshaping; no longer needed

WideL1_FBMath_aMath = reshape(L1_FBMath_aMath, 
                              idvar = c("state_student_number",
                                        "school_year"), 
                              timevar = "test_metric", 
                              direction = "wide",
                              sep = "")

colnames(WideL1_FBMath_aMath)

# FBMathEarlyMath.csv [DONE]
################################################################################

unique_values_list = lapply(FBMathEarlyMath, unique) 

names(FBMathEarlyMath)[names(FBMathEarlyMath) == 'value'] = 'FBem'
names(FBMathEarlyMath)[names(FBMathEarlyMath) == 'state_id'] = 'state_student_number'

# checking for duplicates; n = 10 duplicates, NA's for state_id
FBMathEarlyMath$unique_id = paste(FBMathEarlyMath$state_student_number,FBMathEarlyMath$test_metric, sep = "_")
duplicates_FBEarlyMath = FBMathEarlyMath[duplicated(FBMathEarlyMath$unique_id) | duplicated(FBMathEarlyMath$unique_id, fromLast = TRUE), ]
n = unique(duplicates_FBEarlyMath$unique_id) # 2 types of repetitions

# identifying specific duplicates and removing them
all_duplicates = duplicated(FBMathEarlyMath$unique_id) | duplicated(FBMathEarlyMath$unique_id, fromLast = TRUE) # identifying all duplicates with logic (TRUE/FALSE)
preL1_FBMathEarlyMath = FBMathEarlyMath[!all_duplicates, ] # remove all duplicates from working data frame; 6028 rows to 6018 rows

# renaming test_metric values to prep for reshaping
unique(preL1_FBMath_aMath$test_metric)
preL1_FBMathEarlyMath$test_metric[preL1_FBMathEarlyMath$test_metric == 'Percentile at Nation'] = 'pNAT'
preL1_FBMathEarlyMath$test_metric[preL1_FBMathEarlyMath$test_metric == 'Risk Level'] = 'rLVL'

L1_FBMathEarlyMath = subset(preL1_FBMathEarlyMath, select = -c(assessment, # all aMath
                                                         test_term, # all Fall
                                                         sub_test, # all CHECK
                                                         attribute, # redundant; combined information from other columns
                                                         unique_id)) # causes issues in reshaping; no longer needed

WideL1_FBMathEarlyMath = reshape(L1_FBMathEarlyMath, 
                             idvar = c("state_student_number",
                                       "school_year"), 
                             timevar = "test_metric", 
                             direction = "wide",
                             sep = "")

colnames(WideL1_FBMathEarlyMath)

# FBReading .csv [DONE]
################################################################################

unique_values_list = lapply(FBReading, unique) 

names(FBReading)[names(FBReading) == 'value'] = 'FBr'
names(FBReading)[names(FBReading) == 'state_id'] = 'state_student_number'

# checking for duplicates; n = 12 duplicates, NA's for state_id
FBReading$unique_id = paste(FBReading$state_student_number,FBReading$test_metric, sep = "_")
duplicates_FBReading = FBReading[duplicated(FBReading$unique_id) | duplicated(FBReading$unique_id, fromLast = TRUE), ]
n = unique(duplicates_FBReading$unique_id) # 2 types of repetitions

# identifying specific duplicates and removing them
all_duplicates = duplicated(FBReading$unique_id) | duplicated(FBReading$unique_id, fromLast = TRUE) # identifying all duplicates with logic (TRUE/FALSE)
preL1_FBReading = FBReading[!all_duplicates, ] # remove all duplicates from working data frame; 6202 rows to 6190 rows

# renaming test_metric values to prep for reshaping
unique(preL1_FBMath_aMath$test_metric)
preL1_FBReading$test_metric[preL1_FBReading$test_metric == 'Percentile at Nation'] = 'pNAT'
preL1_FBReading$test_metric[preL1_FBReading$test_metric == 'Risk Level'] = 'rLVL'

L1_FBReading = subset(preL1_FBReading, select = -c(assessment, # all aMath
                                                test_term, # all Fall
                                                sub_test, # all CHECK
                                                attribute, # redundant; combined information from other columns
                                                unique_id)) # causes issues in reshaping; no longer needed

WideL1_FBReading = reshape(L1_FBReading, 
                       idvar = c("state_student_number",
                                 "school_year"), 
                       timevar = "test_metric", 
                       direction = "wide",
                       sep = "")

colnames(WideL1_FBReading)

# Activitieshistorical.csv [DONE]
################################################################################

names(Activitieshistorical)[names(Activitieshistorical) == 'State.Student.Number'] = 'state_student_number'
names(Activitieshistorical)[names(Activitieshistorical) == 'School.Year'] = 'school_year'
names(Activitieshistorical)[names(Activitieshistorical) == 'Activity.Type'] = 'activity_type'
names(Activitieshistorical)[names(Activitieshistorical) == 'Activity.Name'] = 'activity_name'

unique(Activitieshistorical$activity_type) # KSHSAA Non-Athletic (1), KSHSAA Athletic (2), KCKPS Act (3), None (0)
Activitieshistorical$activity_type[Activitieshistorical$activity_type == 'KSHSAA Non-Athletic'] = 1
Activitieshistorical$activity_type[Activitieshistorical$activity_type == 'KSHSAA Athletic'] = 2
Activitieshistorical$activity_type[Activitieshistorical$activity_type == 'KCKPS Act'] = 3

countActivities = aggregate(cbind(actSum = activity_type) ~ state_student_number + school_year + activity_type, data = Activitieshistorical, FUN = length)

# reshaping activity type
countWideActivities = reshape(countActivities, 
                               idvar = c("state_student_number", 
                                         "school_year"),
                               timevar = "activity_type",
                               direction = "wide",
                              sep = "")
countWideActivities[is.na(countWideActivities)] = 0

# reshaping activity name
preL1_Activities = reshape(Activitieshistorical, 
                       idvar = c("state_student_number",
                                 "school_year"), 
                       timevar = "activity_name", 
                       direction = "wide",
                       sep = "")
colnames(preL1_Activities)

# renaming columns for less than 8 characters
columns_to_rename = grep("^activity_type", names(preL1_Activities), value = TRUE)
new_names = paste("act", seq_along(columns_to_rename), sep = "")
name_key = setNames(new_names, columns_to_rename)
names(preL1_Activities)[names(preL1_Activities) %in% columns_to_rename] = new_names

WideL1_Activities = merge(preL1_Activities, countWideActivities, by = c("state_student_number","school_year"), all=TRUE)

# MarkingPeriods1.csv [LEAVING ALONE FOR NOW]
################################################################################

discrepancies =  MarkingPeriods1$marking_period_code != MarkingPeriods1$marking_period_name # checking for potential differences between these columns
any_discrepancies = any(discrepancies) # no discrepancies (:

MarkingPeriods1$start_date = as.Date(MarkingPeriods1$start_date, format = "%m/%d/%Y")
MarkingPeriods1$end_date = as.Date(MarkingPeriods1$end_date, format = "%m/%d/%Y")

timediffMarkingPeriods1 = transform(MarkingPeriods1, 
                                    numDaysMP = as.numeric(difftime(end_date, start_date, units = 'days')))

timediffMarkingPeriods1$orderMP = paste(timediffMarkingPeriods1$marking_period_order, 
                                        timediffMarkingPeriods1$marking_period_name, sep="_")

# there is a double of school 53 for summer sch_1
timediffMarkingPeriods1$temp_key = paste(timediffMarkingPeriods1$school_year, timediffMarkingPeriods1$school_code,timediffMarkingPeriods1$orderMP)
duplicates = duplicated(timediffMarkingPeriods1$temp_key) | duplicated(timediffMarkingPeriods1$temp_key, fromLast = TRUE)
duplicate_rows = timediffMarkingPeriods1[duplicates, ]
clean_datapoint = timediffMarkingPeriods1[!(timediffMarkingPeriods1$temp_key == '2018 53 1_Summer Sch' & timediffMarkingPeriods1$numDaysMP == 11), ]

preL2_MarkingPeriods = subset(clean_datapoint, select = -c(marking_period_code, marking_period_name, marking_period_order, start_date, end_date, temp_key))

WideL2_MarkingPeriods = reshape(preL2_MarkingPeriods,
                            idvar = c("school_code",
                                      "school_year"), 
                           timevar = "orderMP",
                           direction = "wide",
                           sep = "_")

# CourseGrades.csv [LEAVING ALONE FOR NOW]
################################################################################

unique_values_list = lapply(CourseGrades, unique) 
L1_CourseGrades = CourseGrades

colnames(L1_CourseGrades)

# Staff.csv [LEAVING ALONE FOR NOW]
################################################################################

names(DID_Staff)[names(DID_Staff) == 'school_code'] = 'state_school_code'

# CurrentGPA.csv [CANNOT USE]
################################################################################

names(CurrentGPA)[names(CurrentGPA) == 'current_gpa_weighted'] = 'gpaW8'
names(CurrentGPA)[names(CurrentGPA) == 'current_gpa_unweighted'] = 'gpaUnW8'
names(CurrentGPA)[names(CurrentGPA) == 'cumulative_gpa_weighted'] = 'CgpaW8'
names(CurrentGPA)[names(CurrentGPA) == 'cumulative_gpa_unweighted'] = 'CgpaUnW8'

preL1_CurrentGPA = subset(CurrentGPA, select = -c(calculated_date))

# GPAHistorical.csv [CANNOT USE]
################################################################################

names(GPAHistorical)[names(GPAHistorical) == 'current_gpa_weighted'] = 'gpaW8'
names(GPAHistorical)[names(GPAHistorical) == 'current_gpa_unweighted'] = 'gpaUnW8'
names(GPAHistorical)[names(GPAHistorical) == 'cumulative_gpa_weighted'] = 'CgpaW8'
names(GPAHistorical)[names(GPAHistorical) == 'cumulative_gpa_unweighted'] = 'CgpaUnW8'

preL1_GPAHistorical = subset(GPAHistorical, select = -c(calculated_date))

################################################################################
########## SAVING OBJECTS TO CONSERVE MEMORY & MERGE IN A CLEAN FILE ###########
################################################################################

# see variableKey in analysis journal for more details
folder_path = "C:/Users/cgriger/Dropbox/Academia/IOWA/NASP/Replicability and Tutorial of School Readiness Profiles and Growth/Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/"
save(L2_Buildings,
     L0_DID_SA,
     L0_DID_StudentDetails,
     mainWideL1_Enrollment,
     mainWideL1_Discipline,
     WideL1_AttendanceRate,
     WideL1_FullDayAttendance,
     WideL1_PeriodAttendance,
     WideL1_KELPA,
     WideL1_FBMath_aMath,
     WideL1_FBMathEarlyMath,
     WideL1_FBReading,
     WideL1_Activities,
     file = paste0(folder_path, "../../preprocessing/step4_mergeData.RData"))
