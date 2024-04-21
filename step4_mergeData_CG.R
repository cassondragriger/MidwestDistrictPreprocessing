rm(list=ls()) # clears global environment
gc() # clear unused Mb (memory)

# checking which directory we're in - we want to access /Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/
getwd() 

# if you're not in the correct folder (KCKPS At Risk Reports), use the following and replace ... with your own path before /Scanlan Kansas Risk Prediction Research
setwd("C:/Users/cgriger/Dropbox/Academia/IOWA/NASP/Replicability and Tutorial of School Readiness Profiles and Growth/Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/")

# loading CSV files from the saved environment (step2_loadData.RData)
load(file = "../../preprocessing/step4_mergeData.RData")

################################################################################
############################# MERGING DATA #####################################
################################################################################

# data merge goes through the following progression:
# base -> btwn -> main

# base file = file with the most observations
baseData = L0_DID_StudentDetails # preserving the original L0_DID_StudentDetails for checks and references

# list of remaining L0 and L1 data frame names (from greatest to least) to merge
dataFrames = list(L0_DID_SA,
                  mainWideL1_Discipline, # school & year
                  WideL1_AttendanceRate, # school & year
                  WideL1_FullDayAttendance, # school & year
                  WideL1_PeriodAttendance, # school & year
                  WideL1_KELPA, # school & year & school_code 8000
                  WideL1_FBMath_aMath, # school & year
                  WideL1_FBMathEarlyMath, # school & year
                  WideL1_FBReading, # school & year
                  WideL1_Activities) # school & year

# merge all L1 data frames (takes about 15s to run)
for (df in dataFrames) {
  baseData = merge(baseData, df, by = c("state_student_number", "school_year"), all = TRUE)
}

# merging to preserve school data for withdrawals
names(mainWideL1_Enrollment)[names(mainWideL1_Enrollment) == 'bldgCode'] = 'school_code.x' # WideL1_Enrollment (120);
btwn_mainData = merge(baseData, mainWideL1_Enrollment, by = c("state_student_number", "school_year", "school_code.x"), all = TRUE)

# merging L2 (different "by" values, so it needed to be separated) (takes about 8s to run)
names(L2_Buildings)[names(L2_Buildings) == 'school_code'] = 'school_code.x' # L2_Buildings (120);
mainData = merge(btwn_mainData, L2_Buildings, by = "school_code.x", all = TRUE)

# check column names
colnames(mainData)

################################################################################
########################## FIXING MISSING DATA #################################
################################################################################

# new NA's introduced after merging (discipline/withdrawal) and needed to address here
NA4categories = c(
  "catAADR", 
  "catTER", 
  "catBUL", 
  "catDAD",
  "catHAT", 
  "catPVH", 
  "catTAPD", 
  "catTRU",
  "catMAF", 
  "catNWC", 
  "catTRA", 
  "catHOM",
  "catGRA", 
  "catDEA", 
  "catILL", 
  "catEXP",
  "catMAA", 
  "catDIS", 
  "catJCF", 
  "catMIUN",
  "catUNK", 
  "catERR", 
  "catGTE", 
  "catMOU",
  "catUND")
mainData[NA4categories][is.na(mainData[NA4categories])] = 0

################################################################################
########################## INCLUSION COLUMNS ###################################
################################################################################

# closed; if the school is currently closed, though doesn't mean the data is not usable!
mainData$closed # established in step3_cleanData_CG

# dupes; FBMath_aMath duplicates
mainData$dupes = ifelse(grepl("^dupe", mainData$state_student_number), 1, 0)

# anything else?

################################################################################
############## RELABELING COLUMNS that ARE UNIQUE TO MERGE #####################
################################################################################

# relabeling column names to better represent values and meet MPlus 8-character rule
names(mainData)[names(mainData) == 'school_year'] = 'YEAR'
names(mainData)[names(mainData) == 'state_student_number'] = 'stateID' # L0_DID_StudentDetails
names(mainData)[names(mainData) == 'uniqID'] = 'IDyr120' # L0_DID_StudentDetails (ID + year + 120)
names(mainData)[names(mainData) == 'unique_id'] = 'IDyr' # WideL1_Discipline (ID + year)
names(mainData)[names(mainData) == 'acct_bldg'] = 'b8000a' # L0_DID_SA (8000)
names(mainData)[names(mainData) == 'state_school_code'] = 'b8000b'  # L2_Buildings (8000)
names(mainData)[names(mainData) == 'school_code.y'] = 'b8000c'  # WideL1_KELPA (8000)
names(mainData)[names(mainData) == 'school_code.x'] = 'b120' # base, btwn, pre_mainData (120) 
# names(mainData)[names(mainData) == 'bldgCode'] = 'b120a' # WideL1_Enrollment (120); relabeled above
# names(mainData)[names(mainData) == 'school_code.x'] = 'b120b' # L0_DID_StudentDetails (120); relabeled above
names(mainData)[names(mainData) == 'school_name.x'] = 'bNAMEa' # L0_DID_StudentDetails
names(mainData)[names(mainData) == 'school_name.y'] = 'bNAMEb' # L2_Buildings
names(mainData)[names(mainData) == 'school_type'] = 'bTYPE' # L2_Buildings (letters)
names(mainData)[names(mainData) == 'grade'] = 'GRADEa' # L0_DID_StudentDetails
names(mainData)[names(mainData) == 'grade.x'] = 'GRADEb'  # L0_DID_SA
names(mainData)[names(mainData) == 'grade.y'] = 'GRADEc' # WideL1_KELPA
names(mainData)[names(mainData) == 'gender.x'] = 'GENa' # L0_DID_StudentDetails
names(mainData)[names(mainData) == 'gender.y'] = 'GENb' # L0_DID_SA
names(mainData)[names(mainData) == 'lunch.x'] = 'LUNa' # L0_DID_StudentDetails
names(mainData)[names(mainData) == 'lunch.y'] = 'LUNb' # L0_DID_SA
names(mainData)[names(mainData) == 'lep'] = 'IEP' # L0_DID_SA
# school_code L2_Buildings (120); merged with bldgCode of WideL1_Enrollment

# merge b8000b and b8000c
mainData$b8000bc = ifelse(is.na(mainData$b8000b), mainData$b8000c, mainData$b8000b)

# fill in 0's for disciplinary infractions; when left merged to mainData, the Discipline.csv had ~19k rows while the mainData has ~193k rows
mainData$totDisc[is.na(mainData$totDisc)] = 0 # should be moved to step3 at some point

# setting new order to variables above so that we can see discrepancies more easily
col_ordering = c(
  'stateID',
  'localID',
  'disEntry',
  'lstEntry',
  'diffDay',
  'diffYear',
  'YEAR',
  'IDyr120',
  'IDyr',
  'dupes',
  'closed',
  'b120',
  'b8000a',
  'b8000b',
  'b8000c',
  'b8000bc',
  # 'b120a',
  # 'b120b',
  'bNAMEa',
  'bNAMEb',
  'bTYPE',
  'GRADEa',
  'GRADEb',
  'GRADEc',
  'DOB',
  'GENa',
  'GENb',
  'LUNa',
  'LUNb',
  'lunchPRI',
  'lowINC',
  'homeless',
  'race',
  'compRace',
  'hispanic',
  'ethnic',
  'ell',
  'migrant',
  'disabl',
  'spec_ed',
  'IEP',
  'xcept1',
  'xcept2',
  'status'
)

# setting the order to the main data
new_order = c(col_ordering, setdiff(names(mainData), col_ordering))
mainData = mainData[new_order]

# removing residual columns not needed for near and distant, potential analyses
col_remove = c("IDyr120",
               "IDyr",
               "b8000a",
               "b8000b",
               "b8000c",
               "GRADEa",
               "GRADEc",
               "GENb",
               "LUNb",
               "compRace")
mainData = mainData[ , !names(mainData) %in% col_remove]

# sort data by student and year - preparing to fill in constant data
mainData = mainData[order(mainData$stateID, mainData$YEAR), ]

# sanity checking IDs and years for duplicates and specific cases
specific_ID = '1003911064'
rows_for_specific_ID = mainData[mainData$stateID == specific_ID, ]

# checking for duplicates stateID and YEAR
duplicates = mainData[duplicated(mainData[c("stateID", "YEAR")]) | duplicated(mainData[c("stateID", "YEAR")], fromLast = TRUE), ]

# get unique rows that have duplicates
unique_n_year = unique(duplicates[c("stateID", "YEAR")]) # n = 16242
unique_n = unique(duplicates[c("stateID")]) # n = 12304

################################################################################
################ SAVING MAIN DATA FOR STEP6_descriptives #######################
################################################################################

colnames(mainData)
unique(mainData$GRADEb)
unique(mainData$totDisc)

save(mainData, file = "../../main data/mainData.RData")
write.csv(mainData, file = "../../main data/mainData.csv", row.names=FALSE)

# skip merge above and load data
load(file = "../../main data/mainData.RData")
