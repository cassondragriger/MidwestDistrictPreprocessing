rm(list=ls()) # clears global environment
gc() # clear unused Mb (memory)

# checking which directory we're in - we want to access /Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/
getwd() 

# if you're not in the correct folder (KCKPS At Risk Reports), use the following and replace ... with your own path before /Scanlan Kansas Risk Prediction Research
setwd("C:/Users/cgriger/Dropbox/Academia/IOWA/NASP/Replicability and Tutorial of School Readiness Profiles and Growth/Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/")
# setwd("C:/Users/Cassondra/Dropbox/Academia/IOWA/NASP/Replicability and Tutorial of School Readiness Profiles and Growth/Scanlan Kansas Risk Prediction Research/initial data deidentified/KCKPS At Risk Reports/")

# loading CSV files from the saved environment
load(file = "../../main data/mainData.RData")

################################################################################
# quick descriptives  of mainData for number in levels of schools and students

# finding unique counts of students and schools per group of 'closed'
num_closed = aggregate(cbind(stateID, b8000bc) ~ closed, data = mainData, function(x) length(unique(x)))
names(num_closed)[2:3] <- c("UniqueStudents", "UniqueSchools")

# finding unique counts of students and schools per group of 'dupes'
num_dupes = aggregate(stateID ~ dupes, data = mainData, function(x) length(unique(x)))
names(num_dupes)[2] <- "UniqueStudents"

########################################################################################################################################################
# checking assessments

# checking students who have NAs for KAP
typeKAP_NAs = mainData[is.na(mainData$typeKAP), ]

# checking years for 
subsetYEARxKAP = subset(mainData, select = c(stateID, YEAR, GRADEb, typeKAP, scaleKAP))
subset20162017 = subset(subsetYEARxKAP, YEAR == 2016 | YEAR == 2017)

# checking over assessments and what we have to work with
subsetAssessment = subset(mainData, select = c(
  stateID, 
  YEAR,
  GRADEb,
  typeKAP, 
  scaleKAP, 
  KELP_LPL,
  KELP_RPL, 
  KELP_SPL, 
  KELP_WPL, 
  KELP_PD,
  FBmThet,
  FBmpSCH,
  FBmpLEA,
  FBmpNAT,
  FBmrLVL,
  FBmFin,
  FBempNAT,
  FBemrLVL,
  FBrpNAT,
  FBrrLVL
))

################################################################################
# KAP data

# subsetting for KAP scores
summary(mainData$scaleKAP)
subsetKAP = mainData[!is.na(mainData$scaleKAP), ] # 27% of the mainData
subset2023 = subset(mainData, YEAR == 2023) # checking what data is available for 2023

# checks for inclusion criteria
summary(subsetKAP$closed)
summary(subsetKAP$dupes)
subsetInclusion = subset(subsetKAP, dupes == 0 & closed == 0) # no dupes remaining after subsetting KAP
nrow(subsetKAP)-nrow(subsetInclusion) # 3034 cases removed

################################################################################
# subsetting full data frame for analyses

# ctrl + shift + c to comment and uncomment columns as needed
presubsetData = subset(subsetKAP, select = c(
  "stateID", 
  # "localID", 
  # "disEntry", 
  # "lstEntry",
  # "diffDay", 
  # "diffYear", 
  "YEAR", 
  # "dupes",
  # "closed", 
  # "b120", 
  "b8000bc", 
  "bNAMEa",
  # "bNAMEb", 
  "bTYPE", 
  "GRADEb", 
  # "DOB",
  "GENa", 
  "LUNa", 
  # "lunchPRI", 
  "lowINC",
  "homeless", 
  "race", 
  "hispanic", 
  "ethnic",
  "ell", 
  "esol", # same as ell
  "migrant", 
  "disabl", 
  "spec_ed",
  "IEP", 
  "xcept1", 
  "xcept2", 
  "status",
  "typeKAP", 
  # "numqKAP", 
  # "respKAP", 
  "scaleKAP",
  # "catKAP", 
  # "ADCF", 
  # "APAU", 
  # "ASS",
  # "AUL", 
  # "AFR", 
  # "BCU", 
  # "BPU",
  # "BRU", 
  # "BVU", 
  # "BCUC", 
  # "BPD",
  # "CTOI", 
  # "CBC", 
  # "DEF", 
  # "DOAG",
  # "DDE", 
  # "DOIO", 
  # "DUOP", 
  # "DCR",
  # "DPS", 
  # "DPDU", 
  # "ETX", 
  # "EXT",
  # "FRBO", 
  # "FIG", 
  # "FWI", 
  # "GAM",
  # "GRA", 
  # "GISP", 
  # "GDB", 
  # "GDAO",
  # "HAR", 
  # "HRC", 
  # "IDP", 
  # "IDSS",
  # "IDU", 
  # "IDOA", 
  # "ICIU", 
  # "ITF",
  # "IEN", 
  # "ITOS", 
  # "KID", 
  # "LSWP",
  # "MPA", 
  # "MSS", 
  # "MUA", 
  # "MOTB",
  # "MUOT", 
  # "MHU", 
  # "NCTD", 
  # "NVSO",
  # "OBUO", 
  # "PATT", 
  # "PAB", 
  # "POEN",
  # "POFW", 
  # "POID", 
  # "POUO", 
  # "PWIT",
  # "ROB", 
  # "SOFO", 
  # "SAE", 
  # "SHE",
  # "SME", 
  # "SCK", 
  # "SSOA", 
  # "STA",
  # "STT", 
  # "TPO", 
  # "TSS", 
  # "TUO",
  # "TRE", 
  # "TRU", 
  # "USOO", 
  # "USOD",
  # "UDOT", 
  # "UCN", 
  # "UOAW", 
  # "UOFO",
  # "VPA", 
  # "VSP", 
  # "VHR", 
  # "VLR",
  # "VPR", 
  # "VSAR", 
  # "VOGS", 
  "totDisc",
  "catAADR", 
  "catTER", 
  "catBUL", 
  "catDAD",
  "catHAT", 
  "catPVH", 
  "catTAPD", 
  "catTRU",
  "catMAF", 
  "scatAADR", 
  "scatTER", 
  "scatBUL",
  "scatDAD", 
  "scatHAT", 
  "scatPVH", 
  "scatTAPD",
  "scatTRU", 
  "scatMAF", 
  "daysTot", 
  "daysAbs",
  "daysPres", 
  # "absUNKWN", 
  # "absASP", 
  # "absE20",
  # "absEXC", 
  # "absEXT", 
  # "absHS", 
  # "absISS",
  # "absLT", 
  # "absPEN", 
  # "absSRT", 
  # "absUNV",
  # "absUNX", 
  # "perUNKWN", 
  # "perE20", 
  # "perETD",
  # "perEXC", 
  # "perEXT", 
  # "perFHT", 
  # "perHS",
  # "perLT", 
  # "perPEN", 
  # "perSRT", 
  # "perTDY",
  # "perUNV", 
  # "perUNX", 
  "KELP_LPL",
  "KELP_RPL", 
  "KELP_SPL", 
  "KELP_WPL", 
  "KELP_PD",
  # "FBmThet", 
  # "FBmpSCH", 
  # "FBmpLEA", 
  # "FBmpNAT",
  # "FBmrLVL", 
  # "FBmFin", 
  # "FBempNAT", 
  # "FBemrLVL",
  # "FBrpNAT", 
  # "FBrrLVL", 
  # "act1", 
  # "act2",
  # "act3", 
  # "act4", 
  # "act5", 
  # "act6",
  # "act7", 
  # "act8", 
  # "act9", 
  # "act10",
  # "act11", 
  # "act12", 
  # "act13", 
  # "act14",
  # "act15", 
  # "act16", 
  # "act17", 
  # "act18",
  # "act19", 
  # "act20", 
  # "act21", 
  # "act22",
  # "act23", 
  "actSum1", 
  "actSum2", 
  "actSum3",
  # "WD0", 
  # "WD1", 
  # "WD2", 
  # "WD3",
  # "WD4", 
  # "WD4a", 
  # "WD5", 
  # "WD5a",
  # "WD6", 
  # "WD6a", 
  # "WD7", 
  # "WD8",
  # "WD9", 
  # "WD10", 
  # "WD11", 
  # "WD12",
  # "WD13", 
  # "WD14", 
  # "WD15", 
  # "WD16",
  # "WD17", 
  # "WD18", 
  # "WD19", 
  # "WD20",
  # "WD21", 
  # "WD22", 
  # "WD23", 
  # "WD98",
  # "WD99", 
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
  "catUND", 
  "scatNWC", 
  "scatTRA", 
  "scatHOM",
  "scatGRA", 
  "scatDEA", 
  "scatILL", 
  "scatEXP",
  "scatMAA", 
  "scatDIS", 
  "scatJCF", 
  "scatMIUN",
  "scatUNK", 
  "scatERR", 
  "scatGTE", 
  "scatMOU",
  "scatUND"
))

# grade checks
unique(presubsetData$GRADEb)
GRADEb_NAs = presubsetData[is.na(presubsetData$GRADEb), ] # n = 183; must look into these and see if they're fixable
subsetData = presubsetData[!is.na(presubsetData$GRADEb), ] 
unique(subsetData$GRADEb)

# typeKAP check
unique(subsetData$typeKAP)
typeKAP_NotTested = subset(subsetData, typeKAP == "Not Tested") # n = 1202
typeKAP_DLM = subset(subsetData, typeKAP == "DLM") # n = 1090
typeKAP_General = subset(subsetData, typeKAP == "General") # n = 50850

########################### subsetData #########################################

# saving subset for KAP scores 
save(subsetData, file = "../../main data/subsetData.RData")
write.csv(subsetData, file = "../../main data/subsetData.csv", row.names=FALSE)

################################################################################

# Variables for MPlus:
    # YEAR = School Year
    # b8000bc = School ID
    # KAP = Kansas Assessment Program
    # KELPA = Kansas English Language Proficiency Assessment
    # catAADR = Adult and Drug Related Offenses
    # catTER = Terrorism
    # catBUL = Bullying
    # catDAD = Disruption and Defiance
    # catHAT = Harassment and Threats
    # catPVH = Physical Violence
    # catTAPD = Theft and Property Damage
    # catTRU = Truancy
    # catMAF = Misdemeanors and Felonies

# loading CSV files from the saved environment
load(file = "../../main data/subsetData.RData")

# keep columns for MPlus
DisciplineMPlusVar = subset(subsetData, select = c(
  stateID,        # Student ID
  YEAR,           # School Year
  GRADEb,         # Grade
  b8000bc,        # School ID
  # typeKAP,        # 3 types: General, DLM, Not Tested
  scaleKAP,       # Kansas Assessment Program
  KELP_LPL,       # Listening Performance Level, Kansas English Language Proficiency Assessment
  KELP_RPL,       # Reading Performance Level, Kansas English Language Proficiency Assessment
  KELP_SPL,       # Speaking Performance Level, Kansas English Language Proficiency Assessment
  KELP_WPL,       # Writing Performance Level, Kansas English Language Proficiency Assessment
  KELP_PD,        # Performance Determination, Kansas English Language Proficiency Assessment
  catAADR,        # Alcohol and Drug Related Offenses
  catTER,         # Terrorism
  catBUL,         # Bullying
  catDAD,         # Disruption and Defiance
  catHAT,         # Harassment and Threats
  catPVH,         # Physical Violence
  catTAPD,        # Theft and Property Damage
  catTRU,         # Truancy
  catMAF          # Misdemeanors and Felonies
  # bTYPE,        # H = High school, M = Middle, E = Elementary
  # GENa,         # Gender: M, F
  # LUNa,         # Lunch Type: F = Free, R = Reduced, S = Self-Pay
  # lowINC,       # TRUE/FALSE
  # homeless,     # 0/1
  # race,         # Categorical
  # hispanic,     # 0/1
  # ethnic,       # 0/1
  # ell,          # 0 - 8
  # migrant,      # 0/1
  # disabl,       # 0/1
  # spec_ed,      # TRUE/FALSE
  # IEP,          # TRUE/FALSE
))

# subset for year and high school grades
DisciplineMPlusVarHS2022 = subset(DisciplineMPlusVar, YEAR == 2022 & (GRADEb == 9 | GRADEb == 10 | GRADEb == 11 | GRADEb == 12))
unique(DisciplineMPlusVarHS2022$GRADEb)

# removing column names for easy MPlus use
names(DisciplineMPlusVarHS2022) = NULL

# checking there are really only 2 seniors in 2022 cohort with KAP scores
seniorsKAP = subset(subsetData, GRADEb == 12)
seniors2022 = subset(mainData, YEAR == 2022 & GRADEb == 12)

# counting how many 1's there are in the data by discipline categories
cols_to_aggregate = c("b8000bc", grep("^cat", names(DisciplineMPlusVarHS2022), value = TRUE))
selected_data = DisciplineMPlusVarHS2022[cols_to_aggregate]
all_counts = aggregate(. ~ b8000bc, data = selected_data, FUN = function(x) sum(x == 1, na.rm = TRUE))

# CHECKING FOR CONSTANT VARIABLES ##############################################

# BY SCHOOL

# selecting category columns and  creating long format for category columns
category_cols = grep("^cat", names(DisciplineMPlusVarHS2022), value = TRUE)
categories_long_format = stack(DisciplineMPlusVarHS2022[category_cols])

# adding school and grade identifiers to the long format data
repeat_count = length(category_cols)
categories_long_format$b8000bc = rep(DisciplineMPlusVarHS2022$b8000bc, each = repeat_count)
categories_long_format$GRADEb = rep(DisciplineMPlusVarHS2022$GRADEb, each = repeat_count)

# checking if all category values for each school and grade are 1
school_grade_all_ones = aggregate(values ~ b8000bc + GRADEb, data = categories_long_format, FUN = function(x) all(x == 1, na.rm = TRUE))
names(school_grade_all_ones)[3] = "all_categories_one"

# BY SCHOOL AND GRADE AND CATEGORY FOR 1's

# selecting category columns along with school and GRADEb identifiers
category_cols = grep("^cat", names(DisciplineMPlusVarHS2022), value = TRUE) # repeat, but here for replicability needs
cols_to_aggregate = c("b8000bc", "GRADEb", category_cols)

# creating a subset of the data with only the required columns
cat_cols_DisciplineMPlusVarHS2022 = DisciplineMPlusVarHS2022[cols_to_aggregate]

# aggregating by school and GRADEb for each category
school_grade_cat_all_ones = aggregate(. ~ b8000bc + GRADEb, data = cat_cols_DisciplineMPlusVarHS2022, FUN = function(x) all(x == 1, na.rm = TRUE))

# BY SCHOOL AND GRADE AND CATEGORY FOR 0's

# selecting category columns along with school and GRADEb identifiers
category_cols = grep("^cat", names(DisciplineMPlusVarHS2022), value = TRUE) # repeat, but here for replicability needs
cols_to_aggregate = c("b8000bc", "GRADEb", category_cols)

# creating a subset of the data with only the required columns
cat_cols_DisciplineMPlusVarHS2022 = DisciplineMPlusVarHS2022[cols_to_aggregate]

# aggregating by school and GRADEb for each category
school_grade_cat_all_zeros = aggregate(. ~ b8000bc + GRADEb, data = cat_cols_DisciplineMPlusVarHS2022, FUN = function(x) all(x == 0, na.rm = TRUE))

########################## DisciplineHS2022 ####################################

# saving subset for 2022 and high school grades
save(DisciplineMPlusVarHS2022, file = "../../main data/DisciplineHS2022.RData")
write.csv(DisciplineMPlusVarHS2022, file = "../../main data/DisciplineHS2022.csv", row.names=FALSE)

