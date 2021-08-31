library(plyr)
library(tidyr)
library(dplyr)
library(stringr)

# Parameters
curr_month <- "Aug21"
prev_month <- "Jul21"
WorkingArea <- "U:\\Productn\\I_A\\2020 Single Source modules fusion\\Data Files\\"

# Set Working Area
setwd(str_c(WorkingArea, curr_month))


#Functions

# Function to create frequency for variable
freqy <- function(df, coli){
  y = plyr::count(df, coli)
  
  dff <- tidyr::gather(y, 'Var', 'Group', 1)
  
  return(dff)
}

# Function to check Files rows and columns for blanks
RowColsSumCheck <- function(df_chek){
  for (j in 1:nrow(df_chek)){
    vec <- as.numeric(df_chek[j,])
    if (sum(vec, na.rm=T)-vec[1]==0){
      print(paste0("Warning, Zero Sum Respondnet: ", vec[1]))
    }
  }
  
  
  colx <- colnames(df_chek)
  for (k in 2:ncol(df_chek)){
    vec <- as.numeric(df_chek[,k])
    if (sum(vec, na.rm=T)==0){
      print(paste0("Warning, Zero Sum Column: ", colx[k]))
    }
  }
}

# Function to create frequeny table
createFreyTable <-  function(df){
  colss <- colnames(df)
  colss <- colss[-1]
  # Declare empty datafram
  maindf <- data.frame(freq=integer(),
                       Var=character(),
                       Group=integer())
  
  # Build frequency table with all varaibles
  i = 1
  while (i <=length(colss)){
    maindf <- dplyr::bind_rows(maindf, freqy(df, colss[i]))
    i = i + 1
  }
  
  # Turn frequeny to percentages
  maindf['freq'] = maindf['freq']/nrow(df)
  
  return(maindf)
}

# Function to compare frequney tables
CompareFreyTable <- function(prev_month, curr_month, vers){
  # Read this months file
  thismonth <- read.csv(str_c(curr_month , ' ', vers, '.csv'))
  # Read last months file
  lastmonth <- read.csv(str_c(WorkingArea, prev_month, "\\", prev_month , " ", vers, ".csv"))
  
  lastmonth_frey <- createFreyTable(lastmonth)
  thismonth_frey <- createFreyTable(thismonth)
  
  # Join with last months frequency table
  compTab <- full_join(lastmonth_frey, thismonth_frey, by = c("Var"= "Var", "Group" = "Group"))
  # Calculate absolt difference between this months and last months proportions
  compTab["Diff"] <- abs(compTab['freq.x'] - compTab['freq.y'])
  
  # Check this months file for blanks
  RowColsSumCheck(thismonth)
  # Save frequency table
  write.csv(compTab, str_c(prev_month, curr_month, "_" , vers, "_FreqTable.csv"))
}
