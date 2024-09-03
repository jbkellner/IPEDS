################################################################################

# IPEDS_download_transform.R
# Julie Kellner
# Last modified August 27, 2024

# This R code will download the past 5 years of HD, IC and C_A datafiles from IPEDS
# It will also download the frequency and varlist info from the dictionary for those years
# The code uses the most recent version of the data available,
# Which could be the provisional version if the revised/final version has not been released yet.
# More details of the version releases are here: 
# https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=7184165d-b0de-4b97-8490-f612cc0007f4&rtid=7

# The code then appends to the datafiles the plain language valuelabel
# And combines all years together with a column for the year and original datafile

# Output saved includes original downloaded files
# And 5 new files for all 5 years, hd, ic, c_a, Frequencies, varlist

# This code was tested and works on datafiles from 2019-2023

# Original Data Source:
# Institutional Characteristics All Years
# https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&surveyNumber=1
# Completions All Years
# https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&surveyNumber=3

# HD - Institutional characteristics directory info
# IC - Institutional characteristics Educational offerings, organization, services and athletic associations
# C_A - Awards/degrees conferred by program (6-digit CIP code), award level, race/ethnicity, and gender: July 1, prior year to June 30, current year

################################################################################

#Load libraries
library(httr)
library(utils)
library(readr)
library(readxl)
library(tidyverse)
library(RCurl)

# Make directory to store downloaded files
main_dir <- "~/Downloads"
sub_dir <- paste0("IPEDS_download_", format(Sys.time(),format="%Y-%m-%d"))
dir.create(file.path(main_dir, sub_dir))
setwd(file.path(main_dir, sub_dir))
rm(main_dir,sub_dir)

# Get current and past years of IPEDS datafiles and dictionaries for the past 5 years
# Unzip the datafiles and delete all the zips

number_of_pastyears = 5 # year 0 is current year and this data won't be available in IPEDS yet
current_year <- as.numeric(format(Sys.time(),format="%Y"))
files = c("HD","IC")

for (i in 1:number_of_pastyears) {
  for (ii in 1:length(files)) {

    filename <- paste0(files[ii], current_year - i, ".zip")
    url <- paste0("https://nces.ed.gov/ipeds/datacenter/data/", filename)
    if (url.exists(url) == TRUE) {
      GET(url, write_disk(filename, overwrite=TRUE))
      unzip(filename)
      unlink(filename)
    }

    filename <- paste0(files[ii], current_year - i, "_Dict.zip")
    url <- paste0("https://nces.ed.gov/ipeds/datacenter/data/", filename)
    if (url.exists(url) == TRUE) {
      GET(url, write_disk(filename, overwrite=TRUE))
      unzip(filename)
      unlink(filename)
    }
  }
  
  filename <- paste0("C", current_year - i, "_A.zip")
  url <- paste0("https://nces.ed.gov/ipeds/datacenter/data/", filename)
  if (url.exists(url) == TRUE) {
    GET(url, write_disk(filename, overwrite=TRUE))
    unzip(filename)
    unlink(filename)
  }
  
  filename <- paste0("C", current_year - i, "_A_Dict.zip")
  url <- paste0("https://nces.ed.gov/ipeds/datacenter/data/", filename)
  if (url.exists(url) == TRUE) {
    GET(url, write_disk(filename, overwrite=TRUE))
    unzip(filename)
    unlink(filename)
  }
}
rm(i,ii,filename,url,files)

# note that some files have been revised and have an "_rv" at the end, 
# keep the _rv and get rid of the older versions
# find all files with _rv in the directory
# remove the _rv from the name
# delete that non _rv file
# rename the _rv file to not have the _rv

files_rv <- list.files(pattern="rv")
for (i in 1:length(files_rv)) {
  unlink(gsub("_rv","",files_rv[i]))
  file.rename(files_rv[i],gsub("_rv","",files_rv[i]))
}
rm(i, files_rv)

# read all csv files
files_csv <- list.files(pattern="csv") 
for (i in 1:length(files_csv)) {
  assign(noquote(gsub(".csv","",files_csv[i])),read_csv(files_csv[i]))
}
files_csv <- gsub(".csv","",files_csv)
rm(i)

# read all excel files Frequencies and varlist
# if FrequenciesRV tab exists, overwrite Frequencies
files_xlsx <- list.files(pattern="xlsx")
for (i in 1:length(files_xlsx)) {
  assign(noquote(gsub(".xlsx","_Frequencies",files_xlsx[i])),read_excel(files_xlsx[i], sheet = "Frequencies"))
  assign(noquote(gsub(".xlsx","_varlist",files_xlsx[i])),read_excel(files_xlsx[i], sheet = "varlist"))
  
  result <- tryCatch({
  assign(noquote(gsub(".xlsx","_Frequencies",files_xlsx[i])),read_excel(files_xlsx[i], sheet = "FrequenciesRV"))
  }, warning = function(war) {
    # Is executed if warning encountered
  }, error = function(err) {
    # Is executed if error encountered
  })
  
}
rm(i,files_xlsx,result)

# add plain language stored in the valuelabel in the Frequencies
# by merging to datafiles by matching the codevalue 

for (i in 1:length(files_csv)) {
  file = files_csv[i]
  #print(file)
  Frequencieslist = paste0(file,"_Frequencies")
  
  temp <- eval(as.name(Frequencieslist))
  temp_unique <- unique(temp$varname)
  temp_split <- split(temp[c("codevalue","valuelabel")], temp$varname)
  list2env(temp_split, envir = .GlobalEnv)
  rm(temp,temp_split)
  
  x = eval(as.name(paste(file)))
  
  for (ii in 1:length(temp_unique)) {
    varname <- temp_unique[ii]
    #print(varname)
    rename <- paste0({varname},'.valuelabel')
    
    y <- eval(as.name(paste(varname)))
    
    # correct awardlevel in frequencies file because
    # it is single digit and should be two digit
    if (varname == "AWLEVEL") {
      y$codevalue <- sprintf("%02d", as.numeric(y$codevalue))
    }

    xx <- as.matrix(unique(x[varname]))
    yy <- as.matrix(y$codevalue)
    
    # check that a match is not missing
    if (is_empty(setdiff(xx,yy))) {
      #
    } else {
      print(setdiff(xx,yy)) # these are not ok to be missing
      #print(setdiff(y$codevalue,x$AWLEVEL)) # these are ok to be missing
    }
    
    # add common name
    x <- merge(x,y,by.x=varname,by.y='codevalue',all.x=TRUE)
    x <- rename(x, !!rename := valuelabel)
    
    rm(list=varname,varname,rename,y,xx,yy)
  }
 
  # overwrite file with new merged version
  #assign(  paste0(file, "_merged"), x )
  assign(  paste0(file, ""), x )
  rm(x)
  
}

rm(i,ii,temp_unique,file,Frequencieslist)
rm(files_csv)

# append year and file source to columns
# combine varlist and Frequencies for all years
list_df <- mget(ls(pattern = "varlist"))
varlist <- purrr::list_rbind(list_df, names_to = "datafile")
varlist$year <- as.numeric(gsub("\\D", "", varlist$datafile))
varlist$datafile <- gsub("_varlist", "", varlist$datafile)
rm(list_df,list=ls(pattern="_varlist"))

list_df <- mget(ls(pattern = "Frequencies"))
Frequencies <- purrr::list_rbind(list_df, names_to = "datafile")
Frequencies$year <- as.numeric(gsub("\\D", "", Frequencies$datafile))
Frequencies$datafile <- gsub("_Frequencies", "", Frequencies$datafile)
rm(list_df,list=ls(pattern="_Frequencies"))

# append year and file source to columns to all datafiles
list_df <- mget(ls(pattern = "20"))
list_df_names <- names(list_df)
list_df_years <- as.numeric(gsub("\\D", "", list_df_names))

list_df <- Map(cbind, list_df, datafile=list_df_names)
list_df <- Map(cbind, list_df, year=list_df_years)

list2env(list_df ,.GlobalEnv)
rm(list_df,list_df_names,list_df_years)

# combine all hd, ic, and c_a files for all years
# note that some columns may differ year to year
# so the number of columns in the combined files may be greater

list_df <- mget(ls(pattern = "hd"))
hd <- bind_rows(list_df)
rm(list_df,list=ls(pattern="hd2"))

list_df <- mget(ls(pattern = "ic"))
ic <- bind_rows(list_df)
rm(list_df,list=ls(pattern="ic2"))

list_df <- mget(ls(pattern = "_a"))
c_a <- bind_rows(list_df)
rm(list_df,list=ls(pattern="c2"))

rm(current_year,number_of_pastyears)

# everything above produces hd, ic, c_a files for all 5 years
# plus their dictionary frequencies and varnames

# code to reorder columns alphabetically if preferred
# so the code and codevalue are next to each other
# c_a <- c_a[,order(colnames(c_a))]
# hd <- hd[,order(colnames(hd))]
# ic <- ic[,order(colnames(ic))]

# export each dataframe as csv
main_dir <- "~/Downloads"
sub_dir <- paste0("IPEDS_export_transform", format(Sys.time(),format="%Y-%m-%d"))
dir.create(file.path(main_dir, sub_dir))
setwd(file.path(main_dir, sub_dir))
rm(main_dir,sub_dir)

list_df <- mget(ls(pattern = ""))

list_df %>% 
  names(.) %>% 
  map(~ write_csv(list_df[[.]], paste0( ., ".csv")))

rm(list_df)









