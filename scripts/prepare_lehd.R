#===============================================================================#
# PREPARE LODES DATA
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor")
lapply(libs, library, character.only = TRUE)

# read in LODES
il_lodes <- read_csv("data/il_od_auz_JT00_2017.csv")

# change column names
lodes <- il_lodes %>% 
  dplyr::rename(
    "total_jobs" = "S000",
    "SA01",
    "SA02",
    "SA03",
    ""
  )


6 Rev. 20190822OD Filename of the OD datasets are described by the following templates [ST]_od_[PART]_[TYPE]_[YEAR].csv.gz   where [ST] = lowercase, 2-letter postal code for a chosen state [PART] = Part of the state file, can have a value of either “main” or “aux”. Complimentary parts of the state file, the main part includes jobs with both workplace and residence in the state and the aux part includes jobs with the workplace in the state and the residence outside of the state. [TYPE] = Job Type, can have a value of “JT00” for All Jobs, “JT01” for Primary Jobs, “JT02” for All Private Jobs, “JT03” for Private Primary Jobs, “JT04” for All Federal Jobs, or “JT05” for Federal Primary Jobs. [YEAR] = Year of job data. Can have the value of 2002-2015 for most states. As an example the main OD file of Primary Jobs in 2007 for California would be the file: ca_od_main_JT01_2007.csv.gz The structure of the OD files is as follows: Origin-Destination (OD) File Structure Pos   Variable   Type Explanation 1   w_geocode   Char15  Workplace Census Block Code 2   h_geocode   Char15  Residence Census Block Code 3   S000 Num   Total number of jobs 4   SA01 Num   Number of jobs of workers age 29 or younger165   SA02 Num   Number of jobs for workers age 30 to 54166   SA03 Num   Number of jobs for workers age 55 or older167   SE01 Num   Number of jobs with earnings $1250/month or less 8   SE02 Num   Number of jobs with earnings $1251/month to $3333/month 9   SE03 Num   Number of jobs with earnings greater than $3333/month 10   SI01 Num   Number of jobs in Goods Producing industry sectors 11   SI02 Num   Number of jobs in Trade, Transportation, and Utilities industry sectors 12   SI03 Num   Number of jobs in All Other Services industry sectors 13   createdate    Char   Date on which data was created, formatted as YYYYMMDD 