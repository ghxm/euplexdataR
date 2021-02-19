if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, here)

tryCatch(devtools::install_github("ghxm/euplexdataR",force = T)) # install newest version of euplexdata package
library(euplexdata) # load euplexdata package
tryCatch(detach("package:euplexdata", unload=TRUE)) # unload it to be able to access the updated version within this session

# download dataset file

# insert the link to the desired dataset zip file here
dataset_zip_url <- "paste zip url here"
local_zip_path <- here::here(regmatches(dataset_zip_url, regexpr("(euplexdb_.*\\.zip$)", dataset_zip_url)))

download.file(dataset_zip_url, local_zip_path)
unzip(local_zip_path, exdir = here::here())
local_csv_path <- here::here(regmatches(dir(here::here()), regexpr("proc_.*csv$", dir(here::here()))))

# read in raw euplexdb csv
raw_df <- read.csv(here::here(local_csv_path), as.is = TRUE)

# create 'nicer' full dataset using euplexdataR package
euplexdb <- raw_df %>%
    euplexdata::euplexdata(remove_extra_vars = TRUE, rename_vars = TRUE)

# create public euplexdb dataset for public release using euplexdata package
euplexdb_public <- euplexdb %>%
    euplexdata::public_dataset(docs = c("proposal"), events = c("proposal"))

# use `%>% long()` to convert the dataset to long-format (the final release will be in long-format, but wide may be easier to work with)
euplexdb_public_long <- euplexdb_public %>%
    euplexdata::long()

# FOR STATA USERS
# this will write out a stata dataset to the specified directory (argument out) if the argument is set.
euplexdb_public %>%
    euplexdata::stata(out = here::here("euplexdb_stata.dta"))

# FOR R USERS
# you can inspect the dataset using built-in functions of the euplexdata package as follows:
euplexdb_public %>%
    euplexdata::data_summary_report()

# you can also use the observations function of the euplexdataR to display rleevant variables for a dataframe (e.g. subset df to all variables above cutoff for a complexity variable to look for outliers)

