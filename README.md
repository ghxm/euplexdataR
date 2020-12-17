# euplexdataR
R package for working with raw euplexdb data

## Installation

To install the package, run the following code in the R console:

`````
if(!require(devtools)){
    install.packages("devtools")
}
devtools::install_github("ghxm/euplexdataR")

`````

## Usage

The package functions require a data frame object of the euplexdb dataset to be passed as the first argument. Example:

`````
df <- read.csv("your euplexdb csv file", as.is = TRUE)
euplexdb <- euplexdata::euplexdata(df)

`````
