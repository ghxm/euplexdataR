# euplexdataR
R package for working with raw euplexdb data

Currently, the package can perform the following data processing tasks:

- format date variables to the `Date` format
- create procedure type dummy variables
- format logical variables from char to logical
- create event and doc variables for `_proposal_` and `_final_`


## Installation

To install the package, run the following code in the R console:

`````
if(!require(devtools)){
    install.packages("devtools")
}
devtools::install_github("ghxm/euplexdataR")

`````

Under Mac and Linux, please additionally install `protobuf`

## Usage

The package functions require a data frame object of the euplexdb dataset to be passed as the first argument. Example:

`````
df <- read.csv("your euplexdb csv file", as.is = TRUE)
euplexdb <- euplexdata::euplexdata(df)

`````
