## clean all output from previous runs of scripts
## there is one final we create in the main directory (the processed dataset),
## but the rest of the output is all in the 'results' directory
unlink("json", recursive = TRUE) # where data json downloads
unlink("data", recursive = TRUE) # where data is stored after clean
unlink("graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("json"), showWarnings = FALSE)
dir.create(file.path("data"), showWarnings = FALSE)
dir.create(file.path("graph"), showWarnings = FALSE)


## run all scripts
## Scraped and cleaned data wrangling in the Data Wrangling File
## In confidentiality of personal credentials that file is not included on the public repository,
## However, the finished csv file is attached.

source("YamSentimentAnalysis.R")     # General Sentiment Analysis of the different companies.



##Knit the combination of all of the script files.
rmarkdown::render("paper.Rmd", output_format = "html_document")
