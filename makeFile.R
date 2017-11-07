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
zip <- "02903"
source("01_get_dunkin.R")   # Scrape data
source("02_get_list.R")     # create useable data list
source("03_clean_dunkin.R")     ## clean data
source("04_graph.R")  ## create graph
rmarkdown::render("paper.Rmd", output_format = "html_document")
