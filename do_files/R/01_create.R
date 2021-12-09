# Top commands ----
# Create empty R application (no figures, data frames, packages, etc.)
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# load library

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_etal_2019/")
data_files = "data_files/"
support_files = "support_files/"

# Download data files ----

# https://dataverse.lib.umanitoba.ca/dataverse/Mincome_Data

# Mincome Baseline Data (Minc1)
# https://doi.org/10.5203/FK2/NV200L

download.file('https://dataverse.lib.umanitoba.ca/api/access/datafile/1001', 
              destfile = paste0(data_files,"minc1.xlsx"), 
              method = "wget", 
              extra = "-r -p --random-wait")

# Mincome Payments Data (Minc2)
# https://doi.org/10.5203/FK2/MWK24G

download.file('https://dataverse.lib.umanitoba.ca/api/access/datafile/1003', 
              destfile = paste0(data_files,"minc2.xlsx"), 
              method = "wget", 
              extra = "-r -p --random-wait")

# Mincome Longitudinal Labour Market Data (Minc4)
# https://doi.org/10.5203/FK2/PO1F6R

download.file('https://dataverse.lib.umanitoba.ca/api/access/datafile/1007', 
              destfile = paste0(data_files,"minc4.xlsx"), 
              method = "wget", 
              extra = "-r -p --random-wait")

# Download codebooks ----

# Mincome Baseline Data (Minc1)
# https://doi.org/10.5203/FK2/NV200L

download.file('https://dataverse.lib.umanitoba.ca/api/access/datafile/970', 
              destfile = paste0(support_files,"minc1-Baseline-Codebook-1.pdf"), 
              method = "wget",
              extra = "-r -p --random-wait")

# Mincome Payments Data (Minc2)
# https://doi.org/10.5203/FK2/MWK24G

download.file('https://dataverse.lib.umanitoba.ca/api/access/datafile/1013', 
              destfile = paste0(support_files,"minc2-Payments-Summary-File-Codebook-2.pdf"), 
              method = "wget", 
              extra = "-r -p --random-wait")


# Mincome Longitudinal Labour Market Data (Minc4)
# https://doi.org/10.5203/FK2/6C5JSW

download.file('https://dataverse.lib.umanitoba.ca/api/access/datafile/1017', 
              destfile = paste0(support_files,"minc4-Longitudinal-Labour File-Codebook.pdf"), 
              method = "wget", 
              extra = "-r -p --random-wait")


