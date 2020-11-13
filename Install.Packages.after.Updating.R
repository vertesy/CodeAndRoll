install.packages("tidyverse")
install.packages("Seurat")
install.packages("clipr")
install.packages("doMC")
install.packages("tictoc")
install.packages("pheatmap")
install.packages("biomaRt")
install.packages("devtools")
# If you don't have it
install.packages("openxlsx")
install.packages("plotrix")
install.packages("princurve")
install.packages("HGNChelper")
install.packages("R.utils")
install.packages("SoupX")
install.packages("BiocManager")

update.packages(ask = F)

require("devtools")
devtools::install_github(repo = "vertesy/MarkdownReportsDev")
source('/GitHub/Packages/CodeAndRoll/CodeAndRoll.R')

BiocManager::install("schex")
BiocManager::install("biomaRt")
BiocManager::install("STRINGdb")



# MULTI-seq
devtools::install_github('chris-mcginnis-ucsf/MULTI-seq', force = TRUE)
require("deMULTIplex") # This is MULTI-seq

BiocManager::install("ShortRead")

BiocManager::install("DropletUtils")




library(devtools)
install_github("immunogenomics/harmony")
devtools::install_github("immunogenomics/presto")

devtools::install_local("/Users/abel.vertesy/Downloads/harmony")
