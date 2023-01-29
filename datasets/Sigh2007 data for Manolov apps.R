# Extract, recode and export Singh et al. (2007) data for Manolov shiny apps 
# https://manolov.shinyapps.io/Overlap/ 
# https://manolov.shinyapps.io/Regression/ 

# Data must be .txt, tab-delimited, and contain only 'score' and 'phase' as column names (i.e. measurement times must be ordered ascending)
# 'phase' column must contain 'A' and 'B' 

# Need scda pacakge for data
if(!require(scda)){
  if(!require(remotes)){
    # first install package "remotes", used to install from gitlab
    install.packages("remotes")
  }
  # then install scda package from gitlab (under construction)
  remotes::install_gitlab(repo = "R-packages/scda") 
}

# Load the scda package
library(scda)

# Load data and inspect
data <- scda::Singh
data

# Subject to extract
subj_name <- "Michael"

# Extract data 
data_exp <- subset(data, id == subj_name)
data_exp

# Rename and recode according to Manolov apps
data_exp <- data_exp[, c("phase", "score_physical")]
colnames(data_exp) <- c("phase", "score")

data_exp$phase <- c("A", "B")[data_exp$phase + 1]

data_exp

# Export data in .txt
folder <- "E:/CARTI & MATERIALE/Single-Subject Design/Selectii facultate/cod/Data export for Manolov apps"
file_name <- paste0(subj_name, "_Sigh2007", ".txt")
write.table(data_exp, file = file.path(folder, file_name), sep = '\t', row.names = FALSE, quote = FALSE)
