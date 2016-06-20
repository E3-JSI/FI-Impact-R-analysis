# - - - - - - -
# Imports
# - - - - - - -

# Load the Full export file (can be downloaded in the administrative tool)
# - no additional editing is necessary for this file
# - if there are multiple surveys for one subgrantee, we only take the latest one
# - only consider projects for which we have the FI.IMPACT.IDENTIFIER
surveys <- read.delim("data/fi-impact-export.txt")
surveys <- surveys[order(surveys["Q1_23"]), ]
surveys <- surveys[which(!is.na(surveys["FI.IMPACT.IDENTIFIER"]) & surveys["FI.IMPACT.IDENTIFIER"] != ""), ]
surveys <- surveys[!duplicated(surveys["FI.IMPACT.IDENTIFIER"]), ]

# We use only Total scoring from the FIWARE Usage Assessment spreadsheet.
# The original does not contain the FI-Impact identifiers - these need to be included before import.
# Two columns necessary: FI.IMPACT.IDENTIFIER and Total scoring
`technical scores` <- read.csv("data/Final FIWARE Usage Assessment Scorecard.csv")
`technical scores`['Technical.Score..normalized.'] <- round(`technical scores`["TOTAL.scoring"] * 5 / max_na(`technical scores`["TOTAL.scoring"]), 2)

# IDC provides a spreadshit with all the subgrantees that gained additional funding with their
# FI-Impact identifier. This import accepts a text file with these identifiers, one per line.
additional.funding <- as.list(read.table("data/additional-funding.txt", quote="\"", comment.char=""))


`accelerators shorthand` <- read.csv("data/accelerators shorthand.csv")
`accelerators shorthand` <- `accelerators shorthand`[, c("Help", "Accelerator")]
accelerators <- read.csv("data/all indicators-Table 1.csv") # clean % from csv, accelerator names

# Import the list of features.
# SCORES
# The list of scores needs to have columns Score, Category, Type, Computations, Label
# - Score: names of columns from the Full export file
# - Category: short codes indicating the source and content of score
# - Type
#   * normalized if score is between 0 and 5
#   * log if the score needs to be log transformed (used for practice scores)
#   * none otherwise
# - Computations: for which computations is a score used. Currently only benchmark or none.
# - Label: name of the score used for display
# INDICATORS
# The list of indicators needs to have columns Id, Type, Label
# - Id: names of columns
# - Type: numeric, category or binary
# - Label: name of the indicator used for display
scores <- read.csv("data/scores-Table 1.csv")
indicators <- read.csv("data/indicators-Table 1.csv")