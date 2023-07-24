library(pacman)
p_load('pacman', 'mongolite', 'dplyr', 'tidyverse',
        'readr', 'jsonlite')

sensitive_data_loc <- "../sensitive-data"

init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))

server <- "dev"

if(server == "prod") {
  connection_string = init_file$connection_string_prod
  db <- "carbonplus_production_db"
} else if(server == "dev") {
  connection_string = init_file$connection_string_cfdev
  db <- "carbonplusdb"
} else if(server == "test") {
  connection_string = init_file$connection_string_test
  db <- "test_server_db"
} else {stop("Wrong value for variable: server")}
farms_collection = mongo(collection="farms", db=db, url=connection_string)

# Upload to database
carbonresults_collection = mongo(collection="carbonresults", db=db, url=connection_string)

farmIds <- c(
  # 'edf5cce8-eee2-40a8-af32-520d2b93ab5c',
  # '7fe9ced2-73b8-45aa-b6a2-a9ede144ca1b',
  # '3f916c12-3a2c-4904-91cb-bb64e6fb0832',
  # 'f67333e8-34a9-4030-93af-766f49d01310',
  # '584b48dc-0e5d-4ecc-b7d4-9acf281faaba',
  # 'bb393d6d-f952-474e-a790-5486365d929b'
)


# # Get code version and time info
# tag <- system2(command = "git", args = "describe", stdout = TRUE)
# full_tag <- paste0("R-model-version: test ", tag)
# currentTime <- format(Sys.time(), "%Y-%m-%d %H:%M")
# currentYear <- format(Sys.time(), "%Y")
farms_everything = farms_collection$find(paste('{"farmInfo.farmId":"',farmId,'"}',sep=""))
# farms_everything$runInfo <- data.frame(
#   modelVersion=full_tag,
#   resultsGenerationYear=currentYear,
#   resultsGenerationTime=currentTime
#   )
# carbonresults_collection$insert(farms_everything)


