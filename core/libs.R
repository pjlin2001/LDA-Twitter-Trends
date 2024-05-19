## This helps with library import and avoid duplicate imports

export("getPackages")

getPackages <- function(ref){
  # read in the list of packages
  if(ref=="ui"){
    listOfPackages <- as.vector ( utils::read.delim2("core/packages_ui.txt")[, 1] )
  }else if(ref=="server"){
    listOfPackages <- as.vector ( utils::read.delim2("core/packages_server.txt")[, 1] )
  }
  # resolve newly added packages
  newPackages <- listOfPackages[!(listOfPackages %in% utils::installed.packages()[,"Package"])]
  # install new packages if necessary
  #if(length(newPackages)) utils::install.packages(newPackages)
  # now load the libraries
  #lapply(listOfPackages, require, character.only = TRUE) for local development
  
}