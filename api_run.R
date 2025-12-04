
library(plumber)

pr <- plumber::plumb("api.R")
pr$setDocs(FALSE) 
pr$run(host = "0.0.0.0", port = 8000)

source("api.R")