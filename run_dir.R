invisible(lapply(list.files('R', full.names = TRUE), source, echo = FALSE))

# variables
COUNTY <- c("wake", "durham") # current counties
DROPBOX_DIR <- "~/Dropbox/Jails/data"

# get dir paths
PATHS <- file.path(DROPBOX_DIR, paste(COUNTY, 'county', sep = "_"))

# check existence of paths
indx <- !dir.exists(PATHS)
if(any(indx)) invisible(lapply(PATHS[indx], dir.create))

# functions list
FUNS <- list(
  f <- function(x) get_wake_county_inmate_data(OUTPUT_DIR = x),
  f <- function(x) lapply(c(0,1,30), get_durham_county_inmate_data, OUTPUT_DIR = x)
)

mapply(function(f,x) f(x), FUNS, PATHS)