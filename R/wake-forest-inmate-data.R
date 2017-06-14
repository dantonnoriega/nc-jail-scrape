
get_wake_county_inmate_data <- function(OUTPUT_DIR = "~/Dropbox/Jails/data") {
  # variables
  DATE <- format(Sys.time(), '%Y-%m-%d')
  FILENAME <- sprintf("%s-wake-county-records.csv", DATE)
  url <- 'http://p2c.wakeso.net/jqHandler.ashx?op=s'

  # set up form request
  # query found using Chrome developer tools.
  # Ran Query > Go to "Network" tab > find "jqHandler.ashx?op=s"
  # Then, find "Request URL" (top) and "Form Data" (bottom)
  body <- list(
    t = "ii",
    "_search" = "false",
    rows = 10000, # gets all rows
    page = 1,
    sidx = "disp_name",
    sord = "asc"
  )

  r <- httr::POST(url, body = body, encoding = 'form', httr::verbose()) # sending form data request

  tbl <- httr::content(r, "parsed", "application/json") %>% # data is in json
    '$'('rows') %>% # what we want is in the 'rows' array
    lapply(., tibble::as.tibble) %>% # convert each row to a dataframe
    dplyr::bind_rows() # stack

  readr::write_csv(tbl, file.path(OUTPUT_DIR, FILENAME))
}