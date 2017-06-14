
# SET UP VARIABLES
# record_type:
#   "0" (all incarcerated)
#   "1" (last 24 hours)
#   "30" (last 30 days) DEFAULT

get_durham_county_inmate_data <- function(record_type = "1", OUTPUT_DIR = "~/Dropbox/Jails/data") {

  record_type = as.character(record_type)

  DATE <- format(Sys.time(), '%Y-%m-%d')

  # OUTPUT VARIABLES
  url <- 'http://www2.durhamcountync.gov/sheriff/ips/default.aspx'
  type <- switch(record_type, "0" = "all-incarcerated", "1" = "last-24-hours", "last-30-days")
  FILENAME <- sprintf("%s-durham-county-records-%s.csv", DATE, type)

  # FUNCTIONS ------------------------------------------------
  # set up html session
  durham_POST <- function(url, record_type = "30", index = "All") {
    session <- rvest::html_session(url)
    form <- rvest::html_form(session)[[1]] # get form values for session
    # str(form$fields) # look at 'options' for the "select" class

    # NOTE:
    # dynamic loading of pages means that one SHOULD NOT resubmit the default form loaded when first starting session
    # therefore, we check to see if desired form equals the default form. if so, parse immediately

    l <- rvest::pluck(form$fields, 'value')$ddlDateListing # default listing

    # check if desired record type matches listing
    if(record_type == l) {
      xml2::read_html(session)
    } else {
      form <- rvest::set_values(form, ddlDateListing = record_type, ddlNameIndex = index) #
      body <- rvest::pluck(form$fields, 'value') # convert to named list with pluck
      r <- httr::POST(session$url, body = body, httr::verbose()) # sending form data request
      httr::content(r, 'parsed')
    }
  }

  durham_convert <- function(dat, grp) {
    v <- unique(grp) # get group values
    d <- lapply(v, function(x) {
      i <- which(grp %in% x)
      name <- dat[i][[1]][[1]] %>% setNames(., 'name')
      cols <- dat[i][2] %>%
        unlist() %>%
        tolower() %>%
        gsub('\\W', '_', .) # replace punct/space _
      tbl <- do.call(rbind, dat[i][-c(1,2)]) %>% setNames(., cols)
      tibble::as.tibble(cbind(tbl, name))
    })
    d <- dplyr::bind_rows(d)

    # clean up data. order matters.
    d %>%
      dplyr::mutate(days_in_jail_charge = gsub('<1', '.5', days_in_jail_charge)) %>%
      dplyr::mutate(bond_amount = gsub('\\W', '', bond_amount)) %>%
      dplyr::mutate(incarcerated = date_released == "[incarcerated]") %>%
      dplyr::mutate(date_released = gsub('\\[incarcerated\\]', NA_character_, date_released)) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("date_")), dplyr::funs(as.Date), format = '%m/%d/%Y') %>%
      dplyr::mutate_if(is.character, gsub, pattern = '\\[N/A\\]', replacement = NA_character_) %>%
      dplyr::mutate_at(dplyr::vars(bond_amount, days_in_jail_charge), as.numeric)
  }

  # GET DATA ------------------------------------------------

  # extract data
  xml <- durham_POST(url, record_type) %>%
    xml2::xml_find_all(".//form//table[@id='Table1']")

  # the table has no nesting. sucks.
  dat <- xmltools::xml_dig_df(xml)[[1]]
  nms <- lapply(dat, names)
  indx <- sapply(nms, is.null) # when null, its a name
  grp <- cumsum(indx) # gives us an grouping

  tbl <- durham_convert(dat, grp)

  readr::write_csv(tbl, file.path(OUTPUT_DIR, FILENAME))

}