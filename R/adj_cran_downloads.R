#'@title Adjusted Cran Downloads
#'
#'@description Removes daily shared downloads amongst CRAN packages and re-download/CRAN mirror spikes associated with
#'package updates.
#'
#'@param packages A character vector, the packages to query.
#'@param when \code{last-day}, \code{last-week} or \code{last-month}.
#'   If this is given, then \code{from} and \code{to} are ignored.
#'@param from Start date, in \code{yyyy-mm-dd} format, or
#'   \code{last-day}. It is ignored if \code{when} is given.
#'@param to End date, in \code{yyyy-mm-dd} format, or
#'   \code{last-day}. It is ignored if \code{when} is given.
#'@param removeupdatespikes Default TRUE. Removes the spike in downloads due to automatic re-downloads
#'and CRAN mirrors associated with a package update. Replaces value with the median download value across the measurement period.
#'@param numbercomparison Default 100. The number of random packages to inspect to determine the minimum number of downloads in any single day.
#'@param packagequantile Default 0.05. The quantile defining the "minimum" number of downloads on a single day.
#'@return A data frame of downloads and total downloads by package.
#'   \item{\code{date}}{Day of the downloads, it is a Date object.}
#'   \item{\code{package}}{The package. This column is missing if
#'     all packages were queried.}
#'   \item{\code{count}}{Raw download count.}
#'   \item{\code{total_downloads}}{Raw total number of downloads from beginning of measurement period.}
#'   \item{\code{mindownloads}}{Estimated CRAN-wide automated downloads for that day.}
#'   \item{\code{updateday}}{TRUE if the package was updated that day, FALSE otherwise.}
#'   \item{\code{adjusted_downloads}}{Daily download count, adjusted.}
#'   \item{\code{adjusted_total_downloads}}{Adjusted total number of downloads from beginning of measurement period.}
#'@import rvest dplyr cranlogs xml2
#'@export
#'@examples
#'
#'adj_cran_downloads("skpr",from="2017-08-15",to="2017-11-15")
#'adj_cran_downloads("skpr",when="last-month")
#'adj_cran_downloads(c("skpr","AlgDesign"),when="last-month")

adj_cran_downloads = function(packages, when, from, to, removeupdatespikes = TRUE, numbercomparison = 100, packagequantile=0.05) {

  packagedf = packagepublished

  if(length(packages) == 0) {
    stop("Must input at least one package.")
  }

  #search urls for package creation date, then choose earliest
  packageurls = paste0("https://cran.r-project.org/web/packages/",packages,"/index.html")
  packagearchiveurls = paste0("https://cran.r-project.org/src/contrib/Archive/",packages,"/")

  listupdates = list()

  for(package in 1:length(packages)) {
    htmltemp = tryCatch(xml2::read_html(packagearchiveurls[package]),error = function(e) {})

    if(!is.null(htmltemp)) {
      htmltemp %>%
        html_nodes("td:nth-child(3)") %>%
        html_text() %>%
        (function(x) x[-1]) -> listupdates[[package]]
    }
  }

  for(package in 1:length(listupdates)) {
    if(length(listupdates[[package]]) == 0) {
      listupdates[[package]] = xml2::read_html(packageurls[package]) %>%
        html_nodes("td") %>%
        html_text() %>%
        (function(x) x[which(x=="Published:")+1])
    } else {
      listupdates[[package]] = c(listupdates[[package]],xml2::read_html(packageurls[package]) %>%
                                   html_nodes("td") %>%
                                   html_text() %>%
                                   (function(x) x[which(x=="Published:")+1]))
    }
  }

  packagedates = list()

  for(package in 1:length(listupdates)) {
    if(length(listupdates[[package]]) == 1) {
      packagedates[[package]] = lubridate::ymd(listupdates[[package]][1])
    } else {
      numberentries = length(listupdates[[package]])
      packagedates[[package]] = lapply(listupdates[[package]][-numberentries],lubridate::ymd_hm)
      packagedates[[package]][[numberentries]] = lubridate::ymd(listupdates[[package]][numberentries])
    }
  }

  packageupdatelist = list()

  for(package in 1:length(packagedates)) {
    packageupdatelist[[package]] = data.frame(package=packages[package],date=as.Date(unlist(lapply(packagedates[[package]],lubridate::date)),origin = "1970-01-01"),stringsAsFactors = FALSE)
  }

  packageupdatedf = do.call(rbind,packageupdatelist)

  packageupdatedf$updateday = TRUE

  earliestpub = min(packageupdatedf$date)

  priorpackages = dplyr::filter(packagedf, published < earliestpub)

  if(nrow(priorpackages) > numbercomparison) {
    sampledpriorpackages =  sample_n(priorpackages,numbercomparison)
  } else {
    sampledpriorpackages =  sample_n(row(priorpackages),numbercomparison,replace = TRUE)
  }

  packagenames = sampledpriorpackages$package

  packagelogs = cranlogs::cran_downloads(packages,when=when,from=from,to=to)

  comparepackagelogs =  cranlogs::cran_downloads(packagenames,when=when,from=from,to=to)

  if(removeupdatespikes) {
    comparepackagelogs %>%
      group_by(date) %>%
      summarise(mindownloads = stats::quantile(count,packagequantile,type=1)) %>%
      left_join(packagelogs,by=c("date")) %>%
      full_join(packageupdatedf,by=c("date","package")) %>%
      mutate(updateday = ifelse(is.na(updateday),FALSE,TRUE)) %>%
      mutate(adjusted_downloads = ifelse(count - mindownloads < 0,0,count - mindownloads)) %>%
      arrange(package) %>%
      mutate(adjusted_downloads = ifelse(updateday | lag(updateday) & package == lag(package), stats::median(adjusted_downloads), adjusted_downloads)) %>%
      filter(!is.na(adjusted_downloads)) %>%
      group_by(package) %>%
      mutate(adjusted_total_downloads = cumsum(adjusted_downloads), total_downloads = cumsum(count)) %>%
      ungroup() -> finaldf
  } else {
    comparepackagelogs %>%
      group_by(date) %>%
      summarise(mindownloads = stats::quantile(count,packagequantile,type=1)) %>%
      left_join(packagelogs,by=c("date")) %>%
      full_join(packageupdatedf,by=c("date","package")) %>%
      mutate(updateday = ifelse(is.na(updateday),FALSE,TRUE)) %>%
      mutate(adjusted_downloads = ifelse(count - mindownloads < 0,0,count - mindownloads)) %>%
      arrange(package) %>%
      filter(!is.na(adjusted_downloads)) %>%
      group_by(package) %>%
      mutate(adjusted_total_downloads = cumsum(adjusted_downloads), total_downloads = cumsum(count)) %>%
      ungroup() -> finaldf
  }
  return(finaldf)
}

utils::globalVariables(c("published","mindownloads","updateday","adjusted_downloads"))
