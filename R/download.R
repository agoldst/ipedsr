
files_prefixes <- c(
    SIS="S",
    OC="S",
    HD="HD",
    EFIA="EFIA",
    EAP="EAP",
    C="C"
)
files_suffixes <- c(
    SIS="_SIS",
    OC="_SIS",
    HD="",
    EFIA="",
    EAP="",
    C="_A"
)

#' Available shorthands for IPEDS data files
#'
#' @export
ipeds_available_files <- names(files_prefixes)




# Utility functions for getting IPEDS data files and reading them in tidily.
# Yes I know this hsould be a package. Leave me alone.

library(tidyverse)

# take a vector of zip file names, download and extract data where not already
# present, return a list of extracted files

get_ipeds_zip <- function (fs) {
    result <- fs
    names(result) <- fs
    for (zfile in fs) {
        if (!file.exists(zfile)) {
            url <- str_c("https://nces.ed.gov/ipeds/datacenter/data/",
                         basename(zfile))
            message("Downloading ", url)
            download.file(url, zfile)
            Sys.sleep(1) # don't shut down the federal government...'s server
        }
        flist <- unzip(zfile, list=T)[["Name"]]

        # check for presence of revised data;
        # derive extracted filename

        rv <- str_detect(flist, "_rv")
        if (sum(rv) > 1) {
            stop("More than one _rv file found.")
        }
        else if (sum(rv) == 1) {
            f <- flist[rv]
        } else {
            stopifnot(length(flist) == 1)
            f <- flist
        }
        f <- file.path(data_dir, f)

        if (!file.exists(f)) {
            message("Extracting  ", f)
            unzip(zfile, files=basename(f), exdir=data_dir)
        }
        result[zfile] <- f
    }
    result
}

get_ipeds <- function (prefix, years, suffix)
    get_ipeds_zip(file.path(data_dir, str_c(prefix, years, suffix, ".zip")))



load_series <- function (fs) tibble(filename=fs) %>%
    dplyr::mutate(year=as.numeric(stringr::str_extract(filename, "\\d{4}"))) %>%
    dplyr::mutate(frm=map(filename, readr::read_csv)) %>%
    tidyr::unnest(frm)

# given a dataframe x and column name (as string) varname, rewrite varname using IPEDS
# data dictionary (values in coduevalue, labels in valuelabel

ipeds_label <- function (x, varname, dict) {
    dict <- dict %>% filter({{ varname }} == varname) %>%
        select(codevalue, valuelabel)

    # dplyr programming is too hard

    x[[varname]] <- as.character(x[[varname]])
    x <- inner_join(x, dict, by=setNames("codevalue", varname))
    x[[varname]] <- x$valuelabel
    x$valuelabel <- NULL
    x
}





#' Get filenames corresponding to an IPEDS survey component
#'
#' @export
ipeds_files <- function (survey, years)
    paste0(files_prefixes[survey], years, files_suffixes[survey])

#' Download IPEDS data files from NCES
#'
#' @export
#'
ipeds_download <- function (data_files, data_dir="ipeds", force=FALSE,
                            sleep_interval=1) {

    if (sleep_interval <= 0) {
        stop("Please specify a positive sleep interval between GET requests")
    }

    if (!dir.exists(data_dir)) {
        message("Creating data directory ", data_dir)
        dir.create(data_dir)
    }

    fs <- file.path(data_dir, paste0(data_files, ".zip"))
    result <- character(length(fs))

    names(result) <- fs
    for (zfile in fs) {
        if (force || !file.exists(zfile)) {
            url <- str_c("https://nces.ed.gov/ipeds/datacenter/data/",
                         basename(zfile))
            message("Downloading ", url)
            download.file(url, zfile)
            Sys.sleep(sleep_interval) # don't shut down the federal government...'s server
        }
        flist <- unzip(zfile, list=T)[["Name"]]

        # check for presence of revised data;
        # derive extracted filename

        rv <- str_detect(flist, "_rv")
        if (sum(rv) > 1) {
            stop("More than one _rv file found.")
        }
        else if (sum(rv) == 1) {
            f <- flist[rv]
        } else {
            stopifnot(length(flist) == 1)
            f <- flist
        }
        f <- file.path(data_dir, f)

        if (force || !file.exists(f)) {
            message("Extracting  ", f)
            unzip(zfile, files=basename(f), exdir=data_dir)
        }
        result[zfile] <- f
    }
    result
}

#' Load multiple years of an IPEDS survey component
#'
#' @export
ipeds_load_series <- function (data_files, data_dir="ipeds", force=FALSE,
                               read=NULL, ...) {
    fs <- ipeds_download(data_files, data_dir=data_dir, force=force)


    if (is.null(read)) {
        read <- function (f) {
            if (str_detect(f, "HD"))
                read_hd_csv(f)
            else
                readr::read_csv(f, ...)
        }
    }


    tibble::tibble(filename=fs) %>%
    dplyr::mutate(year=as.numeric(stringr::str_extract(filename, "\\d{4}"))) %>%
    dplyr::mutate(frm=purrr::map(filename, read)) %>%
    tidyr::unnest(frm) %>%
    dplyr::select(-filename)
}

read_hd_csv <- function (filename) {
    readr::read_lines(filename) %>%
        stringr::str_replace_all(r'(([^,])"([^,]))', r'(\1""\2)') %>%
        stringr::str_c(collapse="\n") %>%
        readr::read_csv(col_names=TRUE,
                        cols(LONGITUD="d", LATITUDE="d", .default="c"))
}

