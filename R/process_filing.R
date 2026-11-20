#' Download and parse an SEC filing's XBRL
#'
#' @export
#' @param cik the company CIK (padded or unpadded)
#' @param accession the accession number in dashed format
#' @param useragent required by SEC: "Name email@domain.com"
#' @param cache cache directory; filings go in `<cache>/filings`, XBRL schemas
#'     in `<cache>/xbrl`
#' @return the parsed filing, a list of tables from `XBRL::xbrlDoAll()`
process_filing <- function(cik, accession, useragent, cache = "./.cache") {
    message(paste0("Processing ", accession))
    # Guarantee a unique path for xbrl expansion
    tmp <- file.path(tempdir(), accession)
    dir.create(tmp, showWarnings = FALSE)
    on.exit(unlink(tmp, recursive=TRUE), add = TRUE)

    sgml <- get_filing(cik, accession, useragent, cache = cache)
    extract_instance(sgml, out_dir=tmp)
    inst <- list.files(tmp,
                       pattern="(?:\\d{8}|_htm)\\.xml", full.names=TRUE)
    if (length(inst) != 1) stop("Expected one instance file, found ", length(inst))
    xbrlDoAll(inst, cache.dir = file.path(cache, "xbrl"))
}
