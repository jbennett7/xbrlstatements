#' Process an SEC filing based on accession number
#'
#' @export
#' @param cik cik number of company
#' @param accession accession number of the document
#' @param useragent useragent used to access EDGAR
process_filing <- function(cik, accession, useragent) {
    message(paste0("Processing ", accession))
    # Guarantee a unique path for xbrl expansion
    tmp <- file.path(tempdir(), accession)
    dir.create(tmp)
    on.exit(unlink(tmp, recursive=TRUE), add = TRUE)

    sgml <- get_filing(cik, accession, useragent)
    extract_instance(sgml, out_dir=tmp)
    inst <- list.files(tmp,
                       pattern="(?:\\d{8}|_htm)\\.xml", full.names=TRUE)
    if (length(inst) != 1) stop("Expected one instance file, found ", length(inst))
    xbrlDoAll(inst)
}
