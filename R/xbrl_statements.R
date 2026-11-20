STATEMENTS <- list(
    income = list(
        pattern = "statements? of (income|operations|earnings)(?: \\(unaudited\\))?$",
        period  = c(K = "FY", Q = "3M")),
    balance = list(
        pattern = "balance sheets?(?: \\(unaudited\\))?$",
        period  = c(K = "instant", Q = "instant")),
    cash_flow = list(
        pattern = "statements? of cash flows?(?: \\(unaudited\\))?$",
        period  = c(K = "FY", Q = "YTD"))
)

#' statement_config
#'
#' @param kind the statement type
statement_config <- function(kind) {
    cfg <- STATEMENTS[[kind]]
    if (is.null(cfg))
        stop("Unknown kind '", kind, "'. Use one of: ",
             paste(names(STATEMENTS), collapse = ", "))
    cfg
}

#' period_for
#'
#' @param kind the statement type
#' @param form the form type
period_for <- function(kind, form) {
    cfg <- statement_config(kind)
    cfg$period[[if (startsWith(form, "10-K")) "K" else "Q"]]
}

#' find_role(xbrl, kind)
#'
#' @param xbrl the data processed by xbrlDoAll
#' @param kind the statement type
find_role <- function(xbrl, kind) {
    cfg <- statement_config(kind)
    pattern <- cfg$pattern
    stmt_roles <- xbrl$role |> filter(type == "Statement")
    hits <- stmt_roles |> filter(grepl(pattern, description, ignore.case = TRUE))
    if (nrow(hits) == 0)
        stop("No ", kind, " role found. Statement roles:\n  ",
             paste(stmt_roles$description, collapse = "\n  "))
    if (nrow(hits) > 1)
        stop("Multiple ", kind, " roles matched:\n  ",
             paste(hits$description, collapse = "\n  "))
    hits$roleId
}

#' build_statement
#'
#' @export
#' @param xbrl the data processed by xbrlDoAll
#' @param kind the statement type
#' @param form the form type
build_statement <- function(xbrl, kind, form) {
    period <- period_for(kind, form)
    role_id <- find_role(xbrl, kind)
    tree <- role_tree(xbrl, role_id)
    statement <- flatten_tree(tree)
    values <- consolidated_values(xbrl, unique(statement$elementId), period)
    # For Q1 filings YTD will return nothing, retry with 3M.
    if (nrow(values) == 0 && period == "YTD") {
        period <- "3M"
        values <- consolidated_values(xbrl, unique(statement$elementId), period)
    }
    date_cols <- latest_dates(values)
    wide <- widen(values, date_cols)
    member_rows <- get_member_rows(xbrl, tree, statement, period, date_cols)
    data <- merge_members(statement, wide, member_rows)
    if (period != "instant") {
        balances <- balance_values(xbrl, statement, values, date_cols)
        data <- rows_update(data, balances, by = c("elementId", "preferredLabel"),
                            unmatched = "ignore")
    }
    list(kind = kind, period = period, tree = tree,
         data = data)
}
