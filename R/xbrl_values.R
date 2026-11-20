check_period <- function(period) {
    if (!(period %in% c("3M", "YTD", "FY", "instant"))) {
        stop(paste(period, "is not one of: 3M, YTD, FY or instant"))
    }
}

#' Classify a duration as `3M`, `YTD` or `FY`
#'
#' @noRd
#' @param start the start date
#' @param end the end date
span_of <- function(start, end) {
    days <- as.numeric(as.Date(end) - as.Date(start))
    case_when(
        is.na(days) ~ NA_character_,
        days < 100  ~ "3M",
        days > 300  ~ "FY",
        TRUE        ~ "YTD"
    )
}

#' Keep the most precise fact per key
#'
#' @noRd
#' @param facts facts joined to their contexts, with `fact` and `decimals`
#'     columns
#' @param keys columns that identify one fact
best_facts <- function(facts, keys=c("elementId", "startDate", "endDate")) {
    facts |>
        distinct(across(all_of(c(keys, "fact", "decimals")))) |>
        mutate(fact = as.numeric(fact),
               precision = if_else(decimals == "INF", Inf,
                                   suppressWarnings(as.numeric(decimals)))) |>
        group_by(across(all_of(keys))) |>
        slice_max(precision, n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(-decimals, -precision)
}

#' Facts without dimensions for the given elements and period
#'
#' @noRd
#' @param xbrl a parsed filing, as returned by [process_filing()] or
#'     `XBRL::xbrlDoAll()`
#' @param elt_ids element ids to keep
#' @param period one of `3M`, `YTD`, `FY` or `instant`
consolidated_values <- function(xbrl, elt_ids, period) {
    check_period(period)
    is_instant <- period == "instant"

    xbrl$fact |>
        filter(elementId %in% elt_ids) |>
        inner_join(xbrl$context |> filter(instant == is_instant), by="contextId") |>
        anti_join(xbrl$dimension, by="contextId") |>
        filter(is_instant | span_of(startDate, endDate) == period) |>
        best_facts()
}

#' The `n` dates with the most facts, newest first
#'
#' @noRd
#' @param values `consolidated_values()` output
#' @param n number of dates to keep
latest_dates <- function(values, n = 2) {
    if (nrow(values) == 0) stop("No facts found for the requested period")
    values |>
        count(endDate, name = "facts") |>
        arrange(desc(facts), desc(endDate)) |>
        head(n) |>
        arrange(desc(endDate)) |>
        pull(endDate)
}

#' Pivot facts to one column per date
#'
#' @noRd
#' @param values `consolidated_values()` output
#' @param date_cols column dates, from `latest_dates()`
#' @param id_cols columns that identify a row
widen <- function(values, date_cols, id_cols = "elementId") {
    values |>
        filter(endDate %in% date_cols) |>
        # Add missing date columns (together with pivot_wider -> names_expand = TRUE)
        mutate(endDate = factor(endDate, levels = date_cols)) |>
        pivot_wider(id_cols = all_of(id_cols), names_from = endDate,
                    values_from = fact, names_expand = TRUE) |>
        select(all_of(c(id_cols, date_cols)))
}

#' Opening and closing balances for a duration statement
#'
#' @noRd
#' @details Start rows (periodStartLabel) take the instant the day before the
#'     column's period starts; all other instant rows take the instant at the
#'     column date.
#' @param xbrl a parsed filing, as returned by [process_filing()] or
#'     `XBRL::xbrlDoAll()`
#' @param statement `flatten_tree()` output
#' @param values the duration consolidated_values output for the statement period
#' @param date_cols column dates, from `latest_dates()`
#' @return a tibble with elementId, preferredLabel and one column per date
balance_values <- function(xbrl, statement, values, date_cols) {
    instant_ids <- xbrl$element$elementId[xbrl$element$periodType == "instant"]
    instant_rows <- statement |>
        dplyr::filter(elementId %in% instant_ids) |>
        dplyr::distinct(elementId, preferredLabel)
    if (nrow(instant_rows) == 0) {
        empty <- tibble::tibble(elementId = character(), preferredLabel = character())
        empty[date_cols] <- NA_real_
        return(empty)
    }
    starts <- values |>
        dplyr::filter(endDate %in% date_cols) |>
        dplyr::count(endDate, startDate) |>
        dplyr::group_by(endDate) |>
        dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::select(endDate, startDate)
    cells <- tidyr::crossing(instant_rows, starts) |>
        dplyr::mutate(lookup = dplyr::if_else(grepl("periodStartLabel", preferredLabel),
                      as.character(as.Date(startDate) - 1),
                      endDate))
    inst_facts <- consolidated_values(xbrl, unique(instant_rows$elementId), "instant")
    cells |>
        dplyr::left_join(inst_facts |> dplyr::select(elementId, endDate, fact),
                  by = c("elementId", "lookup" = "endDate")) |>
        widen(date_cols, id_cols = c("elementId", "preferredLabel"))
}

#' Single-dimension member rows for the statement's axes
#'
#' @noRd
#' @param xbrl a parsed filing, as returned by [process_filing()] or
#'     `XBRL::xbrlDoAll()`
#' @param tree `role_tree()` output
#' @param statement `flatten_tree()` output
#' @param period one of `3M`, `YTD`, `FY` or `instant`
#' @param date_cols column dates, from `latest_dates()`
get_member_rows <- function(xbrl, tree, statement, period, date_cols) {
    check_period(period)
    is_instant <- period == "instant"

    axes <- intersect(tree$pres$toElementId,
        tree$elements$elementId[tree$elements$substitutionGroup == "xbrldt:dimensionItem"])
    if (length(axes) == 0) {
        empty <- tibble(seq = numeric(), elementId = character(), depth = numeric(),
                        negated = logical(), label = character(), is_member = logical())
        empty[date_cols] <- NA_real_
        return(empty |> select(seq, elementId, depth, negated,
                               label, all_of(date_cols), is_member))
    }
    members <- tree_members(tree, axes)
    member_order <- tree$pres |>
        filter(toElementId %in% members) |>
        distinct(member = toElementId, morder = order, plabel = preferredLabel) |>
        mutate(label = map2_chr(member, plabel, \(m, p) label_for(tree, m, p)))
    single_dim <- xbrl$dimension |>
        add_count(contextId) |>
        filter(n == 1) |>
        rename(member = value) |>
        filter(dimension %in% axes, member %in% members) |>
        select(contextId, member)
    member_values <- xbrl$fact |>
        filter(elementId %in% statement$elementId) |>
        inner_join(xbrl$context |> filter(instant == is_instant), by = "contextId") |>
        inner_join(single_dim, by = "contextId") |>
        filter(is_instant | span_of(startDate, endDate) == period) |>
        best_facts(keys = c("elementId", "member", "startDate", "endDate")) |>
        widen(date_cols, id_cols = c("elementId", "member"))
    member_values |>
        inner_join(statement |> select(elementId, seq, depth, negated), by = "elementId") |>
        left_join(member_order, by = "member") |>
        group_by(elementId) |>
        mutate(seq = seq - 1 + rank(morder) / (n() + 1), depth = depth + 1) |>
        ungroup() |>
        mutate(is_member = TRUE) |>
        select(seq, elementId, depth, negated, label, all_of(date_cols), is_member)
}

#' Combine line items and member rows; replace empty lines with their members
#'
#' @noRd
#' @param statement `flatten_tree()` output
#' @param wide `widen()` output for the line items
#' @param member_rows `get_member_rows()` output
merge_members <- function(statement, wide, member_rows) {
    has_members <- unique(member_rows$elementId)
    date_cols <- setdiff(names(wide), "elementId")
    empty_ids <- statement |>
        select(elementId) |>
        left_join(wide, by = "elementId") |>
        filter(if_all(all_of(date_cols), is.na)) |>
        pull(elementId)
    merge_ids <- intersect(has_members, empty_ids)
    statement |>
        left_join(wide, by = "elementId") |>
        bind_rows(member_rows) |>
        mutate(is_member = coalesce(is_member, FALSE)) |>
        filter(!(elementId %in% merge_ids & !is_member)) |>
        mutate(depth = if_else(is_member & elementId %in% merge_ids, depth - 1, depth)) |>
        arrange(seq) |>
        mutate(seq = row_number())
}

#' Format a statement for display
#'
#' @export
#' @param data the data element of [build_statement()]
#' @param tree the tree element of [build_statement()]
#' @param scale divisor for dollar and share amounts; the default `1e6` shows
#'     millions
#' @return a tibble with `display_label` (indented by depth) and one character
#'     column per date. `negatedLabel` rows are sign-flipped, negatives are
#'     shown in parentheses, and per-share values have 2 decimals.
format_statement <- function(data, tree, scale = 1e6) {
    date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(data), value = TRUE)
    fmt_num <- function(x, digits) {
        s <- formatC(abs(x), format = "f", digits = digits, big.mark = ",")
        out <- if_else(x < 0, paste0("(", s, ")"), s)
        if_else(is.na(x), "", out)
    }
    data |>
        mutate(across(all_of(date_cols), ~ if_else(negated, -.x, .x))) |>
        left_join(tree$elements |> select(elementId, type), by = "elementId") |>
        mutate(
            kind = case_when(
                grepl("perShare", type)     ~ "per_share",
                grepl("shares", type)       ~ "shares",
                grepl("monetary", type)     ~ "dollars",
                grepl("percentItem", type)  ~ "percentage",
                TRUE                        ~ "other"),
            across(all_of(date_cols), ~ case_when(
                kind == "per_share"              ~ fmt_num(.x, 2),
                kind %in% c("dollars", "shares") ~ fmt_num(.x / scale, 0),
                kind == "percentage"             ~ fmt_num(.x, 2),
                TRUE                             ~ fmt_num(.x, 0))),
            display_label = paste0(strrep("  ", depth - 1), label)) |>
        select(display_label, all_of(date_cols))
}
