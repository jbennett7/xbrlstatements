LABEL_STANDARD <- "http://www.xbrl.org/2003/role/label"
LABEL_TERSE    <- "http://www.xbrl.org/2003/role/terseLabel"

`%||na%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' Collect a role's presentation arcs, elements and labels
#'
#' @noRd
#' @param xbrl a parsed filing, as returned by [process_filing()] or
#'    `XBRL::xbrlDoAll()`
#' @param role_id the statement's role URI (e.g.
#'    "http://att.com/role/ConsolidatedBalanceSheets"), as returned by
#'    `find_role()`
#' @return a list: `role_id`, `pres` (arcs), `elements`, `labels`, `root`
role_tree <- function(xbrl, role_id) {
    if (length(role_id) != 1) stop("role_id must be a single value")
    pres <- xbrl$presentation |>
        filter(roleId == role_id) |>
        select(fromElementId, toElementId, order, preferredLabel) |>
        mutate(order = as.numeric(order))
    if (nrow(pres) == 0) stop("No presentation arcs for role ", role_id)
    
    elements <- xbrl$element |>
        select(elementId, type, substitutionGroup, abstract) |>
        mutate(structural = substitutionGroup %in%
               c("xbrldt:hypercubeItem", "xbrldt:dimensionItem") |
               type == "dtr-types:domainItemType")
    
    cubes <- elements$elementId[elements$substitutionGroup == "xbrldt:hypercubeItem"]
    line_items <- pres$toElementId[pres$fromElementId %in% cubes]
    elements <- elements |>
        mutate(structural = structural |
               (elementId %in% line_items & abstract %in% c("true", TRUE)))

    tree_ids <- union(pres$fromElementId, pres$toElementId)
    labels <- xbrl$label |>
        filter(elementId %in% tree_ids) |>
        select(elementId, labelRole, labelString)
    
    root <- setdiff(pres$fromElementId, pres$toElementId)
    if (length(root) != 1)
        stop("Expected one root, found ", length(root), ": ", paste(root, collapse = ", "))

    list(role_id = role_id, pres = pres, elements = elements, labels = labels, root = root)
}

#' Pick an element's label, preferring the arc's label role
#'
#' @noRd
#' @param tree the tree element from `role_tree()`
#' @param elt an element id, e.g. "us-gaap_NetIncomeLoss"
#' @param preferred_role the label role URI from the presentation arc's
#'    `preferredLabel` (e.g. a terseLabel or totalLabel URI); NULL or NA
#'    means the standard label
#' @details Tries the preferred role, then terseLabel, then the standard label;
#'    returns `elt` if none exist. A trailing `[Abstract]`-style suffix is removed.
label_for <- function(tree, elt, preferred_role = NULL) {
    roles <- unique(c(preferred_role %||na% LABEL_STANDARD, LABEL_TERSE, LABEL_STANDARD))
    cand <- tree$labels |> filter(elementId == elt)
    hit <- match(roles, cand$labelRole)
    hit <- hit[!is.na(hit)]
    if (length(hit) == 0) return(elt)

    sub("\\s*\\[[A-Za-z ]+\\]$", "", cand$labelString[hit[1]])
}

#' Walk the tree depth-first into one row per display line
#'
#' @noRd
#' @param tree the tree element from `role_tree()`
flatten_tree <- function(tree) {
    walk <- function(id, depth, plabel) {
        is_struct <- isTRUE(tree$elements$structural[match(id, tree$elements$elementId)])
        row <- if (!is_struct)
            tibble(elementId = id, depth = depth,
                   negated   = grepl("negated", plabel %||na% ""),
                   label     = label_for(tree, id, plabel),
                   preferredLabel = plabel)

        kids <- tree$pres |> filter(fromElementId == id) |> arrange(order)
        child_depth <- if (is_struct) depth else depth + 1
        child_rows  <- map2(kids$toElementId, kids$preferredLabel,
                            \(k, p) walk(k, child_depth, p))

        bind_rows(row, bind_rows(child_rows))
    }

    walk(tree$root, depth = 0, plabel = NA) |>
        slice(-1) |>
        mutate(seq = row_number(), .before = 1)
}

#' All elements below a node
#'
#' @noRd
#' @param tree the tree element from `role_tree()`
#' @param id the element id of the node whose descendants are returned
descendants <- function(tree, id) {
    kids <- tree$pres$toElementId[tree$pres$fromElementId == id]
    c(kids, unlist(lapply(kids, \(k) descendants(tree, k))))
}

#' All members below the given axes
#'
#' @noRd
#' @param tree the tree element from `role_tree()`
#' @param axes element ids of the axis nodes in the tree (elements whose
#'    substitutionGroup is xbrldt:dimensionItem), e.g. "srt_ProductOrServiceAxis"
tree_members <- function(tree, axes)
    unique(unlist(lapply(axes, descendants, tree = tree)))

#' Mark each element as consolidated (`*`) or dimensional-only (`d`)
#'
#' @noRd
#' @param xbrl a parsed filing, as returned by [process_filing()] or
#'    `XBRL::xbrlDoAll()`
fact_status <- function(xbrl) {
    all_ids    <- unique(xbrl$fact$elementId)
    consol_ids <- xbrl$fact |>
        anti_join(xbrl$dimension, by = "contextId") |>
        pull(elementId) |> unique()
    status <- setNames(rep("d", length(all_ids)), all_ids)  # dimensional only
    status[consol_ids] <- "*"                               # has a consolidated fact
    status
}

#' Print the presentation tree (for debugging)
#'
#' @noRd
#' @param tree the tree element from `role_tree()`
#' @param facts optional `fact_status()` output, for marking each line
#' @param id the element id to start from (default: the tree's root)
#' @param depth current indentation depth (used by the recursion)
#' @param plabel preferredLabel of the arc leading to `id`, shown after it
#'    (used by the recursion)
print_tree <- function(tree, facts = NULL, id = tree$root, depth = 0, plabel = NA) {
    el   <- tree$elements[match(id, tree$elements$elementId),]
    tag  <- if (isTRUE(el$structural)) "[S]"
            else if (isTRUE(el$abstract %in% c("true", TRUE))) "[A]"
            else "   "
    mark <- if (is.null(facts)) "" else (facts[id] %||na% " ")
    role <- if (is.na(plabel)) "" else paste0("  <", basename(plabel), ">")
    cat(sprintf("%s %s %s%s%s\n", tag, mark, strrep("  ", depth), id, role))

    kids <- tree$pres |> filter(fromElementId == id) |> arrange(order)
    for (i in seq_len(nrow(kids)))
        print_tree(tree, facts = facts, id = kids$toElementId[i],
                   depth = depth + 1, plabel = kids$preferredLabel[i])
}

