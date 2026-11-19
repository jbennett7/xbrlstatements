#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map2 map2_chr
#' @importFrom tibble tibble
#' @importFrom XBRL xbrlDoAll
#' @importFrom secfilings get_filing extract_instance
#' @importFrom stats setNames
#' @importFrom utils head
NULL

utils::globalVariables(c("elementId", "fromElementId", "toElementId", "roleId",
                         "description", "type", "fact", "facts", "decimals",
                         "endDate", "startDate", "instant", "depth", "negated",
                         "label", "is_member", "member", "plabel", "contextId",
                         "value", "dimension", "morder", "preferredLabel",
                         "substitutionGroup", "abstract", "structural",
                         "labelRole", "labelString", "precision",
                         "display_label", "n", "lookup"))
