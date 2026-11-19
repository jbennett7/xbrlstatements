test_that("label_for returns standard label when there's no preferred role", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(label_for(tree, "us-gaap_Revenues"), "Revenues")
})

test_that("label_for returns preferred role when it exists", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(label_for(tree, "us-gaap_Revenues", LBL_TOT), "Total revenues")
})

test_that("label_for falls back to terse when preferred label is missing", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(label_for(tree,
        "us-gaap_SellingGeneralAndAdministrativeExpense",
        LBL_VERB), "Selling, general and administrative")
})

test_that("label_for the `[Member]` suffix is removed", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(label_for(tree, "us-gaap_ServiceMember"), "Service")
})

test_that("label_for multi-word `[Line Items]` suffix is removed", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(label_for(tree, "us-gaap_StatementLineItems"), "Statement")
})

test_that("label_for if there are no labels then fall back to elementId", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(label_for(tree, "us-gaap_NoSuchElement"), "us-gaap_NoSuchElement")
})

test_that("label_for an NA preferred role (from presentation) is handled", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(label_for(tree, "us-gaap_Revenues", NA), "Revenues")
})

test_that("role_tree returns the expected shape", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_named(tree, c("role_id", "pres", "elements", "labels", "root"))
})

test_that("role_tree filters the presentation arcs to one role", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    expected <- xbrl$presentation |> filter(roleId == ROLE_IS)
    expect_setequal(paste(tree$pres$fromElementId, tree$pres$toElementId),
                    paste(expected$fromElementId, expected$toElementId))
})

test_that("role_tree order is numeric", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_type(tree$pres$order, "double")
})

test_that("role_tree test root for income role", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_equal(tree$root, "us-gaap_IncomeStatementAbstract")
})

test_that("role_tree test root for balance sheet role", {
    tree <- role_tree(make_xbrl(), ROLE_BS)
    expect_equal(tree$root, "us-gaap_StatementOfFinancialPositionAbstract")
})

test_that("role_tree that the labels are limited to the tree", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_true ("us-gaap_Revenues" %in% tree$labels$elementId)
    expect_false("us-gaap_Assets"   %in% tree$labels$elementId)
})

test_that("role_tree structural flags", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    is_struct <- function(tree, id) tree$elements$structural[tree$elements$elementId == id]
    expect_true(is_struct(tree, "us-gaap_StatementTable"))
    expect_true(is_struct(tree, "srt_ProductOrServiceAxis"))
    expect_true(is_struct(tree, "srt_ProductsAndServicesDomain"))
    expect_true(is_struct(tree, "us-gaap_ServiceMember"))
    expect_true(is_struct(tree, "us-gaap_StatementLineItems"))

    expect_false(is_struct(tree, "acme_CostsAndExpensesAbstract"))
    expect_false(is_struct(tree, "us-gaap_Revenues"))
})

test_that("role_tree error cases: more than one role_id", {
    xbrl <- make_xbrl()
    expect_error(role_tree(xbrl, c(ROLE_IS, ROLE_BS)), "role_id must be a single value")
})

test_that("role_tree error cases: role with no arcs", {
    xbrl <- make_xbrl()
    expect_error(role_tree(xbrl, "http://acme.com/role/Cover"), "No presentation arcs for role")
})

test_that("role_tree error cases: two roots", {
    xbrl <- make_xbrl()
    xbrl$presentation <- xbrl$presentation |>
        add_row(roleId = ROLE_IS, fromElementId = "acme_OrphanAbstract",
                toElementId = "us-gaap_Revenues", order = "1")
    expect_error(role_tree(xbrl, ROLE_IS), "Expected one root, found 2")
})

test_that("tree_members returns the domain and all members under an axis", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_setequal(tree_members(tree, "srt_ProductOrServiceAxis"), c(
        "srt_ProductsAndServicesDomain",
        "us-gaap_ServiceMember",
        "us-gaap_ProductMember"))
})

test_that("descendants recurses through every level below a node", {
    tree <- role_tree(make_xbrl(), ROLE_IS)
    expect_setequal(descendants(tree, "us-gaap_StatementTable"), c(
        "srt_ProductOrServiceAxis",
        "us-gaap_StatementLineItems",
        "srt_ProductsAndServicesDomain",
        "us-gaap_ServiceMember",
        "us-gaap_ProductMember",
        "us-gaap_Revenues",
        "acme_CostsAndExpensesAbstract",
        "us-gaap_OperatingIncomeLoss",
        "us-gaap_InterestExpense",
        "us-gaap_NetIncomeLoss",
        "us-gaap_EarningsPerShareBasic",
        "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic",
        "us-gaap_CostOfGoodsAndServicesSold",
        "us-gaap_SellingGeneralAndAdministrativeExpense",
        "us-gaap_CostsAndExpenses"))
})

test_that("flatten_tree has the right shape", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_named(s, c("seq", "elementId", "depth", "negated", "label", "preferredLabel"))
})

test_that("flatten_tree drops the root", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_false("us-gaap_IncomeStatementAbstract" %in% s$elementId)
})

test_that("flatten_tree the structural nodes are skipped", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    structural <- c("us-gaap_StatementTable", "srt_ProductOrServiceAxis",
                    "srt_ProductsAndServicesDomain", "us-gaap_ServiceMember",
                    "us-gaap_ProductMember", "us-gaap_StatementLineItems")
    expect_false(any(structural %in% s$elementId))
})

test_that("flatten_tree lists rows in presentation order", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_equal(s$elementId, c("us-gaap_Revenues",
                                "acme_CostsAndExpensesAbstract",
                                "us-gaap_CostOfGoodsAndServicesSold",
                                "us-gaap_SellingGeneralAndAdministrativeExpense",
                                "us-gaap_CostsAndExpenses",
                                "us-gaap_OperatingIncomeLoss",
                                "us-gaap_InterestExpense",
                                "us-gaap_NetIncomeLoss",
                                "us-gaap_EarningsPerShareBasic",
                                "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic"))
})

test_that("flatten_tree seq is in order", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_equal(s$seq, 1:10)
})

test_that("flatten_tree sorts siblings by numeric order, not text", {
    xbrl <- make_xbrl()
    xbrl$presentation$order[xbrl$presentation$toElementId == "us-gaap_Revenues"] <- "10"
    s <- flatten_tree(role_tree(xbrl, ROLE_IS))
    expect_equal(tail(s$elementId, 1), "us-gaap_Revenues")
})

test_that("flatten_tree the structural nodes add no depth", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_equal(s[s$elementId == "us-gaap_Revenues",]$depth, 1)
})

test_that("flatten_tree headings add depth", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_equal(s[s$elementId == "us-gaap_CostOfGoodsAndServicesSold",]$depth, 2)
})

test_that("flatten_tree negated is correctly applied", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_equal(s$elementId[s$negated], "us-gaap_InterestExpense")
})

test_that("flatten_tree preferredLabel is used", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_IS))
    expect_equal(s$label[s$elementId == "us-gaap_Revenues"], "Total revenues")
    expect_equal(s$label[s$elementId == "us-gaap_SellingGeneralAndAdministrativeExpense"],
                 "Selling, general and administrative")
})

test_that("flatten_tree works for the balance sheet tree", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_BS))
    expect_equal(nrow(s), 8)
    expect_true(all(s$depth[endsWith(s$elementId, "Abstract")] == 1))
    expect_true(all(s$depth[!endsWith(s$elementId, "Abstract")] == 2))
})

test_that("flatten_tree keeps the arc's preferredLabel", {
    s <- flatten_tree(role_tree(make_xbrl(), ROLE_BS))
    expect_type(s$preferredLabel, "character")
    expect_identical(s$preferredLabel[s$elementId == "us-gaap_Assets"], LBL_TOT)
    expect_true(is.na(s$preferredLabel[s$elementId == "us-gaap_AccountsReceivableNetCurrent"]))
})
