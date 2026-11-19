test_that("span_of returns the correct period", {
    expect_equal(span_of("2026-07-01", "2026-09-30"), "3M")
    expect_equal(span_of("2026-01-01", "2026-09-30"), "YTD")
    expect_equal(span_of("2025-10-01", "2026-09-30"), "FY")
    expect_equal(span_of(NA, "2026-09-30"), NA_character_)
    expect_equal(span_of(c("2026-07-01", "2026-01-01", "2025-10-01", NA_character_),
                         c("2026-09-30", "2026-09-30", "2026-09-30", "2026-09-30")),
                 c("3M", "YTD", "FY", NA_character_))
})

test_that("best_facts keeps the most precise of duplicate facts", {
    b <- best_facts(dup_facts)
    expect_equal(b$fact[b$elementId == "us-gaap_EarningsPerShareBasic"], 4.5)
})

test_that("best_facts treats INF as more precise than any number", {
    b <- best_facts(dup_facts)
    expect_equal(b$fact[b$elementId == "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic"],
                 100200000)
})

test_that("best_facts collapses exact duplicates but keeps different periods", {
    b <- best_facts(dup_facts)
    expect_equal(nrow(b[b$elementId == "us-gaap_Revenues" & b$startDate == "2026-07-01",]), 1)
    expect_equal(nrow(b[b$elementId == "us-gaap_Revenues",]), 2)
})

test_that("best_facts returns numeric fact and drops helper columns", {
    b <- best_facts(dup_facts)
    expect_type(b$fact, "double")
    expect_named(b, c("elementId", "startDate", "endDate", "fact"))
})

test_that("best_facts keeps rows that differ only by a custom key", {
    members <- tibble::tribble(
        ~elementId, ~member, ~startDate, ~endDate, ~fact, ~decimals,
        "us-gaap_Revenues", "us-gaap_ServiceMember", "2026-07-01", "2026-09-30", "700000000", "-6",
        "us-gaap_Revenues", "us-gaap_ProductMember", "2026-07-01", "2026-09-30", "300000000", "-6"
    )
    expect_equal(nrow(best_facts(members, keys = c("elementId", "member", "startDate", "endDate"))),
                 2)
    expect_equal(nrow(best_facts(members)), 1)
})

test_that("consolidated_values 3M has 16 rows and 2 start and end dates" , {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    qtr <- consolidated_values(xbrl, unique(s$elementId), "3M")
    expect_equal(nrow(qtr), 16)
    expect_equal(length(unique(qtr$startDate)), 2)
    expect_equal(length(unique(qtr$endDate)), 2)
})

test_that("consolidated_values excludes dimensional facts", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    cv   <- consolidated_values(xbrl, unique(s$elementId), "3M")
    expect_equal(cv$fact[cv$elementId == "us-gaap_Revenues" & cv$endDate == "2026-09-30"], 1e9)
    expect_false("us-gaap_CostOfGoodsAndServicesSold" %in% cv$elementId)
})

test_that("consolidated_values no YTD facts in 3M", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    cv3m  <- consolidated_values(xbrl, unique(s$elementId), "3M")
    expect_setequal(unique(cv3m$startDate), c("2026-07-01", "2025-07-01"))
    expect_false(2.9e9 %in% cv3m$fact)  # Revenues YTD 2026
})

test_that("consolidated_values YTD has four rows, 2 Revenues and 2 NetIncome", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    cvytd <- consolidated_values(xbrl, unique(s$elementId), "YTD")
    expect_equal(nrow(cvytd), 4)
    expect_equal(nrow(cvytd[cvytd$elementId == "us-gaap_Revenues",]), 2)
    expect_equal(nrow(cvytd[cvytd$elementId == "us-gaap_NetIncomeLoss",]), 2)
})

test_that("consolidated_values resolves duplicates", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    cv3m  <- consolidated_values(xbrl, unique(s$elementId), "3M")
    elementId_eps <- "us-gaap_EarningsPerShareBasic"
    elementId_num_shares <- "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic"
    expect_equal(cv3m$fact[cv3m$elementId == elementId_eps &
                           cv3m$startDate == "2026-07-01"], 4.5)
    expect_equal(cv3m$fact[cv3m$elementId == elementId_num_shares &
                           cv3m$startDate == "2026-07-01"], 100200000)
})

test_that("consolidated_values instant contexts only with three end dates including 2024-12-31", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_BS)
    s    <- flatten_tree(tree)
    cv  <- consolidated_values(xbrl, unique(s$elementId), "instant")
    expect_true(all(is.na(cv$startDate)))
    expect_setequal(unique(cv$endDate),
                    c("2026-09-30", "2026-06-30", "2025-12-31", "2025-09-30", "2024-12-31"))
})

test_that("consolidated_values with a bad period input gives an error", {
    expect_error(consolidated_values(make_xbrl(), character(), "Q3"), "is not one of")
})

test_that("latest_dates shows the newest dates first" , {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "3M")
    expect_equal(latest_dates(v), c("2026-09-30", "2025-09-30"))
})

test_that("latest_dates balance sheet values drop 2024", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_BS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "instant")
    expect_equal(latest_dates(v), c("2026-09-30", "2025-12-31"))
})

test_that("latest_dates n = 1 returns only the newest date", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_BS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "instant")
    expect_equal(latest_dates(v, n = 1), "2026-09-30")
})

test_that("latest_dates empty input gives an error", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_BS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "instant")
    expect_error(latest_dates(v[0,]), "No facts found")
})

test_that("widen has the correct shape" , {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "3M")
    d    <- latest_dates(v)
    w    <- widen(v, d)
    expect_equal(nrow(w), 8)
    expect_named(w, c("elementId", "2026-09-30", "2025-09-30"))
})

test_that("widen has one row per element: duplicate-row bug regression test" , {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "3M")
    d    <- latest_dates(v)
    w    <- widen(v, d)
    expect_equal(anyDuplicated(w$elementId), 0)
})

test_that("widen missing date gets a column that is all NA" , {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "3M")
    w    <- widen(v, c("2026-09-30", "2027-09-30"))
    expect_true(all(is.na(w$`2027-09-30`)))
})

test_that("widen column order follows date_cols", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    v    <- consolidated_values(xbrl, unique(s$elementId), "3M")
    expect_equal(colnames(widen(v, c("2026-09-30", "2027-09-30"))),
                 c("elementId", "2026-09-30", "2027-09-30"))
    expect_equal(colnames(widen(v, c("2027-09-30", "2026-09-30"))),
                 c("elementId", "2027-09-30", "2026-09-30"))
})

test_that("get_member_rows returns one row per member fact", {
    m <- member_rows_is()
    expect_equal(nrow(m), 3)
    expect_equal(m$elementId, c("us-gaap_Revenues", "us-gaap_Revenues",
                                "us-gaap_CostOfGoodsAndServicesSold"))
})

test_that("get_member_rows values come from the member context", {
    m <- member_rows_is()
    expect_equal(m[["2026-09-30"]], c(7e8, 3e8, 2e8))
    expect_equal(m[["2025-09-30"]], c(6.4e8, 2.6e8, 1.8e8))
})

test_that("get_member_rows labels use the member's preferredLabel", {
    m <- member_rows_is()
    expect_equal(m$label, c("Service", "Equipment", "Equipment"))
})

test_that("get_member_rows members sit just before their parent", {
    m <- member_rows_is()
    expect_equal(m$seq, c(1/3, 2/3, 2.5))
})

test_that("get_member_rows depth is one deeper than the parent", {
    m <- member_rows_is()
    expect_equal(m$depth, c(2, 2, 3))
})

test_that("get_member_rows YTD member facts are ignored", {
    m <- member_rows_is()
    expect_false(2e9 %in% m[["2026-09-30"]])
})

test_that("get_member_rows contexts with more than one dimension are ignored", {
    m <- member_rows_is()
    expect_false(5e8 %in% m[["2026-09-30"]])
})

test_that("get_member_rows all rows are flagged as members", {
    m <- member_rows_is()
    expect_true(all(m$is_member))
})

test_that("get_member_rows returns an empty, correctly shaped tibble when there are no axes", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_BS)
    dates <- c("2026-09-30", "2025-12-31")
    m     <- get_member_rows(xbrl, tree, flatten_tree(tree), "instant", dates)
    expect_equal(nrow(m), 0)
    expect_named(m, c("seq", "elementId", "depth", "negated", "label", dates, "is_member"))
    expect_type(m$seq, "double")
    expect_type(m$is_member, "logical")
})

test_that("get_member_rows errors when given a bad period", {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    dates <- c("2026-09-30", "2025-12-31")
    expect_error(get_member_rows(xbrl, tree, s, "Q3", dates), "is not one of")
})

test_that("merge_members has the right shape" , {
    md <- merge_members_is()
    expect_equal(nrow(md), 12)
})

test_that("merge_members keeps a parent that has its own value", {
    md <- merge_members_is()
    expect_equal(md$`2026-09-30`[md$elementId == "us-gaap_Revenues" &
                                 !(md$is_member) & md$depth == 1], 1e9)
})

test_that("merge_members an empty parent with members is replaced", {
    md <- merge_members_is()
    expect_false(any(md$elementId == "us-gaap_CostOfGoodsAndServicesSold" & !md$is_member))
})

test_that("merge_members a kept parent's members stay one level deeper", {
    md <- merge_members_is()
    rev <- md[md$elementId == "us-gaap_Revenues", ]
    expect_equal(rev$depth[!rev$is_member], 1)
    expect_equal(rev$depth[rev$is_member], c(2, 2))
})

test_that("merge_members a replacing member takes the parent's depth", {
    md <- merge_members_is()
    expect_equal(md$depth[md$elementId == "us-gaap_CostOfGoodsAndServicesSold"], 2)
})

test_that("merge_members headings without members are kept", {
    md <- merge_members_is()
    expect_equal(length(md$elementId[md$elementId == "acme_CostsAndExpensesAbstract" &
                                     is.na(md$`2026-09-30`) &
                                     is.na(md$`2025-09-30`)]), 1)
})

test_that("merge_members rows are in display order", {
    md <- merge_members_is()
    expect_equal(md$label[1:3], c("Service", "Equipment", "Total revenues"))
    expect_equal(md$elementId[5], "us-gaap_CostOfGoodsAndServicesSold")
})

test_that("merge_members seq is renumbered", {
    md <- merge_members_is()
    expect_equal(md$seq, 1:12)
})

test_that("merge_members is_member has no NAs", {
    md <- merge_members_is()
    expect_equal(sum(md$is_member), 3)
    expect_false(anyNA(md$is_member))
})

test_that("merge_members values are joined as filed, not sign-flipped", {
    md <- merge_members_is()
    expect_equal(md$`2026-09-30`[md$elementId == "us-gaap_InterestExpense"], 5e7)
    expect_true(md$negated[md$elementId == "us-gaap_InterestExpense"])
})

test_that("merge_members no members leaves the tree unchanged", {
    xbrl  <- make_xbrl()
    tree  <- role_tree(xbrl, ROLE_BS)
    s     <- flatten_tree(tree)
    dates <- c("2026-09-30", "2025-12-31")
    m     <- get_member_rows(xbrl, tree, s, "instant", dates)
    w     <- widen(consolidated_values(xbrl, unique(s$elementId), "instant"), dates)
    md    <- merge_members(s, w, m)

    expect_equal(nrow(md), 8)
    expect_false(any(md$is_member))
    expect_equal(md[["2026-09-30"]][md$elementId == "us-gaap_Assets"], 5e8)
    expect_equal(md |> select(-is_member),
                 s |> left_join(w, by = "elementId"))
})

test_that("format_statement returns the correct shape and type", {
    f <- format_statement_is()
    expect_named(f, c("display_label", "2026-09-30", "2025-09-30"))
    expect_type(f$display_label, "character")
    expect_type(f$`2026-09-30`, "character")
    expect_type(f$`2025-09-30`, "character")
})

test_that("format_statement dollars are in millions with commas", {
    f <- format_statement_is()
    expect_equal(f$`2026-09-30`[f$display_label == "Total revenues"], "1,000")
    expect_equal(f$`2025-09-30`[f$display_label == "Total revenues"], "900")
})

test_that("format_statement negated values in parentheses", {
    f <- format_statement_is()
    expect_equal(f$`2026-09-30`[f$display_label == "Interest expense"], "(50)")
    expect_equal(f$`2025-09-30`[f$display_label == "Interest expense"], "(45)")
})

test_that("format_statement EPS gets 2 decimals and isn't scaled", {
    f <- format_statement_is()
    expect_equal(f$`2026-09-30`[f$display_label == "Earnings Per Share, Basic"], "4.50")
    expect_equal(f$`2025-09-30`[f$display_label == "Earnings Per Share, Basic"], "3.95")
})

test_that("format_statement shares are represented in millions", {
    f <- format_statement_is()
    expect_equal(f$`2026-09-30`[f$display_label ==
                 "Weighted Average Number of Shares Outstanding, Basic"], "100")
    expect_equal(f$`2025-09-30`[f$display_label ==
                 "Weighted Average Number of Shares Outstanding, Basic"], "100")
})

test_that("format_statement headings are blank, not \"NA\"", {
    f <- format_statement_is()
    heading <- f$display_label == "Costs and Expenses"
    expect_equal(f$`2026-09-30`[heading], "")
    expect_equal(f$`2025-09-30`[heading], "")
})

test_that("format_statement indentation is two spaces per level below the top", {
    f <- format_statement_is()
    expect_equal(f$display_label[1], "  Service")
    expect_equal(f$display_label[3], "Total revenues")
})

test_that("format_statement the scale argument is used to scale the value", {
    md <- merge_members_is()
    f <- format_statement(md, role_tree(make_xbrl(), ROLE_IS), scale=1e3)
    expect_equal(f$`2026-09-30`[f$display_label == "Total revenues"], "1,000,000")
    expect_equal(f$`2026-09-30`[f$display_label == "Earnings Per Share, Basic"], "4.50")
    expect_equal(f$`2026-09-30`[f$display_label ==
                 "Weighted Average Number of Shares Outstanding, Basic"], "100,200")
})

test_that("format_statement shows a negative filed value on a negated line as positive" , {
    md <- merge_members_is()
    md$`2025-09-30`[md$elementId == "us-gaap_InterestExpense"] <- -45e6
    f <- format_statement(md, role_tree(make_xbrl(), ROLE_IS))
    expect_equal(f$`2025-09-30`[f$display_label == "Interest expense"], "45")
})

test_that("balance_values output is the right shape", {
    bv <- balance_values_cf()
    expect_named(bv, c("elementId", "preferredLabel", "2026-09-30", "2025-09-30"))
    expect_equal(nrow(bv), 2)
})

test_that("balance_values output is the correct values", {
    bv <- balance_values_cf()
    end <- unlist(bv[bv$preferredLabel == LBL_END, c("2026-09-30", "2025-09-30")])
    expect_equal(end, c(200e6, 180e6), ignore_attr = TRUE)
})

test_that("balance_values start row uses the instant before the YTD period", {
    bv <- balance_values_cf()
    start <- unlist(bv[bv$preferredLabel == LBL_START, c("2026-09-30", "2025-09-30")])
    # Trap: using the Q3 start (2026-06-30) instead of the YTD start
    # (2025-12-31) would give 180e6 in the 2026 column.
    expect_equal(start, c(150e6, 120e6), ignore_attr = TRUE)
})

test_that("balance_values returns only instant elements", {
    bv <- balance_values_cf()
    expect_identical(unique(bv$elementId), "us-gaap_CashAndCashEquivalentsAtCarryingValue")
})

test_that("balance_values ensure a statement without instant rows is returned empty", {
    xbrl <- make_xbrl()
    s    <- flatten_tree(role_tree(xbrl, ROLE_IS))
    v    <- consolidated_values(xbrl, unique(s$elementId), "3M")
    bv   <- balance_values(xbrl, s, v, c("2026-09-30", "2025-09-30"))
    expect_equal(nrow(bv), 0)
    expect_named(bv, c("elementId", "preferredLabel", "2026-09-30", "2025-09-30"))
})
