test_that("AT&T 2026 Q3 income statement is unchanged", {
    att <- readRDS(test_path("fixtures", "att_2026q3.rds"))
    s <- build_statement(att, "income", "10-Q")
    expect_snapshot(print(format_statement(s$data, s$tree), n = Inf))
})

test_that("AT&T 2026 Q3 balance sheet is unchanged", {
    att <- readRDS(test_path("fixtures", "att_2026q3.rds"))
    s <- build_statement(att, "balance", "10-Q")
    expect_snapshot(print(format_statement(s$data, s$tree), n = Inf))
})

test_that("AT&T 2026 Q3 cash flow is unchanged", {
    att <- readRDS(test_path("fixtures", "att_2026q3.rds"))
    s <- build_statement(att, "cash_flow", "10-Q")
    expect_snapshot(print(format_statement(s$data, s$tree), n = Inf))
})

test_that("AT&T 2026 Q3 cash flow reconciles: start + change = end", {
    att <- readRDS(test_path("fixtures", "att_2026q3.rds"))
    d <- build_statement(att, "cash_flow", "10-Q")$data
    cols <- c("2026-09-30", "2025-09-30")

    cash <- "us-gaap_CashCashEquivalentsRestrictedCashAndRestrictedCashEquivalentsIncludingDisposalGroupAndDiscontinuedOperations"
    change <- "us-gaap_CashCashEquivalentsRestrictedCashAndRestrictedCashEquivalentsPeriodIncreaseDecreaseIncludingExchangeRateEffect"

    start <- as.numeric(d[which(d$elementId == cash & d$preferredLabel == LBL_START), cols])
    end   <- as.numeric(d[which(d$elementId == cash & d$preferredLabel == LBL_END), cols])
    delta <- as.numeric(d[which(d$elementId == change & !d$is_member), cols])

    expect_equal(end, c(11181e6, 20328e6))
    expect_equal(start + delta, end)
})
