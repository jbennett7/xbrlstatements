test_that("period_for works", {
    expect_equal(period_for("income", "10-Q"), "3M")
    expect_equal(period_for("income", "10-K"), "FY")
    expect_equal(period_for("balance", "10-Q"), "instant")
    expect_equal(period_for("balance", "10-K"), "instant")
    expect_equal(period_for("cash_flow", "10-Q"), "YTD")
    expect_equal(period_for("cash_flow", "10-K"), "FY")
    expect_equal(period_for("income", "10-K/A"), "FY")
    expect_equal(period_for("income", "10-Q/A"), "3M")
})

test_that("find_role works", {
    xbrl <- make_xbrl()
    expect_equal(find_role(xbrl, "income"),
                 "http://acme.com/role/IncomeStatement")
    expect_equal(find_role(xbrl, "balance"),
                 "http://acme.com/role/BalanceSheet")
    expect_equal(find_role(xbrl, "cash_flow"),
                 "http://acme.com/role/CashFlows")
})

test_that("unknown kind errors and lists valid kinds", {
    expect_error(period_for("equity", "10-Q"), "Use one of: income, balance, cash_flow")
    expect_error(find_role(make_xbrl(), "equity"), "Unknown kind 'equity'")
})

test_that("find_role ignores parenthetical, comprehensive and disclosure roles", {
    xbrl <- make_xbrl()
    xbrl$role <- xbrl$role |> filter(roleId != "http://acme.com/role/IncomeStatement")
    expect_error(find_role(xbrl, "income"), "No income role found")
})

test_that("find_role with no match lists statement roles", {
    xbrl <- make_xbrl()
    xbrl$role$description[xbrl$role$type == "Statement"] <- "nothing"
    expect_error(find_role(xbrl, "income"), "Statement roles:\n  nothing")
})

test_that("find_role multiple matches errors", {
    xbrl <- make_xbrl()
    xbrl$role <- xbrl$role |>
        add_row(roleId = "http://acme.com/role/IncomeStatement2", type = "Statement",
                description = "Consolidated Statements of Operations")
    expect_error(find_role(xbrl, "income"), "Multiple income roles matched")
})

test_that("build_statement returns kind, period, tree and data", {
    s <- build_statement(make_xbrl(), "income", "10-Q")
    expect_named(s, c("kind", "period", "tree", "data"))
    expect_equal(s$period, "3M")
    expect_equal(nrow(s$data), 12)
    dates <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(s$data), value = TRUE)
    expect_equal(dates, c("2026-09-30", "2025-09-30"))
})

test_that("build_statement balance uses instant dates", {
    s <- build_statement(make_xbrl(), "balance", "10-Q")
    dates <- grep("^\\d{4}-\\d{2}-\\d{2}$", names(s$data), value = TRUE)
    expect_equal(dates, c("2026-09-30", "2025-12-31"))
})

test_that("build_statement errors when the period has no facts", {
    expect_error(build_statement(make_xbrl(), "income", "10-K"), "No facts found")
})

test_that("build_statement has correct YTD period ends", {
    bs <- build_statement(make_xbrl(), "cash_flow", "10-Q")
    date_cols <- names(bs$data)[grepl("\\d{4}-\\d{2}-\\d{2}", names(bs$data))]
    expect_equal(date_cols, c("2026-09-30", "2025-09-30"))
})

test_that("build_statement has correct shape", {
    bs <- build_statement(make_xbrl(), "cash_flow", "10-Q")
    expect_equal(nrow(bs$data), 5)
    expect_equal(bs$data$label, c("Net Income (Loss)",
                                  "Capital expenditures",
                                  "Net change in cash",
                                  "Cash at beginning of period",
                                  "Cash at end of period"))
})

test_that("build_statement duration values", {
    bs <- build_statement(make_xbrl(), "cash_flow", "10-Q")
    expect_equal(as.numeric(bs$data[
        bs$data$elementId == "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",
        c("2026-09-30", "2025-09-30")]), c(50e6, 60e6))
    expect_equal(as.numeric(bs$data[
        bs$data$elementId == "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment",
        c("2026-09-30", "2025-09-30")]), c(1250e6, 1090e6))
    expect_true(bs$data$negated[
        bs$data$elementId == "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment"])
})

test_that("build_statement balance rows", {
    bs    <- build_statement(make_xbrl(), "cash_flow", "10-Q")
    cols  <- c("2026-09-30", "2025-09-30")
    start <- bs$data[bs$data$label == "Cash at beginning of period", cols]
    end   <- bs$data[bs$data$label == "Cash at end of period", cols]
    expect_equal(as.numeric(start), c(150e6, 120e6))
    expect_equal(as.numeric(end),   c(200e6, 180e6))
})

test_that("build_statement rows are formatted correctly", {
    bs <- build_statement(make_xbrl(), "cash_flow", "10-Q")
    f <- format_statement(bs$data, bs$tree)
    expect_equal(f$`2026-09-30`[f$display_label == "Capital expenditures"], "(1,250)")
    expect_equal(f$`2026-09-30`[f$display_label == "Cash at end of period"], "200")
})
