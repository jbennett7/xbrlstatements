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

test_that("build_statement uses YTD and has correct YTD period ends", {
    bs <- build_statement(make_xbrl(), "cash_flow", "10-Q")
    date_cols <- names(bs$data)[grepl("\\d{4}-\\d{2}-\\d{2}", names(bs$data))]
    expect_equal(bs$period, "YTD")
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

test_that("build_statement falls back to using \"3M\" when examining Q1 filings", {
    bs <- build_statement(make_xbrl_q1(), "cash_flow", "10-Q")
    # 3M: excluded from YTD; the Q1 value in make_xbrl_q1()
    expect_equal(bs$period, "3M")
})

test_that("build_statement date columns are correct when falling back (Q1 filings)", {
    bs <- build_statement(make_xbrl_q1(), "cash_flow", "10-Q")
    # Columns have the following dates "2026-09-30" and "2025-09-30"
    expect_equal(names(bs$data)[grepl("\\d{4}-\\d{2}-\\d{2}", names(bs$data))],
                 c("2026-09-30", "2025-09-30"))
})

test_that("build_statement uses the 3M duration fact when falling back (Q1 filings)", {
    bs <- build_statement(make_xbrl_q1(), "cash_flow", "10-Q")
    # Duration value of net change for 2026 = 20e6
    expect_equal(bs$data$`2026-09-30`[
        bs$data$elementId == "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease"], 20e6)
})

test_that("build_statement start row uses the quarter start when falling back (Q1 filings)", {
    bs <- build_statement(make_xbrl_q1(), "cash_flow", "10-Q")
    label <- "Cash at beginning of period"
    # Beginning of period 2026 = 180e6
    expect_equal(bs$data$`2026-09-30`[bs$data$label == label], 180e6)
})

test_that("build_statement for 10-K filings, the period and date columns are correct", {
    bs <- build_statement(make_xbrl_fy(), "cash_flow", "10-K")
    expect_equal(bs$period, "FY")
    expect_equal(names(bs$data)[grepl("\\d{4}-\\d{2}-\\d{2}", names(bs$data))],
                 c("2025-12-31", "2024-12-31"))
})

test_that("build_statement uses FY duration fact for 10-K filings", {
    bs <- build_statement(make_xbrl_fy(), "cash_flow", "10-K")
    id <- "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease"
    expect_equal(bs$data$`2025-12-31`[bs$data$elementId == id], 30e6)
    expect_equal(bs$data$`2024-12-31`[bs$data$elementId == id], 20e6)
})

test_that("build_statement start row uses the prior year-end balances", {
    bs <- build_statement(make_xbrl_fy(), "cash_flow", "10-K")
    label <- "Cash at beginning of period"
    expect_equal(bs$data$`2025-12-31`[bs$data$label == label], 120e6)
    expect_equal(bs$data$`2024-12-31`[bs$data$label == label], 100e6)
})

test_that("build_statement end row", {
    bs <- build_statement(make_xbrl_fy(), "cash_flow", "10-K")
    label <- "Cash at end of period"
    expect_equal(bs$data$`2025-12-31`[bs$data$label == label], 150e6)
    expect_equal(bs$data$`2024-12-31`[bs$data$label == label], 120e6)
})

test_that("build_statement reconciliation", {
    bs   <- build_statement(make_xbrl_fy(), "cash_flow", "10-K")$data
    cols <- c("2025-12-31", "2024-12-31")
    start <- as.numeric(bs[which(bs$preferredLabel == LBL_START), cols])
    end   <- as.numeric(bs[which(bs$preferredLabel == LBL_END), cols])
    delta <- as.numeric(bs[which(bs$elementId ==
        "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease"), cols])
    expect_equal(end, c(150e6, 120e6))
    expect_equal(start + delta, end)
})
