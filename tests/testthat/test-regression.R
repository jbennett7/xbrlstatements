
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
