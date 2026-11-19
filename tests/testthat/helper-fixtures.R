# Test fixtures: a tiny, hand-built ACME Corp filing shaped like xbrlDoAll() output.
#
# Only the columns the package reads are included (plus a few for realism).
# Element IDs use the schema-id form (prefix_Local), as XBRL >= 1.1.0 does
# everywhere, including xbrl$dimension.
#
# Filing: ACME 10-Q for the quarter ended 2026-09-30.
#
# Income statement (3M, in $M)        2026-09-30   2025-09-30
#   Revenues  (total)                      1,000          900
#     Service   (member)                     700          640
#     Equipment (member)                     300          260
#   Costs and Expenses
#     Equipment (member; CostOfGoods has no consolidated fact -> replaced)
#                                            200          180
#     Selling, general and administrative    300          280
#     Total costs and expenses               500          460
#   Operating Income (Loss)                  500          440
#   Interest expense (negatedLabel)          (50)         (45)
#   Net income                               450          395
#   EPS basic                               4.50         3.95
#   Weighted average shares (M)              100          100
#
# Balance sheet (instant, in $M)      2026-09-30   2025-12-31   (2024-12-31 is older; dropped)
#   Cash                                     200          150
#   Receivables                              300          250
#   Total assets                             500          400
#   Liabilities                              300          240
#   Equity                                   200          160
#   Total liabilities and equity             500          400
#
# Cash flow statement (YTD, in $M)     2026-09-30   2025-09-30
#   Net income                              1,300        1,150
#   Capital expenditures (negatedLabel)    (1,250)      (1,090)
#   Net change in cash (total)                 50           60
#   Cash at beginning of period (periodStartLabel)  150   120
#   Cash at end of period (periodEndLabel)          200   180
#
# Traps built in:
#   - YTD facts (and a YTD member fact) that must not appear in a 3M statement
#   - a two-dimension context on Revenues that must be ignored
#   - EPS filed twice (decimals 2 and 0): the precise 4.5 must win over 5
#   - shares filed twice (INF and -6): the INF value must win
#   - a parenthetical role and a Disclosure role whose descriptions resemble statements

ROLE_IS <- "http://acme.com/role/IncomeStatement"
ROLE_BS <- "http://acme.com/role/BalanceSheet"
ROLE_CF <- "http://acme.com/role/CashFlows"

LBL      <- "http://www.xbrl.org/2003/role/label"
LBL_TERS <- "http://www.xbrl.org/2003/role/terseLabel"
LBL_TOT  <- "http://www.xbrl.org/2003/role/totalLabel"
LBL_VERB <- "http://www.xbrl.org/2003/role/verboseLabel"
LBL_NEG  <- "http://www.xbrl.org/2009/role/negatedLabel"
LBL_START <- "http://www.xbrl.org/2003/role/periodStartLabel"
LBL_END   <- "http://www.xbrl.org/2003/role/periodEndLabel"

fixture_roles <- function() {
    tibble::tribble(
        ~roleId,                                          ~order,    ~type,        ~description,
        "http://acme.com/role/Cover",                     "0000001", "Document",   "Cover",
        "http://acme.com/role/IncomeStatement",           "0000002", "Statement",  "CONSOLIDATED STATEMENTS OF INCOME (Unaudited)",
        "http://acme.com/role/IncomeStatementParen",      "0000003", "Statement",  "Consolidated Statements of Income (Parenthetical)",
        "http://acme.com/role/ComprehensiveIncome",       "0000004", "Statement",  "Consolidated Statements of Comprehensive Income",
        "http://acme.com/role/BalanceSheet",              "0000005", "Statement",  "Consolidated Balance Sheets",
        "http://acme.com/role/BalanceSheetParen",         "0000006", "Statement",  "Consolidated Balance Sheets (Parenthetical)",
        "http://acme.com/role/CashFlows",                 "0000007", "Statement",  "Consolidated Statements of Cash Flows",
        "http://acme.com/role/ParentStatementsOfIncome",  "0000008", "Disclosure", "Parent Company Statements of Income"
    ) |>
        dplyr::mutate(definition = paste(order, type, description, sep = " - "))
}

fixture_elements <- function() {
    str  <- "xbrli:stringItemType"
    usd  <- "xbrli:monetaryItemType"
    dom  <- "dtr-types:domainItemType"
    item <- "xbrli:item"
    tibble::tribble(
        ~elementId,                                              ~type,                        ~substitutionGroup,     ~periodType, ~abstract, ~balance,
        # income statement scaffolding
        "us-gaap_IncomeStatementAbstract",                       str,                          item,                   "duration",  "true",    NA,
        "us-gaap_StatementTable",                                str,                          "xbrldt:hypercubeItem", "duration",  "true",    NA,
        "srt_ProductOrServiceAxis",                              str,                          "xbrldt:dimensionItem", "duration",  "true",    NA,
        "srt_ProductsAndServicesDomain",                         dom,                          item,                   "duration",  "true",    NA,
        "us-gaap_ServiceMember",                                 dom,                          item,                   "duration",  "true",    NA,
        "us-gaap_ProductMember",                                 dom,                          item,                   "duration",  "true",    NA,
        "us-gaap_StatementBusinessSegmentsAxis",                 str,                          "xbrldt:dimensionItem", "duration",  "true",    NA,
        "acme_WirelessMember",                                   dom,                          item,                   "duration",  "true",    NA,
        "us-gaap_StatementLineItems",                            str,                          item,                   "duration",  "true",    NA,
        "acme_CostsAndExpensesAbstract",                         str,                          item,                   "duration",  "true",    NA,
        # income statement line items
        "us-gaap_Revenues",                                      usd,                          item,                   "duration",  "false",   "credit",
        "us-gaap_CostOfGoodsAndServicesSold",                    usd,                          item,                   "duration",  "false",   "debit",
        "us-gaap_SellingGeneralAndAdministrativeExpense",        usd,                          item,                   "duration",  "false",   "debit",
        "us-gaap_CostsAndExpenses",                              usd,                          item,                   "duration",  "false",   "debit",
        "us-gaap_OperatingIncomeLoss",                           usd,                          item,                   "duration",  "false",   "credit",
        "us-gaap_InterestExpense",                               usd,                          item,                   "duration",  "false",   "debit",
        "us-gaap_NetIncomeLoss",                                 usd,                          item,                   "duration",  "false",   "credit",
        "us-gaap_EarningsPerShareBasic",                         "dtr-types:perShareItemType", item,                   "duration",  "false",   NA,
        "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic", "xbrli:sharesItemType",       item,                   "duration",  "false",   NA,
        # balance sheet
        "us-gaap_StatementOfFinancialPositionAbstract",          str,                          item,                   "duration",  "true",    NA,
        "us-gaap_AssetsAbstract",                                str,                          item,                   "duration",  "true",    NA,
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         usd,                          item,                   "instant",   "false",   "debit",
        "us-gaap_AccountsReceivableNetCurrent",                  usd,                          item,                   "instant",   "false",   "debit",
        "us-gaap_Assets",                                        usd,                          item,                   "instant",   "false",   "debit",
        "us-gaap_LiabilitiesAndStockholdersEquityAbstract",      str,                          item,                   "duration",  "true",    NA,
        "us-gaap_Liabilities",                                   usd,                          item,                   "instant",   "false",   "credit",
        "us-gaap_StockholdersEquity",                            usd,                          item,                   "instant",   "false",   "credit",
        "us-gaap_LiabilitiesAndStockholdersEquity",              usd,                          item,                   "instant",   "false",   "credit",
        "us-gaap_StatementOfCashFlowsAbstract",                  str,                          item,                   "duration",  "true",    NA,
        "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment",    usd,                          item,                   "duration",  "false",   "credit",
        "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",  usd,                          item,                   "duration",  "false",   "debit"
    )
}

fixture_presentation <- function() {
    tibble::tribble(
        ~roleId, ~fromElementId,                                     ~toElementId,                                            ~order, ~preferredLabel,
        # income statement
        ROLE_IS, "us-gaap_IncomeStatementAbstract",                  "us-gaap_StatementTable",                                "1",    NA,
        ROLE_IS, "us-gaap_StatementTable",                           "srt_ProductOrServiceAxis",                              "1",    NA,
        ROLE_IS, "srt_ProductOrServiceAxis",                         "srt_ProductsAndServicesDomain",                         "1",    NA,
        ROLE_IS, "srt_ProductsAndServicesDomain",                    "us-gaap_ServiceMember",                                 "1",    NA,
        ROLE_IS, "srt_ProductsAndServicesDomain",                    "us-gaap_ProductMember",                                 "2",    LBL_TERS,
        ROLE_IS, "us-gaap_StatementTable",                           "us-gaap_StatementLineItems",                            "2",    NA,
        ROLE_IS, "us-gaap_StatementLineItems",                       "us-gaap_Revenues",                                      "1",    LBL_TOT,
        ROLE_IS, "us-gaap_StatementLineItems",                       "acme_CostsAndExpensesAbstract",                         "2",    NA,
        ROLE_IS, "acme_CostsAndExpensesAbstract",                    "us-gaap_CostOfGoodsAndServicesSold",                    "1",    NA,
        ROLE_IS, "acme_CostsAndExpensesAbstract",                    "us-gaap_SellingGeneralAndAdministrativeExpense",        "2",    LBL_VERB,
        ROLE_IS, "acme_CostsAndExpensesAbstract",                    "us-gaap_CostsAndExpenses",                              "3",    LBL_TOT,
        ROLE_IS, "us-gaap_StatementLineItems",                       "us-gaap_OperatingIncomeLoss",                           "3",    NA,
        ROLE_IS, "us-gaap_StatementLineItems",                       "us-gaap_InterestExpense",                               "4",    LBL_NEG,
        ROLE_IS, "us-gaap_StatementLineItems",                       "us-gaap_NetIncomeLoss",                                 "5",    LBL_TOT,
        ROLE_IS, "us-gaap_StatementLineItems",                       "us-gaap_EarningsPerShareBasic",                         "6",    NA,
        ROLE_IS, "us-gaap_StatementLineItems",                       "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic", "7",    NA,
        # balance sheet (no hypercube: exercises the no-axes path)
        ROLE_BS, "us-gaap_StatementOfFinancialPositionAbstract",     "us-gaap_AssetsAbstract",                                "1",    NA,
        ROLE_BS, "us-gaap_AssetsAbstract",                           "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "1",    NA,
        ROLE_BS, "us-gaap_AssetsAbstract",                           "us-gaap_AccountsReceivableNetCurrent",                  "2",    NA,
        ROLE_BS, "us-gaap_AssetsAbstract",                           "us-gaap_Assets",                                        "3",    LBL_TOT,
        ROLE_BS, "us-gaap_StatementOfFinancialPositionAbstract",     "us-gaap_LiabilitiesAndStockholdersEquityAbstract",      "2",    NA,
        ROLE_BS, "us-gaap_LiabilitiesAndStockholdersEquityAbstract", "us-gaap_Liabilities",                                   "1",    NA,
        ROLE_BS, "us-gaap_LiabilitiesAndStockholdersEquityAbstract", "us-gaap_StockholdersEquity",                            "2",    NA,
        ROLE_BS, "us-gaap_LiabilitiesAndStockholdersEquityAbstract", "us-gaap_LiabilitiesAndStockholdersEquity",              "3",    LBL_TOT,
        ROLE_CF, "us-gaap_StatementOfCashFlowsAbstract",             "us-gaap_NetIncomeLoss",                                 "1",    NA,
        ROLE_CF, "us-gaap_StatementOfCashFlowsAbstract",             "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment",    "2",    LBL_NEG,
        ROLE_CF, "us-gaap_StatementOfCashFlowsAbstract",             "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",  "3",    LBL_TOT,
        ROLE_CF, "us-gaap_StatementOfCashFlowsAbstract",             "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "4",    LBL_START,
        ROLE_CF, "us-gaap_StatementOfCashFlowsAbstract",             "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "5",    LBL_END
    ) |>
        dplyr::mutate(preferredLabel = as.character(preferredLabel))
}

fixture_labels <- function() {
    tibble::tribble(
        ~elementId,                                              ~labelRole, ~labelString,
        "us-gaap_IncomeStatementAbstract",                       LBL,        "Income Statement [Abstract]",
        "us-gaap_StatementTable",                                LBL,        "Statement [Table]",
        "srt_ProductOrServiceAxis",                              LBL,        "Product and Service [Axis]",
        "srt_ProductsAndServicesDomain",                         LBL,        "Product and Service [Domain]",
        "us-gaap_ServiceMember",                                 LBL,        "Service [Member]",
        "us-gaap_ProductMember",                                 LBL,        "Product [Member]",
        "us-gaap_ProductMember",                                 LBL_TERS,   "Equipment",
        "us-gaap_StatementBusinessSegmentsAxis",                 LBL,        "Segments [Axis]",
        "acme_WirelessMember",                                   LBL,        "Wireless [Member]",
        "us-gaap_StatementLineItems",                            LBL,        "Statement [Line Items]",
        "acme_CostsAndExpensesAbstract",                         LBL,        "Costs and Expenses [Abstract]",
        "us-gaap_Revenues",                                      LBL,        "Revenues",
        "us-gaap_Revenues",                                      LBL_TOT,    "Total revenues",
        "us-gaap_CostOfGoodsAndServicesSold",                    LBL,        "Cost of Goods and Services Sold",
        # verboseLabel is preferred but missing -> falls back to terse
        "us-gaap_SellingGeneralAndAdministrativeExpense",        LBL,        "Selling, General and Administrative Expense",
        "us-gaap_SellingGeneralAndAdministrativeExpense",        LBL_TERS,   "Selling, general and administrative",
        "us-gaap_CostsAndExpenses",                              LBL,        "Costs and Expenses",
        "us-gaap_CostsAndExpenses",                              LBL_TOT,    "Total costs and expenses",
        "us-gaap_OperatingIncomeLoss",                           LBL,        "Operating Income (Loss)",
        "us-gaap_InterestExpense",                               LBL,        "Interest Expense",
        "us-gaap_InterestExpense",                               LBL_NEG,    "Interest expense",
        "us-gaap_NetIncomeLoss",                                 LBL,        "Net Income (Loss)",
        "us-gaap_NetIncomeLoss",                                 LBL_TOT,    "Net income",
        "us-gaap_EarningsPerShareBasic",                         LBL,        "Earnings Per Share, Basic",
        "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic", LBL,        "Weighted Average Number of Shares Outstanding, Basic",
        "us-gaap_StatementOfFinancialPositionAbstract",          LBL,        "Statement of Financial Position [Abstract]",
        "us-gaap_AssetsAbstract",                                LBL,        "Assets [Abstract]",
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         LBL,        "Cash and Cash Equivalents",
        "us-gaap_AccountsReceivableNetCurrent",                  LBL,        "Accounts Receivable, Net",
        "us-gaap_Assets",                                        LBL,        "Assets",
        "us-gaap_Assets",                                        LBL_TOT,    "Total assets",
        "us-gaap_LiabilitiesAndStockholdersEquityAbstract",      LBL,        "Liabilities and Equity [Abstract]",
        "us-gaap_Liabilities",                                   LBL,        "Liabilities",
        "us-gaap_StockholdersEquity",                            LBL,        "Stockholders' Equity",
        "us-gaap_LiabilitiesAndStockholdersEquity",              LBL,        "Liabilities and Equity",
        "us-gaap_LiabilitiesAndStockholdersEquity",              LBL_TOT,    "Total liabilities and equity",
        "us-gaap_StatementOfCashFlowsAbstract",                  LBL,        "Statement of Cash Flows [Abstract]",
        "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment",    LBL,        "Payments to Acquire Property, Plant and Equipment",
        "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment",    LBL_NEG,    "Capital expenditures",
        "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",  LBL,        "Cash, Period Increase (Decrease)",
        "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",  LBL_TOT,    "Net change in cash",
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         LBL_START,  "Cash at beginning of period",
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         LBL_END,    "Cash at end of period"
    ) |>
        dplyr::mutate(lang = "en-US", .after = elementId)
}

fixture_contexts <- function() {
    tibble::tribble(
        ~contextId,           ~startDate,   ~endDate,     ~instant,
        # consolidated durations
        "c_q3_2026",          "2026-07-01", "2026-09-30", FALSE,
        "c_ytd_2026",         "2026-01-01", "2026-09-30", FALSE,
        "c_q3_2025",          "2025-07-01", "2025-09-30", FALSE,
        "c_ytd_2025",         "2025-01-01", "2025-09-30", FALSE,
        # consolidated instants (startDate NA, date in endDate, as XBRL does)
        "c_i_2026_09_30",     NA,           "2026-09-30", TRUE,
        "c_i_2025_12_31",     NA,           "2025-12-31", TRUE,
        "c_i_2024_12_31",     NA,           "2024-12-31", TRUE,
        "c_i_2026_06_30",     NA,           "2026-06-30", TRUE,
        "c_i_2025_09_30",     NA,           "2025-09-30", TRUE,
        # single-dimension member contexts
        "c_q3_2026_svc",      "2026-07-01", "2026-09-30", FALSE,
        "c_q3_2026_prd",      "2026-07-01", "2026-09-30", FALSE,
        "c_q3_2025_svc",      "2025-07-01", "2025-09-30", FALSE,
        "c_q3_2025_prd",      "2025-07-01", "2025-09-30", FALSE,
        "c_ytd_2026_svc",     "2026-01-01", "2026-09-30", FALSE,
        # two-dimension context (product x segment): must be ignored
        "c_q3_2026_svc_wls",  "2026-07-01", "2026-09-30", FALSE
    ) |>
        dplyr::mutate(startDate = as.character(startDate))
}

fixture_dimensions <- function() {
    axis_ps  <- "srt_ProductOrServiceAxis"
    axis_seg <- "us-gaap_StatementBusinessSegmentsAxis"
    tibble::tribble(
        ~contextId,          ~dimension, ~value,
        "c_q3_2026_svc",     axis_ps,    "us-gaap_ServiceMember",
        "c_q3_2026_prd",     axis_ps,    "us-gaap_ProductMember",
        "c_q3_2025_svc",     axis_ps,    "us-gaap_ServiceMember",
        "c_q3_2025_prd",     axis_ps,    "us-gaap_ProductMember",
        "c_ytd_2026_svc",    axis_ps,    "us-gaap_ServiceMember",
        "c_q3_2026_svc_wls", axis_ps,    "us-gaap_ServiceMember",
        "c_q3_2026_svc_wls", axis_seg,   "acme_WirelessMember"
    )
}

fixture_facts <- function() {
    tibble::tribble(
        ~elementId,                                              ~contextId,          ~unitId,         ~fact,         ~decimals,
        # --- income statement, consolidated ---
        "us-gaap_Revenues",                                      "c_q3_2026",         "usd",           "1000000000",  "-6",
        "us-gaap_Revenues",                                      "c_q3_2025",         "usd",           "900000000",   "-6",
        "us-gaap_Revenues",                                      "c_ytd_2026",        "usd",           "2900000000",  "-6",
        "us-gaap_Revenues",                                      "c_ytd_2025",        "usd",           "2650000000",  "-6",
        "us-gaap_SellingGeneralAndAdministrativeExpense",        "c_q3_2026",         "usd",           "300000000",   "-6",
        "us-gaap_SellingGeneralAndAdministrativeExpense",        "c_q3_2025",         "usd",           "280000000",   "-6",
        "us-gaap_CostsAndExpenses",                              "c_q3_2026",         "usd",           "500000000",   "-6",
        "us-gaap_CostsAndExpenses",                              "c_q3_2025",         "usd",           "460000000",   "-6",
        "us-gaap_OperatingIncomeLoss",                           "c_q3_2026",         "usd",           "500000000",   "-6",
        "us-gaap_OperatingIncomeLoss",                           "c_q3_2025",         "usd",           "440000000",   "-6",
        "us-gaap_InterestExpense",                               "c_q3_2026",         "usd",           "50000000",    "-6",
        "us-gaap_InterestExpense",                               "c_q3_2025",         "usd",           "45000000",    "-6",
        "us-gaap_NetIncomeLoss",                                 "c_q3_2026",         "usd",           "450000000",   "-6",
        "us-gaap_NetIncomeLoss",                                 "c_q3_2025",         "usd",           "395000000",   "-6",
        "us-gaap_NetIncomeLoss",                                 "c_ytd_2026",        "usd",           "1300000000",  "-6",
        "us-gaap_EarningsPerShareBasic",                         "c_q3_2026",         "usdPerShare",   "4.5",         "2",
        "us-gaap_EarningsPerShareBasic",                         "c_q3_2026",         "usdPerShare",   "5",           "0",    # rounded duplicate
        "us-gaap_EarningsPerShareBasic",                         "c_q3_2025",         "usdPerShare",   "3.95",        "2",
        "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic", "c_q3_2026",         "shares",        "100200000",   "INF",
        "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic", "c_q3_2026",         "shares",        "100000000",   "-6",   # rounded duplicate
        "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic", "c_q3_2025",         "shares",        "100000000",   "-6",
        # --- income statement, members ---
        "us-gaap_Revenues",                                      "c_q3_2026_svc",     "usd",           "700000000",   "-6",
        "us-gaap_Revenues",                                      "c_q3_2026_prd",     "usd",           "300000000",   "-6",
        "us-gaap_Revenues",                                      "c_q3_2025_svc",     "usd",           "640000000",   "-6",
        "us-gaap_Revenues",                                      "c_q3_2025_prd",     "usd",           "260000000",   "-6",
        "us-gaap_Revenues",                                      "c_ytd_2026_svc",    "usd",           "2000000000",  "-6",   # YTD: excluded from 3M
        "us-gaap_Revenues",                                      "c_q3_2026_svc_wls", "usd",           "500000000",   "-6",   # two dims: ignored
        "us-gaap_CostOfGoodsAndServicesSold",                    "c_q3_2026_prd",     "usd",           "200000000",   "-6",
        "us-gaap_CostOfGoodsAndServicesSold",                    "c_q3_2025_prd",     "usd",           "180000000",   "-6",
        # --- balance sheet ---
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "c_i_2026_09_30",    "usd",           "200000000",   "-6",
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "c_i_2025_12_31",    "usd",           "150000000",   "-6",
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "c_i_2024_12_31",    "usd",           "120000000",   "-6",
        "us-gaap_AccountsReceivableNetCurrent",                  "c_i_2026_09_30",    "usd",           "300000000",   "-6",
        "us-gaap_AccountsReceivableNetCurrent",                  "c_i_2025_12_31",    "usd",           "250000000",   "-6",
        "us-gaap_Assets",                                        "c_i_2026_09_30",    "usd",           "500000000",   "-6",
        "us-gaap_Assets",                                        "c_i_2025_12_31",    "usd",           "400000000",   "-6",
        "us-gaap_Liabilities",                                   "c_i_2026_09_30",    "usd",           "300000000",   "-6",
        "us-gaap_Liabilities",                                   "c_i_2025_12_31",    "usd",           "240000000",   "-6",
        "us-gaap_StockholdersEquity",                            "c_i_2026_09_30",    "usd",           "200000000",   "-6",
        "us-gaap_StockholdersEquity",                            "c_i_2025_12_31",    "usd",           "160000000",   "-6",
        "us-gaap_LiabilitiesAndStockholdersEquity",              "c_i_2026_09_30",    "usd",           "500000000",   "-6",
        "us-gaap_LiabilitiesAndStockholdersEquity",              "c_i_2025_12_31",    "usd",           "400000000",   "-6",
        "us-gaap_NetIncomeLoss",                                 "c_ytd_2025",        "usd",           "1150000000", "-6",
        "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment",    "c_ytd_2026",        "usd",           "1250000000", "-6",
        "us-gaap_PaymentsToAcquirePropertyPlantAndEquipment",    "c_ytd_2025",        "usd",           "1090000000", "-6",
        "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",  "c_ytd_2026",        "usd",           "50000000", "-6",
        "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",  "c_ytd_2025",        "usd",           "60000000", "-6",
        "us-gaap_CashAndCashEquivalentsPeriodIncreaseDecrease",  "c_q3_2025",         "usd",           "20000000", "-6",    # 3M: excluded from YTD
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "c_i_2025_09_30",    "usd",           "180000000", "-6",
        "us-gaap_CashAndCashEquivalentsAtCarryingValue",         "c_i_2026_06_30",    "usd",           "180000000", "-6",   # Q3 start: not the YTD start
    )
}

make_xbrl <- function() {
    list(
        role         = fixture_roles(),
        element      = fixture_elements(),
        presentation = fixture_presentation(),
        label        = fixture_labels(),
        context      = fixture_contexts(),
        dimension    = fixture_dimensions(),
        fact         = fixture_facts()
    )
}

dup_facts <- tibble::tribble(
    ~elementId, ~startDate, ~endDate, ~fact, ~decimals,
    # precision: "2" beats "0"
    "us-gaap_EarningsPerShareBasic", "2026-07-01", "2026-09-30", "5", "0",
    "us-gaap_EarningsPerShareBasic", "2026-07-01", "2026-09-30", "4.5", "2",
    # INF beats any number
    "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic",
                                     "2026-07-01", "2026-09-30", "100000000", "-6",
    "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic",
                                     "2026-07-01", "2026-09-30", "100200000", "INF",
    # exact duplicate collapses to one
    "us-gaap_Revenues",              "2026-07-01", "2026-09-30", "100000000", "-6",
    "us-gaap_Revenues",              "2026-07-01", "2026-09-30", "100000000", "-6",
    # same element, different period: both kept
    "us-gaap_Revenues",              "2026-01-01", "2026-09-30", "290000000", "-6"
)

flatten_tree_elements <- c("us-gaap_Revenues", "acme_CostsAndExpensesAbstract",
                           "us-gaap_CostOfGoodsAndServicesSold",
                           "us-gaap_SellingGeneralAndAdministrativeExpense",
                           "us-gaap_CostsAndExpenses",
                           "us-gaap_OperatingIncomeLoss",
                           "us-gaap_InterestExpense", "us-gaap_NetIncomeLoss",
                           "us-gaap_EarningsPerShareBasic",
                           "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic")

member_rows_is <- function() {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    dates <- c("2026-09-30", "2025-09-30")
    get_member_rows(xbrl, tree, s, "3M", dates) |> dplyr::arrange(seq)
}

merge_members_is <- function() {
    xbrl <- make_xbrl()
    tree <- role_tree(xbrl, ROLE_IS)
    s    <- flatten_tree(tree)
    dates <- c("2026-09-30", "2025-09-30")
    w     <- widen(consolidated_values(xbrl, unique(s$elementId), "3M"), dates)
    m     <- get_member_rows(xbrl, tree, s, "3M", dates)
    merge_members(s, w, m)
}

format_statement_is <- function() {
    format_statement(merge_members_is(), role_tree(make_xbrl(), ROLE_IS))
}

balance_values_cf <- function() {
    xbrl <- make_xbrl()
    s    <- flatten_tree(role_tree(xbrl, ROLE_CF))
    v    <- consolidated_values(xbrl, unique(s$elementId), "YTD")
    balance_values(xbrl, s, v, c("2026-09-30", "2025-09-30"))
}
