# xbrlstatements

Rebuild financial statements from SEC XBRL filings, using the company's own
presentation linkbase so line order, indentation, labels and signs match the
filed statement.

```
XBRL         parse the instance and linkbases
secfilings   fetch filings from EDGAR
xbrlstatements  turn a parsed filing into statements <- this package
```

## Installation

```r
# install.packages("remotes")
remotes::install_github("jbennett7/XBRL")          # >= 1.1.0
remotes::install_github("jbennett7/secfilings")
remotes::install_github("jbennett7/xbrlstatements")
```

## Usage

```r
library(xbrlstatements)
library(secfilings)
library(XBRL)
library(dplyr)

useragent <- Sys.getenv("SEC_USERAGENT")   # "Name email@domain.com", required by the SEC
cik <- secfilings::get_cik('T', useragent)  # example get cik for AT&T
filings <- secfilings::get_submissions(cik, useragent) |>
    dplyr::filter(
        grepl("^10-[KQ](?:\\/A)?$", form),  # Download 10-K and 10-Q including amended filings
        isXBRL == 1) |>
    dplyr::arrange(dplyr::desc(reportDate))

accession <- filings$accessionNumber[1]   # pull the latest filing
form <- filings$form[1]   # and the form type

xbrl <- process_filing(cik, accession, useragent, cache = "~/.cache/sec")

is <- build_statement(xbrl, "income", form)
format_statement(is$data, is$tree)
```

An example display

```
  display_label                 `2026-09-30` `2025-09-30`
1 Operating Revenues            ""           ""
2     Service                   "26,301"     "25,336"
3     Equipment                 "6,021"      "5,373"
4   Total operating revenues    "32,322"     "30,709"
...
15  Interest expense            "(1,979)"    "(1,700)"
```

`build_statement()` returns a list

| element | contents |
|---|---|
| `data` | one row per line item: `seq`, `elementId`, `depth`, `negated`, `label`, `preferredLabel`, one numeric column per date, `is_member` |
| `tree` | the presentation tree for the statement's role |
| `kind`, `period` | what was built; `period` is the period actually used (`3M` for a Q1 cash flow) |

Values in `data` are **as filed**. `format_statement()` applies the
`negatedLabel` sign flip, scales to millions (by default), and formats EPS to 2
decimals.

## Statements

| kind | 10-Q period | 10-K period |
|---|---|---|
| `income` | 3 months | fiscal year |
| `balance` | instant | instant |
| `cash_flow` | year to date (3 months in Q1) | fiscal year |

The statement role is found by its title (for example "Statements of Income",
"Operations" or "Earnings"); parenthetical roles are excluded.

## How it works

- **Presentation tree:** hypercube, axis and domain nodes are traversed but not shown.
- **Facts:** consolidated facts only (no dimensions); when the same fact is
  filed at different precisions, the most precise one wins.
- **Members:** single-dimension facts for members shown in the tree (e.g.
  Service / Equipment) become indented rows; a line with no consolidated value
  is replaced by its members.
- **Dates:** the two dates with the most facts, newest first.
- **Balances in duration statements:** opening and closing balances (e.g. cash
  on the cash flow statement) come from instant facts: the start row uses the
  day before the period starts, the end row uses the column date.

## Limitations

- Members from contexts with more than one dimension are ignored.
- Only the latest two periods are returned.
- Signs follow the filer's `negatedLabel` roles. A line the filer labeled
  incorrectly shows the wrong sign (e.g. AT&T's 2026 Q3 short-term debt
  repayments); the calculation linkbase is not used to correct it.
- The statement of stockholders' equity is not supported yet. `kind = "equity"`
  does not work.

## Tests

```r
devtools::test()
```

Unit tests run against a small hand-built filing (`tests/testthat/helper-fixtures.R`);
regression snapshots compare against a trimmed AT&T 10-Q. The script in
`data-raw/att_2026q3.R` rebuilds the AT&T fixture.
