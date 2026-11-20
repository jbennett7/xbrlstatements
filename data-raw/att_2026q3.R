library(dplyr)
devtools::load_all('.')
full <- process_filing("0000732717", "0000732717-26-000372", Sys.getenv("SEC_USERAGENT"))

roles <- vapply(c("income", "balance", "cash_flow"), \(k) find_role(full, k), character(1))

pres <- full$presentation |> filter(roleId %in% roles)
ids  <- union(pres$fromElementId, pres$toElementId)
fact <- full$fact |> filter(elementId %in% ids)
att  <- list(
    role         = full$role,
    presentation = pres,
    element      = full$element   |> filter(elementId %in% ids),
    label        = full$label     |> filter(elementId %in% ids),
    fact         = fact,
    context      = full$context   |> filter(contextId %in% fact$contextId),
    dimension   = full$dimension |> filter(contextId %in% fact$contextId))
saveRDS(att, "tests/testthat/fixtures/att_2026q3.rds")
