############################################
# MEGA BARRIERS & FACILITATORS PUBMED PULL #
# Community / Public Health / Impl Science #
############################################

library(easyPubMed)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# ---------- 1) Barriers / Facilitators clause ----------
bf_clause <- paste0(
  "(",
  paste(
    c(
      "barrier*", "facilitat*", "obstacle*", "hindrance*", "challenge*",
      "enabler*", "driver*", "determinant*", "impediment*", "constraint*",
      "bottleneck*",
      "\"barriers and facilitators\"",
      "\"barrier and facilitator\"",
      "\"implementation barrier*\"",
      "\"implementation facilit*\"",
      "\"perceived barrier*\"",
      "\"perceived facilit*\""
    ) |>
      paste0("[All Fields]"),
    collapse = " OR "
  ),
  ")"
)

# ---------- 2) Context clause: public/community health + implementation ----------
context_clause <- paste0(
  "(",
  paste(
    c(
      "\"public health\"", "\"community health\"", "\"population health\"",
      "\"health promotion\"", "\"health education\"",
      "\"primary care\"", "\"community health worker*\"", "CHW",
      "\"health equity\"", "\"health disparities\"",
      "\"implementation science\"", "\"implementation research\"",
      "implement*", "adopt*", "uptake", "scale-up", "scaling",
      "disseminat*", "\"knowledge translation\"", "\"quality improvement\"",
      "\"evidence-based\"", "\"evidence based\"", "EBP",
      "\"program implementation\"", "\"service delivery\"",
      "clinic*", "\"community based\"", "\"community-based\"",
      "\"public health practice\""
    ) |>
      paste0("[All Fields]"),
    collapse = " OR "
  ),
  ")"
)

# ---------- 3) Base mega query ----------
base_query <- paste(bf_clause, context_clause, sep = " AND ")

# ---------- 4) Year-based chunking (safe for large corpora) ----------
start_year <- 1980
end_year   <- as.integer(format(Sys.Date(), "%Y"))
years <- start_year:end_year

query_by_year <- function(year) {
  paste0(
    "(", base_query, ") AND (",
    year, "[PDAT] : ", year, "[PDAT])"
  )
}

# ---------- 5) Pull one year ----------
pull_year <- function(y) {
  q <- query_by_year(y)
  message("Pulling year: ", y)
  
  epm <- epm_query(q)
  epm <- epm_fetch(epm, format = "xml")
  epm <- epm_parse(epm, compact_output = TRUE)
  
  Sys.sleep(0.34)  # polite to NCBI
  
  getEPMData(epm) |>
    as_tibble() |>
    mutate(query = q, year_chunk = y)
}

# ---------- 6) Execute full pull + deduplicate ----------
imp_database2 <- map_dfr(years, pull_year) |>
  rename_with(tolower) |>
  distinct(pmid, .keep_all = TRUE)

# ---------- 7) Timestamp ----------
today <- as.Date(Sys.time())
library(data.table)

#fwrite(imp_database2, "C:\\Users\\jonat\\Desktop\\barriers_big.csv")
