

################################################ blog 1


library(data.table)
library(tidyverse)
library(janitor)
library(purrr)
library(beepr)
library(extrafont)
library(broom)
library(quanteda)
library(quanteda.textstats)


df <- fread("C:\\Users\\jonat\\Desktop\\barriers_big.csv") |> as_tibble() |>
  clean_names()

#Reusable blog palette ---------------------------------------------------
  
  bf_pal <- c(
    blue = "#2563EB",
    teal = "#0F766E",
    gold = "#D97706",
    coral = "#DC2626",
    ink = "#111827",
    gray = "#6B7280",
    light_gray = "#E5E7EB",
    bg = "#FFFFFF"
  )

# Reusable blog theme -----------------------------------------------------

theme_bf_blog <- function(base_size = 14, base_family = "Segoe UI") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(plot.background = element_rect(fill = bf_pal["bg"], color = NA),
          panel.background = element_rect(fill = bf_pal["bg"], color = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = bf_pal["light_gray"], linewidth = 0.35),
      plot.title = element_text(face = "bold",
                                size = base_size + 5,
                                color = "black",
                                margin = margin(b = 6)),
      plot.subtitle = element_text(size = base_size,
                                   color = "black",
                                   margin = margin(b = 14)),
      plot.caption = element_text(size = base_size - 3,
                                  color = "black",
                                  hjust = 0,
                                  margin = margin(t = 10)),
      axis.title.y = element_text(size = base_size - 1,
                                  color = bf_pal["ink"],
                                  margin = margin(r = 8)),
      axis.text = element_text(size = base_size - 2,
                               color = bf_pal["ink"]),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1, color = bf_pal["ink"]),
      plot.margin = margin(12, 18, 12, 12))
  }


#####################################################################

bf_pattern <- regex(
  paste(
    "\\bbarrier\\w*\\b",
    "\\bfacilitat\\w*\\b",
    "\\bobstacle\\w*\\b",
    "\\bchallenge\\w*\\b",
    "\\benabler\\w*\\b",
    "\\bdriver\\w*\\b",
    "\\bdeterminant\\w*\\b",
    "\\bconstraint\\w*\\b",
    "\\bbottleneck\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

df_blog2 <- df |>
  filter(!is.na(pmid), !is.na(year)) |>
  mutate(
    pmid = as.character(pmid),
    year = as.integer(year),
    text = str_to_lower(str_squish(paste(title, abstract, sep = " "))),
    bf_language = str_detect(text, bf_pattern),
    period = case_when(
      year <= 1999 ~ "1980-1999",
      year <= 2009 ~ "2000-2009",
      year <= 2019 ~ "2010-2019",
      TRUE ~ "2020-2025"
    )
  ) |>
  distinct(pmid, .keep_all = TRUE) |>
  filter(year >= 1980,
         year <= 2025,
         bf_language,
         !is.na(text),
         nchar(text) > 50)


# 1. Settings


sample_per_year_lexdiv <- 500
n_reps <- 10
mattr_window <- 25

sample_per_year_vocab <- 500
top_n <- 10

# I had to play with sample size and rep because this took a long time to run: 
# a few hours over the weekend. 

set.seed(1701)


custom_stopwords <- c(
  "study", "studies", "result", "results", "method", "methods",
  "conclusion", "conclusions", "background", "objective", "objectives",
  "aim", "aims", "data", "analysis", "participants", "using",
  "among", "across", "based", "however"
)

sample_by_year <- function(dat, sample_per_year) {
  dat |>
    group_by(year) |>
    group_modify(\(.x, .y) {
      slice_sample(.x, n = min(nrow(.x), sample_per_year))
    }) |>
    ungroup()
}

tokenize_blog2 <- function(dat) {
  corp <- corpus(dat, text_field = "text")
  
  toks <- tokens(
    corp,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE
  ) |>
    tokens_tolower() |>
    tokens_remove(stopwords("en")) |>
    tokens_remove(custom_stopwords) |>
    tokens_keep(min_nchar = 3)
  
  docvars(toks, "year") <- dat$year
  
  if ("period" %in% names(dat)) {
    docvars(toks, "period") <- dat$period
  }
  
  toks
}


# 2. Lexical diversity: repeated within-year samples


run_lexdiv_rep <- function(rep_id, dat, sample_per_year, mattr_window = 25) {
  message("Running lexical diversity replicate ", rep_id, " of ", n_reps)
  t0 <- Sys.time()
  
  dat_samp <- sample_by_year(dat, sample_per_year)
  message("  sampled: ", round(difftime(Sys.time(), t0, units = "mins"), 2), " min")
  
  toks <- tokenize_blog2(dat_samp)
  message("  tokenized: ", round(difftime(Sys.time(), t0, units = "mins"), 2), " min")
  
  # Keep only documents with enough tokens for a stable MATTR window
  keep <- ntoken(toks) >= mattr_window
  
  toks <- toks[keep]
  message("  kept ", ndoc(toks), " docs with at least ", mattr_window, " tokens")
  
  lex_doc <- textstat_lexdiv(
    toks,
    measure = c("TTR", "MATTR"),
    MATTR_window = mattr_window
  ) |>
    as_tibble() |>
    mutate(year = docvars(toks, "year"))
  
  message("  lexdiv done: ", round(difftime(Sys.time(), t0, units = "mins"), 2), " min")
  
  out <- lex_doc |>
    group_by(year) |>
    summarise(
      rep = rep_id,
      n_documents = n(),
      mean_ttr = mean(TTR, na.rm = TRUE),
      mean_mattr = mean(MATTR, na.rm = TRUE),
      .groups = "drop"
    )
  
  rm(dat_samp, toks, lex_doc)
  gc()
  
  out
}

sample_per_year_lexdiv <- 500
n_reps <- 10
mattr_window <- 25

df_blog2_fast <- df_blog2 |>
  mutate(text = str_sub(text, 1, 5000)) |>
  filter(nchar(text) > 50)

set.seed(1701)

lexdiv_rep_year <- map_dfr(
  seq_len(n_reps),
  \(r) run_lexdiv_rep(
    rep_id = r,
    dat = df_blog2_fast,
    sample_per_year = sample_per_year_lexdiv,
    mattr_window = mattr_window
  )
)

lexdiv_rep_year <- map_dfr(
  seq_len(n_reps),
  \(r) run_lexdiv_rep(r, df_blog2_fast, sample_per_year_lexdiv)
)

lexdiv_year <- lexdiv_rep_year |>
  group_by(year) |>
  summarise(
    reps = n(),
    mean_documents = mean(n_documents),
    mean_ttr = mean(mean_ttr, na.rm = TRUE),
    mean_mattr = mean(mean_mattr, na.rm = TRUE),
    mattr_low = quantile(mean_mattr, 0.025, na.rm = TRUE),
    mattr_high = quantile(mean_mattr, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

p_lexdiv <- ggplot(lexdiv_year, aes(x = year, y = mean_mattr)) +
  geom_ribbon(
    aes(ymin = mattr_low, ymax = mattr_high),
    fill = bf_pal["blue"],
    alpha = 0.15
  ) +
  geom_line(color = bf_pal["blue"], linewidth = 1.15, lineend = "round") +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = bf_pal["ink"],
    linewidth = 0.8,
    linetype = "longdash"
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title = "The Literature Is Growing. Is Its Vocabulary?",
    subtitle = "Mean MATTR across repeated within-year samples.",
    x = NULL,
    y = "Mean MATTR",
    caption = paste0(
      "Each year sampled up to ", comma(sample_per_year_lexdiv),
      " records per replicate; ", n_reps,
      " replicates. MATTR window = ", mattr_window,
      ". Documents with fewer than ", mattr_window,
      " tokens after preprocessing excluded."
    )
  ) +
  theme_bf_blog()

p_lexdiv

# -------------------------------------------------------------------------
# 3. Vocabulary sample for entropy and concentration
# -------------------------------------------------------------------------

set.seed(1702)

df_blog2_vocab_sample <- sample_by_year(df_blog2, sample_per_year_vocab)
toks_vocab <- tokenize_blog2(df_blog2_vocab_sample)

dfm_year <- toks_vocab |>
  dfm() |>
  dfm_group(groups = year)

calc_entropy <- function(x) {
  p <- x / sum(x)
  p <- p[p > 0]
  -sum(p * log(p))
}

entropy_year <- tibble(
  year = as.integer(docnames(dfm_year)),
  n_tokens = ntoken(dfm_year),
  n_types = Matrix::rowSums(dfm_year > 0),
  entropy = map_dbl(seq_len(ndoc(dfm_year)), \(i) {
    calc_entropy(as.numeric(dfm_year[i, ]))
  })
) |>
  mutate(normalized_entropy = entropy / log(n_types))

p_entropy <- ggplot(entropy_year, aes(x = year, y = normalized_entropy)) +
  geom_line(color = bf_pal["teal"], linewidth = 1.15, lineend = "round") +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = bf_pal["ink"],
    linewidth = 0.8,
    linetype = "longdash"
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = number_format(accuracy = 0.001)) +
  labs(
    title = "Vocabulary Evenness Has Not Kept Pace With Volume",
    subtitle = "Normalized Shannon entropy of BF-language abstracts by year.",
    x = NULL,
    y = "Normalized entropy",
    caption = paste0(
      "Each year sampled up to ", comma(sample_per_year_vocab),
      " records. Higher values indicate a more even distribution of terms.")) +
  theme_bf_blog()

p_entropy

# 4. Top-term concentration


top_term_concentration <- tibble(
  year = as.integer(docnames(dfm_year)),
  top_25_share = map_dbl(seq_len(ndoc(dfm_year)), \(i) {
    counts <- as.numeric(dfm_year[i, ])
    counts <- sort(counts[counts > 0], decreasing = TRUE)
    sum(head(counts, top_n)) / sum(counts)
  })
)

p_top_concentration <- ggplot(top_term_concentration, aes(x = year, y = top_25_share)) +
  geom_line(color = bf_pal["gold"], linewidth = 1.15, lineend = "round") +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = bf_pal["ink"],
    linewidth = 0.8,
    linetype = "longdash"
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "A Small Vocabulary Does a Lot of the Work",
    subtitle = paste0("Share of yearly tokens accounted for by the ", top_n, " most frequent terms."),
    x = NULL,
    y = paste0("Share of tokens in top ", top_n, " terms"),
    caption = "Higher values indicate greater vocabulary concentration."
  ) +
  theme_bf_blog()

p_top_concentration

blog2_summary <- lexdiv_year |>
  left_join(entropy_year, by = "year") |>
  left_join(top_term_concentration, by = "year") |>
  filter(year %in% c(min(year), max(year))) |>
  select(
    year,
    mean_documents,
    mean_mattr,
    mattr_low,
    mattr_high,
    normalized_entropy,
    top_25_share)


top_terms_by_period <- toks_vocab |>
  dfm() |>
  dfm_group(groups = period) |>
  textstat_frequency(n = 20, groups = period)

top_terms_by_period |> print(n = 80)

blog2_summary
top_terms_by_period
lexdiv_year 
entropy_year |> print(n = 80)
top_term_concentration |> print(n = 80)