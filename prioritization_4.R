

# BLOG 4 ------------------------------------------------------------------
# The Missing Ingredient: Prioritization

library(data.table)
library(tidyverse)
library(janitor)
library(purrr)
library(beepr)
library(extrafont)
library(broom)
library(quanteda)
library(quanteda.textstats)
library(scales)
library(tictoc)

df <- fread("C:\\Users\\jonat\\Desktop\\barriers_big.csv") |> as_tibble() |>
  clean_names()

bf_pal <- c(blue = "#2563EB",  teal = "#0F766E",  gold = "#D97706",
            coral = "#DC2626", ink = "#111827",  gray = "#6B7280",
            light_gray = "#E5E7EB",   bg = "#FFFFFF")

pal <- function(color_name) {
  unname(bf_pal[color_name])
}

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


# BLOG 4 ------------------------------------------------------------------
# The Missing Ingredient: Prioritization
#
# Question:
# Do implementation papers actually prioritize barriers?
#
# Analytic idea:
# Implementation requires choices. Explicit prioritization language should name
# priority, rank, weighting, or relative importance. Ambient decision language
# gestures toward action or decisions without clearly prioritizing barriers.

tic()

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

list_pattern <- regex(
  paste(
    "\\bthemes?\\b",
    "\\bcodes?\\b",
    "\\bcategor\\w+\\b",
    "\\bfactors?\\b",
    "\\bdomains?\\b",
    "\\bdimensions?\\b",
    "\\bconstructs?\\b",
    "\\bidentified\\b",
    "\\bemerged\\b",
    "\\binterviews?\\b",
    "\\bfocus groups?\\b",
    "\\bqualitative\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

explicit_prioritization_pattern <- regex(
  paste(
    "\\bpriorit\\w+\\b",
    "\\brank\\w*\\b",
    "\\brating\\w*\\b",
    "\\brated\\b",
    "\\bscore\\w*\\b",
    "\\bscored\\b",
    "\\bweight\\w*\\b",
    "\\bweigh\\w*\\b",
    "\\brelative importance\\b",
    "\\bimportance score\\w*\\b",
    "\\bimportance rating\\w*\\b",
    "\\bmost important\\b",
    "\\bleast important\\b",
    "\\btop priorit\\w*\\b",
    "\\bhighest priorit\\w*\\b",
    "\\blowest priorit\\w*\\b",
    "\\bnominal group\\b",
    "\\bdelphi\\b",
    "\\banalytic hierarchy process\\b",
    "\\bAHP\\b",
    "\\bbest-worst\\b",
    "\\bbest worst\\b",
    "\\bconjoint\\b",
    "\\bdiscrete choice\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)


ambient_decision_pattern <- regex(
  paste(
    "\\bdecision\\w*\\b",
    "\\bchoose\\w*\\b",
    "\\bchoice\\w*\\b",
    "\\bselect\\w*\\b",
    "\\baction\\w*\\b",
    "\\bactionable\\b",
    "\\bstrategy\\b",
    "\\bstrategies\\b",
    "\\brecommend\\w*\\b",
    "\\bleverage\\b",
    "\\bimpact\\b",
    "\\bfeasib\\w*\\b",
    "\\bacceptab\\w*\\b",
    "\\bcost\\w*\\b",
    "\\bcost-effect\\w*\\b",
    "\\btrade-?off\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

df_blog4 <- df |>
  filter(!is.na(pmid), !is.na(year)) |>
  mutate(
    pmid = as.character(pmid),
    year = as.integer(year),
    title = coalesce(title, ""),
    abstract = coalesce(abstract, ""),
    text = str_squish(paste(title, abstract, sep = ". ")),
    text_lower = str_to_lower(text),
    bf_language = str_detect(text_lower, bf_pattern),
    list_language = str_detect(text_lower, list_pattern),
    explicit_prioritization = str_detect(text_lower, explicit_prioritization_pattern),
    ambient_decision = str_detect(text_lower, ambient_decision_pattern),
    ambient_only = ambient_decision & !explicit_prioritization,
    any_decision_signal = explicit_prioritization | ambient_decision,
    no_decision_signal = !any_decision_signal,
    priority_status = case_when(
      explicit_prioritization ~ "Explicit prioritization",
      ambient_only ~ "Ambient decision language only",
      TRUE ~ "No decision signal"
    ),
    period = case_when(
      year <= 1999 ~ "1980-1999",
      year <= 2009 ~ "2000-2009",
      year <= 2019 ~ "2010-2019",
      TRUE ~ "2020-2025"
    )
  ) |>
  distinct(pmid, .keep_all = TRUE) |>
  filter(
    year >= 1980,
    year <= 2025,
    bf_language,
    !is.na(text),
    nchar(text) > 50
  )


# 1. Headline percentages --------------------------------------------------

blog4_summary <- df_blog4 |>
  summarise(
    n_bf_records = n(),
    pct_with_explicit_prioritization = mean(explicit_prioritization, na.rm = TRUE),
    pct_with_ambient_decision_language = mean(ambient_decision, na.rm = TRUE),
    pct_with_ambient_only = mean(ambient_only, na.rm = TRUE),
    pct_with_any_decision_signal = mean(any_decision_signal, na.rm = TRUE),
    pct_with_no_decision_signal = mean(no_decision_signal, na.rm = TRUE),
    pct_list_records_with_explicit_prioritization = mean(
      explicit_prioritization[list_language],
      na.rm = TRUE
    ),
    pct_list_records_with_no_decision_signal = mean(
      no_decision_signal[list_language],
      na.rm = TRUE
    )
  )

blog4_status_summary <- df_blog4 |>
  count(priority_status, name = "n_records") |>
  mutate(
    share = n_records / sum(n_records),
    priority_status = factor(
      priority_status,
      levels = c(
        "Explicit prioritization",
        "Ambient decision language only",
        "No decision signal"
      )
    )
  ) |>
  arrange(priority_status)

# 2. Trends over time ------------------------------------------------------

blog4_year <- df_blog4 |>
  group_by(year) |>
  summarise(
    n_records = n(),
    n_explicit_prioritization = sum(explicit_prioritization, na.rm = TRUE),
    n_ambient_decision = sum(ambient_decision, na.rm = TRUE),
    n_ambient_only = sum(ambient_only, na.rm = TRUE),
    n_any_decision_signal = sum(any_decision_signal, na.rm = TRUE),
    n_no_decision_signal = sum(no_decision_signal, na.rm = TRUE),
    explicit_prioritization_share = n_explicit_prioritization / n_records,
    ambient_decision_share = n_ambient_decision / n_records,
    ambient_only_share = n_ambient_only / n_records,
    any_decision_signal_share = n_any_decision_signal / n_records,
    no_decision_signal_share = n_no_decision_signal / n_records,
    .groups = "drop"
  )

blog4_period <- df_blog4 |>
  group_by(period) |>
  summarise(
    n_records = n(),
    n_explicit_prioritization = sum(explicit_prioritization, na.rm = TRUE),
    n_ambient_only = sum(ambient_only, na.rm = TRUE),
    n_no_decision_signal = sum(no_decision_signal, na.rm = TRUE),
    explicit_prioritization_share = n_explicit_prioritization / n_records,
    ambient_only_share = n_ambient_only / n_records,
    no_decision_signal_share = n_no_decision_signal / n_records,
    .groups = "drop"
  ) |>
  mutate(period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")))

# 3. Explicit vs ambient comparison ---------------------------------------

blog4_status_year <- df_blog4 |>
  count(year, priority_status, name = "n_records") |>
  group_by(year) |>
  mutate(share = n_records / sum(n_records)) |>
  ungroup() |>
  mutate(
    priority_status = factor(
      priority_status,
      levels = c(
        "Explicit prioritization",
        "Ambient decision language only",
        "No decision signal"
      )
    )
  )

blog4_status_period <- df_blog4 |>
  count(period, priority_status, name = "n_records") |>
  group_by(period) |>
  mutate(share = n_records / sum(n_records)) |>
  ungroup() |>
  mutate(
    period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")),
    priority_status = factor(
      priority_status,
      levels = c(
        "Explicit prioritization",
        "Ambient decision language only",
        "No decision signal"
      )
    )
  )
p_prioritization_trend <- ggplot(blog4_year, aes(x = year)) +
  geom_line(
    aes(y = explicit_prioritization_share, color = "Explicit prioritization"),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_line(
    aes(y = ambient_only_share, color = "Ambient decision language only"),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_line(
    aes(y = no_decision_signal_share, color = "No decision signal"),
    linewidth = 1.5,
    lineend = "round"
  ) +
  scale_color_manual(
    values = c(
      "Explicit prioritization" = pal("teal"),
      "Ambient decision language only" = pal("blue"),
      "No decision signal" = pal("coral")
    )
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(breaks = seq(0.1,0.8,0.1),
                     labels = percent_format(accuracy = 1)) +
  labs(
    title = "The Missing Ingredient: Prioritization",
    subtitle = "BF-language records are grouped by explicit prioritization, ambient decision language, or no decision signal.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Explicit prioritization includes priority, rank, weighting, relative importance, and prioritization methods. Ambient decision language gestures toward action without explicit prioritization."
  ) +
  theme_bf_blog()

p_prioritization_trend

p_explicit_vs_ambient_period <- blog4_status_period |>
  ggplot(aes(x = period, y = share, fill = priority_status)) +
  geom_col(position = position_dodge(width = 0.76), width = 0.68) +
  scale_fill_manual(
    values = c(
      "Explicit prioritization" = pal("teal"),
      "Ambient decision language only" = pal("blue"),
      "No decision signal" = pal("coral")
    )
  ) +
  scale_y_continuous(breaks = seq(0.1,0.8,0.1),
                     labels = percent_format(accuracy = 1)) +
  labs(
    title = "Explicit Prioritization Is Not the Same as Decision Talk",
    subtitle = "Dodged bars separate direct prioritization from more ambient action or decision language.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Periods are based on publication year. Shares sum to 100% within each period."
  ) +
  theme_bf_blog()

p_explicit_vs_ambient_period

p_prioritization_gap <- blog4_period |>
  ggplot(aes(x = period, y = explicit_prioritization_share, group = 1)) +
  geom_col(fill = pal("teal"), width = 0.62) +
  geom_line(color = pal("ink"), linewidth = 0.8) +
  geom_point(color = pal("ink"), size = 2.8) +
  geom_text(
    aes(label = percent(explicit_prioritization_share, accuracy = 0.1)),
    vjust = -0.55,
    size = 6,
    color = pal("ink")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(blog4_period$explicit_prioritization_share, na.rm = TRUE) * 1.22)
  ) +
  labs(
    title = "How Often Do Papers Actually Prioritize?",
    subtitle = "Share of BF-language records with explicit prioritization language by period.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Explicit prioritization includes priority, rank, weighting, relative importance, most/least important language, and common prioritization methods."
  ) +
  theme_bf_blog()

p_prioritization_gap

# 4. Candidate examples ----------------------------------------------------
# These are records with explicit prioritization language. Use them to inspect
# whether the abstract truly ranks or weights barriers, or merely uses the word.

blog4_explicit_examples <- df_blog4 |>
  filter(explicit_prioritization) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, example_text) |>
  slice_head(n = 20)

blog4_ambient_only_examples <- df_blog4 |>
  filter(ambient_only, list_language) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, example_text) |>
  slice_head(n = 20)

# 5. Compact tables for writing -------------------------------------------

blog4_summary
blog4_status_summary
blog4_period
blog4_year |> print(n = 60)
blog4_explicit_examples |> print(n = 20, width = Inf)
blog4_ambient_only_examples |> print(n = 20, width = Inf)

toc()
beep(8)

