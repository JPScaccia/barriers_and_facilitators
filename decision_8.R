

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


# BLOG 7 ------------------------------------------------------------------
# Who Actually Has the Power?
#
# Question:
# Who actually controls implementation?
#
# Analytic idea:
# Implementation papers often discuss front-line workers, but many decisions
# that shape implementation are controlled upstream: state agencies, hospitals,
# school districts, behavioral health systems, payers, regulators, budgets, and
# contracts. This script compares actor language with control/power language.

# BLOG 8 ------------------------------------------------------------------
# What Makes Research Useful for Decision-Makers?
#
# Introduces Decision-Useful Implementation Science.
#
# Four pillars:
# 1. Prioritization: What matters most?
# 2. Mechanism: Why does it matter?
# 3. Institutional leverage: Who can change it, and through what levers?
# 4. Action: What should happen next?
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

prioritization_pattern <- regex(
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

mechanism_pattern <- regex(
  paste(
    "\\bmechanism\\w*\\b",
    "\\bcausal\\w*\\b",
    "\\bcause\\w*\\b",
    "\\bpathway\\w*\\b",
    "\\bmediate\\w*\\b",
    "\\bmediation\\b",
    "\\bmoderate\\w*\\b",
    "\\bmoderation\\b",
    "\\bexplain\\w*\\b",
    "\\bexplanatory\\b",
    "\\bhow .*\\b(work\\w*|operate\\w*|influenc\\w*|affect\\w*|lead\\w*|contribut\\w*)\\b",
    "\\bwhy .*\\b(work\\w*|succeed\\w*|fail\\w*|influenc\\w*|affect\\w*)\\b",
    "\\bthrough which\\b",
    "\\bby which\\b",
    "\\bdue to\\b",
    "\\bbecause of\\b",
    "\\blead\\w* to\\b",
    "\\bresult\\w* in\\b",
    "\\bcontribut\\w* to\\b",
    "\\binfluenc\\w*\\b",
    "\\baffect\\w*\\b",
    "\\bimpact\\w*\\b",
    "\\beffect\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

success_outcome_pattern <- regex(
  paste(
    "\\bsuccess\\w*\\b",
    "\\beffective\\w*\\b",
    "\\beffectiveness\\b",
    "\\bimplementation outcome\\w*\\b",
    "\\badoption\\b",
    "\\buptake\\b",
    "\\bacceptability\\b",
    "\\bappropriateness\\b",
    "\\bfeasibility\\b",
    "\\bfidelity\\b",
    "\\breach\\b",
    "\\bpenetration\\b",
    "\\bsustainability\\b",
    "\\bscale-?up\\b",
    "\\bspread\\b",
    "\\bintegration\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

institutional_leverage_pattern <- regex(
  paste(
    "\\bauthorit\\w*\\b",
    "\\bcontrol\\w*\\b",
    "\\bgovern\\w*\\b",
    "\\bdecision-?making\\b",
    "\\bdecision making\\b",
    "\\bmandate\\w*\\b",
    "\\bapproval\\b",
    "\\bpermission\\b",
    "\\bdiscretion\\b",
    "\\bautonom\\w*\\b",
    "\\baccountab\\w*\\b",
    "\\bpolicy lever\\w*\\b",
    "\\blever\\w*\\b",
    "\\bresource allocation\\b",
    "\\bbudget author\\w*\\b",
    "\\bprocure\\w*\\b",
    "\\bpurchas\\w*\\b",
    "\\bcontract\\w*\\b",
    "\\bpayment\\w*\\b",
    "\\breimburse\\w*\\b",
    "\\bbilling\\b",
    "\\bfunding\\b",
    "\\bbudget\\w*\\b",
    "\\bregulat\\w*\\b",
    "\\bpolicy\\b",
    "\\bpolicies\\b",
    "\\blegislat\\w*\\b",
    "\\blicens\\w*\\b",
    "\\baccredit\\w*\\b",
    "\\bgovernance\\b",
    "\\bstate agenc\\w*\\b",
    "\\bpayer\\w*\\b",
    "\\binsurer\\w*\\b",
    "\\bhospital\\w*\\b",
    "\\bhealth system\\w*\\b",
    "\\bschool district\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

action_pattern <- regex(
  paste(
    "\\baction\\w*\\b",
    "\\bactionable\\b",
    "\\brecommend\\w*\\b",
    "\\bstrategy\\b",
    "\\bstrategies\\b",
    "\\bintervention\\w*\\b",
    "\\bsolution\\w*\\b",
    "\\broadmap\\w*\\b",
    "\\bnext step\\w*\\b",
    "\\bimplement\\w* strategy\\w*\\b",
    "\\badapt\\w*\\b",
    "\\bredesign\\w*\\b",
    "\\bworkflow redesign\\b",
    "\\bquality improvement\\b",
    "\\bscale-?up\\b",
    "\\bsustain\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

df_blog8 <- df |>
  filter(!is.na(pmid), !is.na(year)) |>
  mutate(
    pmid = as.character(pmid),
    year = as.integer(year),
    title = coalesce(title, ""),
    abstract = coalesce(abstract, ""),
    text = str_squish(paste(title, abstract, sep = ". ")),
    text_lower = str_to_lower(text),
    bf_language = str_detect(text_lower, bf_pattern),
    has_prioritization = str_detect(text_lower, prioritization_pattern),
    has_mechanism_language = str_detect(text_lower, mechanism_pattern),
    has_success_outcome_language = str_detect(text_lower, success_outcome_pattern),
    has_mechanism = has_mechanism_language & has_success_outcome_language,
    has_institutional_leverage = str_detect(text_lower, institutional_leverage_pattern),
    has_action = str_detect(text_lower, action_pattern),
    decision_usefulness_score =
      as.integer(has_prioritization) +
      as.integer(has_mechanism) +
      as.integer(has_institutional_leverage) +
      as.integer(has_action),
    decision_usefulness_group = case_when(
      decision_usefulness_score == 0 ~ "0 pillars",
      decision_usefulness_score == 1 ~ "1 pillar",
      decision_usefulness_score == 2 ~ "2 pillars",
      decision_usefulness_score == 3 ~ "3 pillars",
      decision_usefulness_score == 4 ~ "4 pillars"
    ),
    decision_usefulness_level = case_when(
      decision_usefulness_score <= 1 ~ "Low decision usefulness",
      decision_usefulness_score == 2 ~ "Partial decision usefulness",
      decision_usefulness_score == 3 ~ "Strong decision usefulness",
      TRUE ~ "Full four-pillar signal"
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

# 1. Headline summaries ----------------------------------------------------

blog8_summary <- df_blog8 |>
  summarise(
    n_bf_records = n(),
    pct_with_prioritization = mean(has_prioritization, na.rm = TRUE),
    pct_with_mechanism = mean(has_mechanism, na.rm = TRUE),
    pct_with_institutional_leverage = mean(has_institutional_leverage, na.rm = TRUE),
    pct_with_action = mean(has_action, na.rm = TRUE),
    mean_decision_usefulness_score = mean(decision_usefulness_score, na.rm = TRUE),
    pct_with_0_pillars = mean(decision_usefulness_score == 0, na.rm = TRUE),
    pct_with_1_or_more_pillars = mean(decision_usefulness_score >= 1, na.rm = TRUE),
    pct_with_2_or_more_pillars = mean(decision_usefulness_score >= 2, na.rm = TRUE),
    pct_with_3_or_more_pillars = mean(decision_usefulness_score >= 3, na.rm = TRUE),
    pct_with_all_4_pillars = mean(decision_usefulness_score == 4, na.rm = TRUE)
  )

blog8_pillar_summary <- df_blog8 |>
  summarise(
    Prioritization = mean(has_prioritization, na.rm = TRUE),
    Mechanism = mean(has_mechanism, na.rm = TRUE),
    `Institutional leverage` = mean(has_institutional_leverage, na.rm = TRUE),
    Action = mean(has_action, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "pillar",
    values_to = "share"
  ) |>
  mutate(
    pillar = factor(
      pillar,
      levels = c("Prioritization", "Mechanism", "Institutional leverage", "Action")
    )
  )

blog8_score_summary <- df_blog8 |>
  count(decision_usefulness_group, name = "n_records") |>
  mutate(
    share = n_records / sum(n_records),
    decision_usefulness_group = factor(
      decision_usefulness_group,
      levels = c("0 pillars", "1 pillar", "2 pillars", "3 pillars", "4 pillars")
    )
  ) |>
  arrange(decision_usefulness_group)

blog8_level_summary <- df_blog8 |>
  count(decision_usefulness_level, name = "n_records") |>
  mutate(
    share = n_records / sum(n_records),
    decision_usefulness_level = factor(
      decision_usefulness_level,
      levels = c(
        "Low decision usefulness",
        "Partial decision usefulness",
        "Strong decision usefulness",
        "Full four-pillar signal"
      )
    )
  ) |>
  arrange(decision_usefulness_level)

# 2. Trends over time ------------------------------------------------------

blog8_year <- df_blog8 |>
  group_by(year) |>
  summarise(
    n_records = n(),
    prioritization_share = mean(has_prioritization, na.rm = TRUE),
    mechanism_share = mean(has_mechanism, na.rm = TRUE),
    institutional_leverage_share = mean(has_institutional_leverage, na.rm = TRUE),
    action_share = mean(has_action, na.rm = TRUE),
    mean_decision_usefulness_score = mean(decision_usefulness_score, na.rm = TRUE),
    full_four_pillar_share = mean(decision_usefulness_score == 4, na.rm = TRUE),
    three_or_more_pillar_share = mean(decision_usefulness_score >= 3, na.rm = TRUE),
    zero_pillar_share = mean(decision_usefulness_score == 0, na.rm = TRUE),
    .groups = "drop"
  )

blog8_period <- df_blog8 |>
  group_by(period) |>
  summarise(
    n_records = n(),
    prioritization_share = mean(has_prioritization, na.rm = TRUE),
    mechanism_share = mean(has_mechanism, na.rm = TRUE),
    institutional_leverage_share = mean(has_institutional_leverage, na.rm = TRUE),
    action_share = mean(has_action, na.rm = TRUE),
    mean_decision_usefulness_score = mean(decision_usefulness_score, na.rm = TRUE),
    full_four_pillar_share = mean(decision_usefulness_score == 4, na.rm = TRUE),
    three_or_more_pillar_share = mean(decision_usefulness_score >= 3, na.rm = TRUE),
    zero_pillar_share = mean(decision_usefulness_score == 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")))

blog8_score_period <- df_blog8 |>
  count(period, decision_usefulness_group, name = "n_records") |>
  group_by(period) |>
  mutate(share = n_records / sum(n_records)) |>
  ungroup() |>
  mutate(
    period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")),
    decision_usefulness_group = factor(
      decision_usefulness_group,
      levels = c("0 pillars", "1 pillar", "2 pillars", "3 pillars", "4 pillars")
    )
  )

blog8_pillar_period <- df_blog8 |>
  group_by(period) |>
  summarise(
    Prioritization = mean(has_prioritization, na.rm = TRUE),
    Mechanism = mean(has_mechanism, na.rm = TRUE),
    `Institutional leverage` = mean(has_institutional_leverage, na.rm = TRUE),
    Action = mean(has_action, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -period,
    names_to = "pillar",
    values_to = "share"
  ) |>
  mutate(
    period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")),
    pillar = factor(
      pillar,
      levels = c("Prioritization", "Mechanism", "Institutional leverage", "Action")
    )
  )

# 3. Pillar combinations ---------------------------------------------------

blog8_combination_summary <- df_blog8 |>
  mutate(
    pillar_combination = pmap_chr(
      list(has_prioritization, has_mechanism, has_institutional_leverage, has_action),
      \(priority, mechanism, leverage, action) {
        present <- c(
          if (priority) "Priority",
          if (mechanism) "Mechanism",
          if (leverage) "Leverage",
          if (action) "Action"
        )
        if (length(present) == 0) "None" else paste(present, collapse = " + ")
      }
    )
  ) |>
  count(pillar_combination, name = "n_records") |>
  mutate(share = n_records / sum(n_records)) |>
  arrange(desc(n_records))

# 4. Visuals ---------------------------------------------------------------

p_pillar_prevalence <- blog8_pillar_summary |>
  ggplot(aes(x = share, y = fct_rev(pillar))) +
  geom_col(fill = pal("blue"), width = 0.68) +
  geom_text(
    aes(label = percent(share, accuracy = 0.1)),
    hjust = -0.12,
    size = 10,
    color = pal("ink")
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(blog8_pillar_summary$share, na.rm = TRUE) * 1.18)
  ) +
  labs(
    title = "The Four Pillars of Decision-Useful Implementation Science",
    subtitle = "Share of BF-language records with each decision-usefulness signal.",
    x = "Share of BF-language records",
    y = NULL,
    caption = "Pillars are detected from title and abstract language. A record can contain more than one pillar."
  ) +
  theme_bf_blog()

p_pillar_prevalence

p_score_distribution <- blog8_score_summary |>
  ggplot(aes(x = decision_usefulness_group, y = share)) +
  geom_col(fill = pal("teal"), width = 0.68) +
  geom_text(
    aes(label = percent(share, accuracy = 0.1)),
    vjust = -0.55,
    size = 10,
    color = pal("ink")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(blog8_score_summary$share, na.rm = TRUE) * 1.2)
  ) +
  labs(
    title = "How Decision-Useful Is the Literature?",
    subtitle = "Records receive one point each for prioritization, mechanism, institutional leverage, and action.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "The score is intentionally simple: 0 to 4 pillars detected in the title or abstract."
  ) +
  theme_bf_blog()

p_score_distribution

p_score_trend <- ggplot(blog8_year, aes(x = year, y = mean_decision_usefulness_score)) +
  geom_line(color = pal("blue"), linewidth = 1.15, lineend = "round") +
  geom_point(color = "black", size = 2) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1), limits = c(0, 4)) +
  labs(
    title = "Decision Usefulness Has to Be Built",
    subtitle = "Mean four-pillar decision-usefulness score over time.",
    x = NULL,
    y = "Mean decision-usefulness score",
    caption = "Records receive one point each for prioritization, mechanism, institutional leverage, and action."
  ) +
  theme_bf_blog()

p_score_trend

p_three_or_more_pillars_trend <- ggplot(blog8_year, aes(x = year, y = three_or_more_pillar_share)) +
  geom_line(color = pal("coral"), linewidth = 1.5, lineend = "round") +
  geom_point(color = "black", size = 2) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "How Often Do Papers Get Close?",
    subtitle = "Share of BF-language records with at least three of the four decision-usefulness pillars.",
    x = NULL,
    y = "Share with 3+ pillars",
    caption = "Three-pillar records may still be missing prioritization, mechanism, institutional leverage, or action."
  ) +
  theme_bf_blog()

p_three_or_more_pillars_trend

p_full_framework_trend <- blog8_period |>
  ggplot(aes(x = period, y = full_four_pillar_share, group = 1)) +
  geom_col(fill = pal("coral"), width = 0.62) +
  geom_line(color = pal("ink"), linewidth = 0.8) +
  geom_point(color = "black", size = 3) +
  geom_text(
    aes(label = percent(full_four_pillar_share, accuracy = 0.1)),
    vjust = -0.55,
    size = 10,
    color = pal("ink")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(blog8_period$full_four_pillar_share, na.rm = TRUE) * 1.25)
  ) +
  labs(
    title = "How Often Do Papers Contain All Four Pillars?",
    subtitle = "Share of BF-language records with prioritization, mechanism, institutional leverage, and action signals.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "A full four-pillar signal does not prove a paper is decision-useful, but it indicates that the abstract contains the ingredients decision-makers need."
  ) +
  theme_bf_blog()

p_full_framework_trend

p_pillar_period_heatmap <- blog8_pillar_period |>
  ggplot(aes(x = period, y = pillar, fill = share)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(label = percent(share, accuracy = 1)),
    size = 10,
    color = pal("ink")
  ) +
  scale_fill_gradient(
    low = "#F3F4F6",
    high = pal("blue"),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Which Pillars Are Becoming More Common?",
    subtitle = "Decision-usefulness signals by publication period.",
    x = NULL,
    y = NULL,
    fill = "Share",
    caption = "Each cell shows the share of BF-language records in that period with the pillar signal."
  ) +
  theme_bf_blog() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

p_pillar_period_heatmap

# 5. Candidate examples ----------------------------------------------------

blog8_full_four_pillar_examples <- df_blog8 |>
  filter(decision_usefulness_score == 4) |>
  mutate(example_text = str_squish(str_sub(text, 1, 750))) |>
  arrange(desc(year)) |>
  select(
    pmid,
    year,
    title,
    decision_usefulness_score,
    example_text
  ) |>
  slice_head(n = 25)

blog8_three_pillar_examples <- df_blog8 |>
  filter(decision_usefulness_score == 3) |>
  mutate(
    missing_pillar = case_when(
      !has_prioritization ~ "Prioritization",
      !has_mechanism ~ "Mechanism",
      !has_institutional_leverage ~ "Institutional leverage",
      !has_action ~ "Action"
    ),
    example_text = str_squish(str_sub(text, 1, 750))
  ) |>
  arrange(desc(year)) |>
  select(
    pmid,
    year,
    title,
    missing_pillar,
    decision_usefulness_score,
    example_text
  ) |>
  slice_head(n = 30)

blog8_low_score_examples <- df_blog8 |>
  filter(decision_usefulness_score <= 1) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(
    pmid,
    year,
    title,
    decision_usefulness_score,
    example_text
  ) |>
  slice_head(n = 25)

toc()
beep(8)

# 6. Compact tables for writing -------------------------------------------

blog8_summary |> print(width = Inf)
blog8_pillar_summary |> print(width = Inf)
blog8_score_summary |> print(width = Inf)
blog8_level_summary |> print(width = Inf)
blog8_period |> print(width = Inf)
blog8_combination_summary |> print(n = 25, width = Inf)
blog8_year |> print(n = 60, width = Inf)
blog8_full_four_pillar_examples |> print(n = 25, width = Inf)
blog8_three_pillar_examples |> print(n = 30, width = Inf)
blog8_low_score_examples |> print(n = 25, width = Inf)


