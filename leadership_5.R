

#blog 5, what about leadership


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

# BLOG 5 ------------------------------------------------------------------
# Leadership Isn't a Mechanism
#
# Question:
# What causes implementation to succeed?
#
# Analytic idea:
# Leadership, communication, and relationships are often named as ingredients.
# But naming an ingredient is description. A mechanism explains how or why that
# ingredient changes implementation outcomes.

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

enumeration_pattern <- regex(
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
    "\\bperceptions?\\b",
    "\\bperspectives?\\b",
    "\\bexperiences?\\b",
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
    "\\boutcome\\w*\\b",
    "\\bimplementation outcome\\w*\\b",
    "\\badoption\\b",
    "\\bacceptability\\b",
    "\\bappropriateness\\b",
    "\\bfeasibility\\b",
    "\\bfidelity\\b",
    "\\bpenetration\\b",
    "\\bsustainability\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

success_pattern <- regex(
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

ingredient_dictionary <- tribble(
  ~ingredient, ~pattern,
  "Leadership", "\\bleader\\w*\\b|\\bchampion\\w*\\b|\\bmanagement\\b|\\bmanager\\w*\\b|\\bexecutive\\w*\\b|\\bgovernance\\b",
  "Communication", "\\bcommunicat\\w*\\b|\\binformation\\b|\\bfeedback\\b|\\bcoordination\\b|\\bcollaboration\\b|\\bengagement\\b",
  "Relationships", "\\brelationship\\w*\\b|\\brelational\\b|\\btrust\\b|\\bteamwork\\b|\\bpartnership\\w*\\b|\\bnetwork\\w*\\b|\\bsocial support\\b"
) |>
  mutate(pattern = map(pattern, regex, ignore_case = TRUE))

df_blog5 <- df |>
  filter(!is.na(pmid), !is.na(year)) |>
  mutate(
    pmid = as.character(pmid),
    year = as.integer(year),
    title = coalesce(title, ""),
    abstract = coalesce(abstract, ""),
    text = str_squish(paste(title, abstract, sep = ". ")),
    text_lower = str_to_lower(text),
    bf_language = str_detect(text_lower, bf_pattern),
    enumeration_language = str_detect(text_lower, enumeration_pattern),
    mechanism_language = str_detect(text_lower, mechanism_pattern),
    success_language = str_detect(text_lower, success_pattern),
    mechanism_success_language = mechanism_language & success_language,
    analysis_status = case_when(
      enumeration_language & mechanism_success_language ~ "Enumeration plus mechanism",
      enumeration_language & !mechanism_success_language ~ "Enumeration only",
      !enumeration_language & mechanism_success_language ~ "Mechanism without enumeration",
      TRUE ~ "Neither enumeration nor mechanism"
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

# 1. Detect the familiar ingredients --------------------------------------

ingredient_flags <- ingredient_dictionary |>
  mutate(value = map(pattern, \(pat) str_detect(df_blog5$text_lower, pat))) |>
  select(ingredient, value) |>
  unnest_longer(value, indices_to = "row_id")

blog5_ingredients <- df_blog5 |>
  mutate(row_id = row_number()) |>
  left_join(ingredient_flags, by = "row_id") |>
  filter(value) |>
  select(-value)

ingredient_count_by_record <- blog5_ingredients |>
  distinct(pmid, ingredient) |>
  count(pmid, name = "n_familiar_ingredients")

df_blog5 <- df_blog5 |>
  left_join(ingredient_count_by_record, by = "pmid") |>
  mutate(
    n_familiar_ingredients = replace_na(n_familiar_ingredients, 0L),
    names_familiar_ingredient = n_familiar_ingredients > 0,
    familiar_ingredient_without_mechanism = names_familiar_ingredient & !mechanism_success_language,
    familiar_ingredient_with_mechanism = names_familiar_ingredient & mechanism_success_language
  )

# 2. Headline percentages --------------------------------------------------

blog5_summary <- df_blog5 |>
  summarise(
    n_bf_records = n(),
    pct_with_enumeration_language = mean(enumeration_language, na.rm = TRUE),
    pct_with_mechanism_language = mean(mechanism_language, na.rm = TRUE),
    pct_with_mechanism_success_language = mean(mechanism_success_language, na.rm = TRUE),
    pct_with_familiar_ingredient = mean(names_familiar_ingredient, na.rm = TRUE),
    pct_familiar_ingredient_without_mechanism = mean(familiar_ingredient_without_mechanism, na.rm = TRUE),
    pct_familiar_ingredient_with_mechanism = mean(familiar_ingredient_with_mechanism, na.rm = TRUE)
  )

blog5_status_summary <- df_blog5 |>
  count(analysis_status, name = "n_records") |>
  mutate(
    share = n_records / sum(n_records),
    analysis_status = factor(
      analysis_status,
      levels = c(
        "Enumeration only",
        "Enumeration plus mechanism",
        "Mechanism without enumeration",
        "Neither enumeration nor mechanism"
      )
    )
  ) |>
  arrange(analysis_status)

blog5_ingredient_summary <- blog5_ingredients |>
  group_by(ingredient) |>
  summarise(
    n_records = n_distinct(pmid),
    pct_all_bf_records = n_records / n_distinct(df_blog5$pmid),
    n_with_mechanism_success_language = n_distinct(pmid[mechanism_success_language]),
    n_without_mechanism_success_language = n_distinct(pmid[!mechanism_success_language]),
    mechanism_success_share_within_ingredient = n_with_mechanism_success_language / n_records,
    .groups = "drop"
  ) |>
  arrange(desc(n_records))

# 3. Trends over time ------------------------------------------------------

blog5_year <- df_blog5 |>
  group_by(year) |>
  summarise(
    n_records = n(),
    enumeration_share = mean(enumeration_language, na.rm = TRUE),
    mechanism_share = mean(mechanism_language, na.rm = TRUE),
    mechanism_success_share = mean(mechanism_success_language, na.rm = TRUE),
    familiar_ingredient_share = mean(names_familiar_ingredient, na.rm = TRUE),
    familiar_without_mechanism_share = mean(familiar_ingredient_without_mechanism, na.rm = TRUE),
    familiar_with_mechanism_share = mean(familiar_ingredient_with_mechanism, na.rm = TRUE),
    .groups = "drop"
  )

blog5_period <- df_blog5 |>
  group_by(period) |>
  summarise(
    n_records = n(),
    enumeration_share = mean(enumeration_language, na.rm = TRUE),
    mechanism_success_share = mean(mechanism_success_language, na.rm = TRUE),
    familiar_ingredient_share = mean(names_familiar_ingredient, na.rm = TRUE),
    familiar_without_mechanism_share = mean(familiar_ingredient_without_mechanism, na.rm = TRUE),
    familiar_with_mechanism_share = mean(familiar_ingredient_with_mechanism, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")))

blog5_status_period <- df_blog5 |>
  count(period, analysis_status, name = "n_records") |>
  group_by(period) |>
  mutate(share = n_records / sum(n_records)) |>
  ungroup() |>
  mutate(
    period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")),
    analysis_status = factor(
      analysis_status,
      levels = c(
        "Enumeration only",
        "Enumeration plus mechanism",
        "Mechanism without enumeration",
        "Neither enumeration nor mechanism"
      )
    )
  )

# 4. Visuals ---------------------------------------------------------------

p_enum_vs_mechanism_trend <- ggplot(blog5_year, aes(x = year)) +
  geom_line(
    aes(y = enumeration_share, color = "Enumeration language"),
    linewidth = 1.15,
    lineend = "round"
  ) +
  geom_line(
    aes(y = mechanism_success_share, color = "Mechanism + success language"),
    linewidth = 1.15,
    lineend = "round"
  ) +
  geom_line(
    aes(y = familiar_without_mechanism_share, color = "Familiar ingredient, no mechanism"),
    linewidth = 1.15,
    lineend = "round"
  ) +
  scale_color_manual(
    values = c(
      "Enumeration language" = pal("blue"),
      "Mechanism + success language" = pal("teal"),
      "Familiar ingredient, no mechanism" = pal("coral")
    )
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Leadership Isn't a Mechanism",
    subtitle = "Naming ingredients is more common than explaining how they produce implementation success.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Mechanism + success language requires both mechanism terms and implementation success/outcome terms in the title or abstract."
  ) +
  theme_bf_blog()

p_enum_vs_mechanism_trend

p_ingredient_mechanism_gap <- blog5_ingredient_summary |>
  mutate(ingredient = fct_reorder(ingredient, pct_all_bf_records)) |>
  ggplot(aes(x = pct_all_bf_records, y = ingredient)) +
  geom_col(fill = pal("blue"), width = 0.68) +
  geom_point(
    aes(x = mechanism_success_share_within_ingredient),
    color = pal("coral"),
    size = 3
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Ingredients Are Easy to Name",
    subtitle = "Bars show ingredient prevalence. Dots show the share with mechanism + success language.",
    x = "Share of BF-language records",
    y = NULL,
    caption = "Leadership, communication, and relationships are treated as named ingredients, not mechanisms by themselves."
  ) +
  theme_bf_blog()

p_ingredient_mechanism_gap

p_status_period <- blog5_status_period |>
  ggplot(aes(x = period, y = share, fill = analysis_status)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68) +
  scale_fill_manual(
    values = c(
      "Enumeration only" = pal("coral"),
      "Enumeration plus mechanism" = pal("teal"),
      "Mechanism without enumeration" = pal("gold"),
      "Neither enumeration nor mechanism" = pal("light_gray")
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Description and Mechanism Are Different Claims",
    subtitle = "Enumeration names what appeared. Mechanism language tries to explain how or why outcomes happen.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Shares sum to 100% within each period."
  ) +
  theme_bf_blog()

p_status_period

p_familiar_without_mechanism <- blog5_period |>
  ggplot(aes(x = period, y = familiar_without_mechanism_share, group = 1)) +
  geom_col(fill = pal("coral"), width = 0.62) +
  geom_line(color = pal("ink"), linewidth = 0.8) +
  geom_point(color = pal("ink"), size = 2.8) +
  geom_text(
    aes(label = percent(familiar_without_mechanism_share, accuracy = 0.1)),
    vjust = -0.55,
    size = 4.2,
    color = pal("ink")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(blog5_period$familiar_without_mechanism_share, na.rm = TRUE) * 1.22)
  ) +
  labs(
    title = "How Often Do Papers Name Ingredients Without Mechanisms?",
    subtitle = "Share of BF-language records naming leadership, communication, or relationships without mechanism + success language.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "A named ingredient is not counted as a mechanism unless the record also includes mechanism and implementation success/outcome language."
  ) +
  theme_bf_blog()

p_familiar_without_mechanism

# 5. Candidate examples ----------------------------------------------------

blog5_enumeration_only_examples <- df_blog5 |>
  filter(enumeration_language, names_familiar_ingredient, !mechanism_success_language) |>
  mutate(
    matched_ingredients = map_chr(pmid, \(id) {
      blog5_ingredients |>
        filter(pmid == id) |>
        distinct(ingredient) |>
        arrange(ingredient) |>
        pull(ingredient) |>
        paste(collapse = ", ")
    }),
    example_text = str_squish(str_sub(text, 1, 700))
  ) |>
  arrange(desc(year), desc(n_familiar_ingredients)) |>
  select(pmid, year, title, n_familiar_ingredients, matched_ingredients, example_text) |>
  slice_head(n = 20)

blog5_mechanism_examples <- df_blog5 |>
  filter(names_familiar_ingredient, mechanism_success_language) |>
  mutate(
    matched_ingredients = map_chr(pmid, \(id) {
      blog5_ingredients |>
        filter(pmid == id) |>
        distinct(ingredient) |>
        arrange(ingredient) |>
        pull(ingredient) |>
        paste(collapse = ", ")
    }),
    example_text = str_squish(str_sub(text, 1, 700))
  ) |>
  arrange(desc(year), desc(n_familiar_ingredients)) |>
  select(pmid, year, title, n_familiar_ingredients, matched_ingredients, example_text) |>
  slice_head(n = 20)

# 6. Compact tables for writing -------------------------------------------

blog5_summary
blog5_status_summary
blog5_ingredient_summary
blog5_period
blog5_year |> print(n = 60)
blog5_enumeration_only_examples |> print(n = 20, width = Inf)
blog5_mechanism_examples |> print(n = 20, width = Inf)


