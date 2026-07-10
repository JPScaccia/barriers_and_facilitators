

################################################ blog 1


library(data.table)
library(tidyverse)
library(janitor)
library(purrr)
library(beepr)
library(extrafont)
library(broom)


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
    theme(
      plot.background = element_rect(fill = bf_pal["bg"], color = NA),
      panel.background = element_rect(fill = bf_pal["bg"], color = NA),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = bf_pal["light_gray"], linewidth = 0.35),
      
      plot.title = element_text(
        face = "bold",
        size = base_size + 5,
        color = "black",
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = base_size,
        color = "black",
        margin = margin(b = 14)
      ),
      plot.caption = element_text(
        size = base_size - 3,
        color = "black",
        hjust = 0,
        margin = margin(t = 10)
      ),
      
      axis.title.y = element_text(
        size = base_size - 1,
        color = bf_pal["ink"],
        margin = margin(r = 8)
      ),
      axis.text = element_text(
        size = base_size - 2,
        color = bf_pal["ink"]
      ),
      axis.title.x = element_blank(),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1, color = bf_pal["ink"]),
      
      plot.margin = margin(12, 18, 12, 12)
    )
}

# Basic analytic frame ----------------------------------------------------

df_blog1 <- df |>
  filter(!is.na(pmid), !is.na(year)) |>
  mutate(
    pmid = as.character(pmid),
    year = as.integer(year),
    text = str_to_lower(str_squish(paste(title, abstract, sep = " ")))
  ) |>
  distinct(pmid, .keep_all = TRUE) |>
  filter(year >= 1980, year <= 2025)

# Barrier/facilitator vocabulary -----------------------------------------

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

explicit_bf_pattern <- regex(
  "\\bbarriers\\s+and\\s+facilitators\\b|\\bbarrier\\s+and\\s+facilitator\\b",
  ignore_case = TRUE
)

df_blog1 <- df_blog1 |>
  mutate(
    bf_language = str_detect(text, bf_pattern),
    explicit_bf = str_detect(text, explicit_bf_pattern)
  )

# Scope of corpus ---------------------------------------------------------

corpus_scope <- df_blog1 |>
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    total_publications = n(),
    publications_with_bf_language = sum(bf_language, na.rm = TRUE),
    publications_with_explicit_bf_phrase = sum(explicit_bf, na.rm = TRUE),
    pct_with_bf_language = mean(bf_language, na.rm = TRUE) * 100,
    pct_with_explicit_bf_phrase = mean(explicit_bf, na.rm = TRUE) * 100
  )

print(corpus_scope)

# Publication growth ------------------------------------------------------

pub_growth <- df_blog1 |>
  count(year, name = "n_publications")

ggplot(pub_growth, aes(x = year, y = n_publications)) +
  geom_col(fill = bf_pal["blue"], width = 0.85) +
  scale_x_continuous(
    breaks = seq(1980, 2025, 5),
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title = "The Literature Has Exploded",
    subtitle = "PubMed-indexed implementation-relevant records, 1980-2025",
    x = NULL,
    y = "Number of publications",
    caption = "Source: PubMed-indexed records identified in the barriers/facilitators corpus."
  ) +
  theme_bf_blog()

# BF prevalence over time -------------------------------------------------

bf_prevalence <- df_blog1 |>
  group_by(year) |>
  summarise(
    n_publications = n(),
    n_with_bf_language = sum(bf_language, na.rm = TRUE),
    n_with_explicit_bf_phrase = sum(explicit_bf, na.rm = TRUE),
    share_with_bf_language = n_with_bf_language / n_publications,
    share_with_explicit_bf_phrase = n_with_explicit_bf_phrase / n_publications,
    .groups = "drop"
  )

print(bf_prevalence, n = 50)

bf_prevalence_long <- bf_prevalence |>
  select(year, share_with_bf_language, share_with_explicit_bf_phrase) |>
  pivot_longer(
    cols = c(share_with_bf_language, share_with_explicit_bf_phrase),
    names_to = "measure",
    values_to = "share"
  ) |>
  mutate(
    measure = dplyr::recode(
      measure,
      share_with_bf_language = "Broad barrier-like vocabulary",
      share_with_explicit_bf_phrase = "Explicit phrase: barriers and facilitators"
    )
  )

ggplot(bf_prevalence_long, aes(x = year, y = share, color = measure, linetype = measure)) +
  geom_line(linewidth = 1.15, lineend = "round") +
  scale_color_manual(
    values = c(
      "Broad barrier-like vocabulary" = bf_pal["blue"],
      "Explicit phrase: barriers and facilitators" = bf_pal["ink"]
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Broad barrier-like vocabulary" = "solid",
      "Explicit phrase: barriers and facilitators" = "longdash"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2025, 5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Barrier Language Is Nearly Everywhere",
    subtitle = "Broad barrier-like vocabulary is common across the corpus; explicit B&F framing remains much rarer.",
    x = NULL,
    y = "Share of publications",
    caption = "Source: PubMed-indexed implementation-relevant records, 1980-2025."
  ) +
  theme_bf_blog()

# Blog-facing summary -----------------------------------------------------

blog_summary <- bf_prevalence |>
  filter(year %in% c(min(year), max(year))) |>
  select(
    year,
    n_publications,
    share_with_bf_language,
    share_with_explicit_bf_phrase
  )

corpus_scope |> print(width = Inf)
blog_summary

# 1. Growth multiplier ----------------------------------------------------

growth_hook <- blog_summary |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    publications_first_year = n_publications[year == first_year],
    publications_last_year = n_publications[year == last_year],
    growth_multiplier = publications_last_year / publications_first_year
  )

print(growth_hook)

# 2. Narrower genre groups ------------------------------------------------

df_blog1 <- df_blog1 |>
  mutate(
    bf_explicit = str_detect(text, explicit_bf_pattern),
    bf_ambient = bf_language & !bf_explicit
  )

genre_scope <- df_blog1 |>
  summarise(
    total_publications = n(),
    n_bf_ambient = sum(bf_ambient, na.rm = TRUE),
    n_bf_explicit = sum(bf_explicit, na.rm = TRUE),
    pct_bf_ambient = mean(bf_ambient, na.rm = TRUE) * 100,
    pct_bf_explicit = mean(bf_explicit, na.rm = TRUE) * 100
  )

print(genre_scope)

# 3. Actionability / prioritization signal --------------------------------

prio_pattern <- regex(
  paste(
    "\\bprioritiz\\w*\\b",
    "\\brank\\w*\\b",
    "\\bmost important\\b",
    "\\bmost critical\\b",
    "\\bkey barrier\\b",
    "\\bprimary barrier\\b",
    "\\btop barrier\\b",
    "\\bdominant barrier\\b",
    "\\bhigh priority\\b",
    "\\bleverage point\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

action_pattern <- regex(
  paste(
    "\\brecommend\\w*\\b",
    "\\bwe suggest\\b",
    "\\bshould address\\b",
    "\\bshould target\\b",
    "\\bshould prioritize\\b",
    "\\bimplementation strateg\\w*\\b",
    "\\btailored strateg\\w*\\b",
    "\\bimplications for practice\\b",
    "\\bimplications for policy\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

actionability_scope <- df_blog1 |>
  filter(bf_explicit) |>
  summarise(
    n_explicit_bf = n(),
    pct_with_prioritization = mean(str_detect(text, prio_pattern), na.rm = TRUE) * 100,
    pct_with_action_language = mean(str_detect(text, action_pattern), na.rm = TRUE) * 100,
    pct_with_both = mean(
      str_detect(text, prio_pattern) & str_detect(text, action_pattern),
      na.rm = TRUE
    ) * 100
  )



# 4. Did actionability keep pace with growth? -----------------------------

actionability_trend <- df_blog1 |>
  filter(bf_explicit) |>
  mutate(
    has_prioritization = str_detect(text, prio_pattern),
    has_action = str_detect(text, action_pattern)
  ) |>
  group_by(year) |>
  summarise(
    n_explicit_bf = n(),
    share_prioritization = mean(has_prioritization, na.rm = TRUE),
    share_action = mean(has_action, na.rm = TRUE),
    share_both = mean(has_prioritization & has_action, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_explicit_bf >= 25)

print(actionability_trend, n = 50)


corpus_scope |> print(width = Inf)
blog_summary
pub_growth 
bf_prevalence |> print(n =50)
growth_hook
genre_scope