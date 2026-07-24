

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

#
# BLOG 6 ------------------------------------------------------------------
# The Comfortable Politics of Implementation Science
#
# Question:
# Why do implementation papers talk about people instead of institutions?
#

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

people_pattern <- regex(
  paste(
    "\\btrust\\w*\\b",
    "\\bbuy-?in\\b",
    "\\bengag\\w*\\b",
    "\\baccept\\w*\\b",
    "\\battitude\\w*\\b",
    "\\bbelief\\w*\\b",
    "\\bperception\\w*\\b",
    "\\bperspective\\w*\\b",
    "\\bmotivat\\w*\\b",
    "\\bconfidence\\b",
    "\\bself-efficacy\\b",
    "\\brelationship\\w*\\b",
    "\\brelational\\b",
    "\\bchampion\\w*\\b",
    "\\bleader\\w*\\b",
    "\\bstakeholder\\w*\\b",
    "\\bcommunication\\b",
    "\\bcollaboration\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

institution_pattern <- regex(
  paste(
    "\\bprocure\\w*\\b",
    "\\bpurchas\\w*\\b",
    "\\bcontract\\w*\\b",
    "\\bcommission\\w*\\b",
    "\\bpayment\\w*\\b",
    "\\breimburse\\w*\\b",
    "\\bbilling\\b",
    "\\bfinanc\\w*\\b",
    "\\bfunding\\b",
    "\\bfunder\\w*\\b",
    "\\bbudget\\w*\\b",
    "\\bcost\\w*\\b",
    "\\bresource allocation\\b",
    "\\bregulat\\w*\\b",
    "\\bpolicy\\b",
    "\\bpolicies\\b",
    "\\blaw\\b",
    "\\blegal\\b",
    "\\bliability\\b",
    "\\bcompliance\\b",
    "\\baccredit\\w*\\b",
    "\\bgovernance\\b",
    "\\binstitution\\w*\\b",
    "\\borganization\\w* structure\\w*\\b",
    "\\borganizational structure\\w*\\b",
    "\\bworkforce\\b",
    "\\blabor\\b",
    "\\bmarket\\w*\\b",
    "\\binfrastructure\\b",
    "\\bsupply chain\\w*\\b",
    "\\breporting requirement\\w*\\b",
    "\\beligibility\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

people_dictionary <- tribble(
  ~category, ~pattern,
  "Trust", "\\btrust\\w*\\b",
  "Buy-in", "\\bbuy-?in\\b|\\baccept\\w*\\b|\\battitude\\w*\\b|\\bbelief\\w*\\b",
  "Engagement", "\\bengag\\w*\\b|\\bstakeholder\\w*\\b|\\bparticipat\\w*\\b",
  "Relationships", "\\brelationship\\w*\\b|\\brelational\\b|\\bcollaboration\\b|\\bcommunication\\b"
) |>
  mutate(pattern = map(pattern, regex, ignore_case = TRUE))

institution_dictionary <- tribble(
  ~category, ~pattern,
  "Procurement and Contracting", "\\bprocure\\w*\\b|\\bpurchas\\w*\\b|\\bcontract\\w*\\b|\\bcommission\\w*\\b",
  "Payment and Reimbursement", "\\bpayment\\w*\\b|\\breimburse\\w*\\b|\\bbilling\\b|\\bfinanc\\w*\\b",
  "Budgets and Funding", "\\bbudget\\w*\\b|\\bfunding\\b|\\bfunder\\w*\\b|\\bcost\\w*\\b|\\bresource allocation\\b",
  "Regulation and Policy", "\\bregulat\\w*\\b|\\bpolicy\\b|\\bpolicies\\b|\\blaw\\b|\\blegal\\b|\\bliability\\b|\\bcompliance\\b|\\baccredit\\w*\\b"
) |>
  mutate(pattern = map(pattern, regex, ignore_case = TRUE))

df_blog6 <- df |>
  filter(!is.na(pmid), !is.na(year)) |>
  mutate(
    pmid = as.character(pmid),
    year = as.integer(year),
    title = coalesce(title, ""),
    abstract = coalesce(abstract, ""),
    text = str_squish(paste(title, abstract, sep = ". ")),
    text_lower = str_to_lower(text),
    bf_language = str_detect(text_lower, bf_pattern),
    people_language = str_detect(text_lower, people_pattern),
    institution_language = str_detect(text_lower, institution_pattern),
    politics_status = case_when(
      people_language & institution_language ~ "People and institutions",
      people_language & !institution_language ~ "People only",
      !people_language & institution_language ~ "Institutions only",
      TRUE ~ "Neither people nor institutions"
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

# 1. Category flags --------------------------------------------------------

people_flags <- people_dictionary |>
  mutate(value = map(pattern, \(pat) str_detect(df_blog6$text_lower, pat))) |>
  select(category, value) |>
  unnest_longer(value, indices_to = "row_id") |>
  mutate(domain = "People")

institution_flags <- institution_dictionary |>
  mutate(value = map(pattern, \(pat) str_detect(df_blog6$text_lower, pat))) |>
  select(category, value) |>
  unnest_longer(value, indices_to = "row_id") |>
  mutate(domain = "Institutions")

blog6_category_flags <- bind_rows(people_flags, institution_flags)

blog6_categories <- df_blog6 |>
  mutate(row_id = row_number()) |>
  left_join(blog6_category_flags, by = "row_id") |>
  filter(value) |>
  select(-value)

# 2. Headline percentages --------------------------------------------------

blog6_summary <- df_blog6 |>
  summarise(
    n_bf_records = n(),
    pct_with_people_language = mean(people_language, na.rm = TRUE),
    pct_with_institution_language = mean(institution_language, na.rm = TRUE),
    pct_people_only = mean(people_language & !institution_language, na.rm = TRUE),
    pct_institutions_only = mean(!people_language & institution_language, na.rm = TRUE),
    pct_both_people_and_institutions = mean(people_language & institution_language, na.rm = TRUE),
    pct_neither = mean(!people_language & !institution_language, na.rm = TRUE),
    people_to_institution_ratio = sum(people_language, na.rm = TRUE) / sum(institution_language, na.rm = TRUE)
  )

blog6_status_summary <- df_blog6 |>
  count(politics_status, name = "n_records") |>
  mutate(
    share = n_records / sum(n_records),
    politics_status = factor(
      politics_status,
      levels = c(
        "People only",
        "People and institutions",
        "Institutions only",
        "Neither people nor institutions"
      )
    )
  ) |>
  arrange(politics_status)

blog6_category_summary <- blog6_categories |>
  group_by(domain, category) |>
  summarise(
    n_records = n_distinct(pmid),
    pct_all_bf_records = n_records / n_distinct(df_blog6$pmid),
    .groups = "drop"
  ) |>
  arrange(domain, desc(n_records))


blog6_year <- df_blog6 |>
  group_by(year) |>
  summarise(
    n_records = n(),
    people_share = mean(people_language, na.rm = TRUE),
    institution_share = mean(institution_language, na.rm = TRUE),
    people_only_share = mean(people_language & !institution_language, na.rm = TRUE),
    institutions_only_share = mean(!people_language & institution_language, na.rm = TRUE),
    both_share = mean(people_language & institution_language, na.rm = TRUE),
    neither_share = mean(!people_language & !institution_language, na.rm = TRUE),
    people_to_institution_ratio = sum(people_language, na.rm = TRUE) / sum(institution_language, na.rm = TRUE),
    .groups = "drop"
  )

blog6_period <- df_blog6 |>
  group_by(period) |>
  summarise(
    n_records = n(),
    people_share = mean(people_language, na.rm = TRUE),
    institution_share = mean(institution_language, na.rm = TRUE),
    people_only_share = mean(people_language & !institution_language, na.rm = TRUE),
    institutions_only_share = mean(!people_language & institution_language, na.rm = TRUE),
    both_share = mean(people_language & institution_language, na.rm = TRUE),
    neither_share = mean(!people_language & !institution_language, na.rm = TRUE),
    people_to_institution_ratio = sum(people_language, na.rm = TRUE) / sum(institution_language, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")))

blog6_status_period <- df_blog6 |>
  count(period, politics_status, name = "n_records") |>
  group_by(period) |>
  mutate(share = n_records / sum(n_records)) |>
  ungroup() |>
  mutate(
    period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")),
    politics_status = factor(
      politics_status,
      levels = c(
        "People only",
        "People and institutions",
        "Institutions only",
        "Neither people nor institutions")))


p_people_vs_institutions_trend <- ggplot(blog6_year, aes(x = year)) +
  geom_line(
    aes(y = people_share, color = "People language"),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_line(
    aes(y = institution_share, color = "Institution language"),
    linewidth = 1.5,
    lineend = "round"
  ) +
  geom_line(
    aes(y = people_only_share, color = "People only"),
    linewidth = 1.5,
    lineend = "round"
  ) +
  scale_color_manual(
    values = c(
      "People language" = pal("blue"),
      "Institution language" = pal("teal"),
      "People only" = pal("coral")
    )
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, 0.05),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "The Comfortable Politics of Implementation Science",
    subtitle = "BF-language records more often talk about people than institutional arrangements.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "People language includes trust, buy-in, engagement, attitudes, stakeholders, leadership, relationships, communication, and related terms."
  ) +
  theme_bf_blog()

p_people_vs_institutions_trend

p_people_institution_ratio <- blog6_period |>
  ggplot(aes(x = period, y = people_to_institution_ratio, group = 1)) +
  geom_col(fill = pal("gold"), width = 0.62) +
  geom_line(color = pal("ink"), linewidth = 0.8) +
  geom_point(color = pal("ink"), size = 3) +
  geom_text(
    aes(label = number(people_to_institution_ratio, accuracy = 0.1)),
    vjust = -0.55,
    size = 8,
    color = pal("ink")
  ) +
  scale_y_continuous(
    limits = c(0, max(blog6_period$people_to_institution_ratio, na.rm = TRUE) * 1.22),
    breaks = seq(0, 1.25, 0.25)
  ) +
  labs(
    title = "How Much More Often Do Papers Talk About People?",
    subtitle = "Ratio of people-language records to institution-language records by period.",
    x = NULL,
    y = "People-to-institution ratio",
    caption = "Institution language includes procurement, contracting, payment, reimbursement, budgets, funding, regulation, policy, compliance, and governance."
  ) +
  theme_bf_blog()

p_people_institution_ratio

p_politics_status_period <- blog6_status_period |>
  ggplot(aes(x = period, y = share, fill = politics_status)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68, color = "black") +
  scale_fill_manual(
    values = c(
      "People only" = pal("coral"),
      "People and institutions" = pal("teal"),
      "Institutions only" = pal("gold"),
      "Neither people nor institutions" = pal("light_gray")
    )
  ) +
  scale_y_continuous(breaks = seq(0,1,0.1),
                     labels = percent_format(accuracy = 1)) +
  labs(
    title = "People Problems or Institutional Problems?",
    subtitle = "Many records use people language without also naming institutional arrangements.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Shares sum to 100% within each period."
  ) +
  theme_bf_blog()

p_politics_status_period

p_category_comparison <- blog6_category_summary |>
  mutate(
    category = fct_reorder(category, pct_all_bf_records),
    domain = factor(domain, levels = c("People", "Institutions"))
  ) |>
  ggplot(aes(x = pct_all_bf_records, y = category, fill = domain)) +
  geom_col(width = 0.68) +
  scale_fill_manual(
    values = c(
      "People" = pal("blue"),
      "Institutions" = pal("teal")
    )
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Soft Words and Hard Structures",
    subtitle = "Relational categories are compared with institutional categories across BF-language records.",
    x = "Share of BF-language records",
    y = NULL,
    caption = "Categories are detected from title and abstract language. Records can appear in more than one category."
  ) +
  theme_bf_blog()

p_category_comparison

# 5. Candidate examples ----------------------------------------------------

blog6_people_only_examples <- df_blog6 |>
  filter(people_language, !institution_language) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, example_text) |>
  slice_head(n = 20)

blog6_institution_examples <- df_blog6 |>
  filter(institution_language) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, example_text) |>
  slice_head(n = 20)

blog6_both_examples <- df_blog6 |>
  filter(people_language, institution_language) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, example_text) |>
  slice_head(n = 20)

# 6. Compact tables for writing -------------------------------------------

blog6_summary
blog6_status_summary
blog6_category_summary
blog6_period
blog6_year |> print(n = 60)
blog6_people_only_examples |> print(n = 20, width = Inf)
blog6_institution_examples |> print(n = 20, width = Inf)
blog6_both_examples |> print(n = 20, width = Inf)

# Optional exports ---------------------------------------------------------
