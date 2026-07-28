

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

frontline_pattern <- regex(
  paste(
    "\\bfront-?line\\b",
    "\\bfront line\\b",
    "\\bworkforce\\b",
    "\\bstaff\\b",
    "\\bworker\\w*\\b",
    "\\bclinician\\w*\\b",
    "\\bprovider\\w*\\b",
    "\\bpractitioner\\w*\\b",
    "\\bnurse\\w*\\b",
    "\\bphysician\\w*\\b",
    "\\bteacher\\w*\\b",
    "\\bcounselor\\w*\\b",
    "\\btherapist\\w*\\b",
    "\\bsocial worker\\w*\\b",
    "\\bcase manager\\w*\\b",
    "\\bcaseworker\\w*\\b",
    "\\bpeer specialist\\w*\\b",
    "\\bcare coordinator\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

organizational_authority_pattern <- regex(
  paste(
    "\\bleader\\w*\\b",
    "\\bmanager\\w*\\b",
    "\\bmanagement\\b",
    "\\badministrator\\w*\\b",
    "\\bdirector\\w*\\b",
    "\\bexecutive\\w*\\b",
    "\\bsupervisor\\w*\\b",
    "\\bboard\\b",
    "\\bgovernance\\b",
    "\\bhospital\\w*\\b",
    "\\bhealth system\\w*\\b",
    "\\bclinic\\w*\\b",
    "\\borganization\\w*\\b",
    "\\bagenc\\w*\\b",
    "\\bschool district\\w*\\b",
    "\\bsystem leadership\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

system_authority_pattern <- regex(
  paste(
    "\\bstate agenc\\w*\\b",
    "\\bfederal\\b",
    "\\bgovernment\\b",
    "\\bministry\\b",
    "\\bdepartment of health\\b",
    "\\bpublic health department\\w*\\b",
    "\\bmedicaid\\b",
    "\\bmedicare\\b",
    "\\bpayer\\w*\\b",
    "\\binsurer\\w*\\b",
    "\\breimburse\\w*\\b",
    "\\bpayment\\w*\\b",
    "\\bprocure\\w*\\b",
    "\\bcontract\\w*\\b",
    "\\bbudget\\w*\\b",
    "\\bfunding\\b",
    "\\bregulat\\w*\\b",
    "\\bpolicy\\b",
    "\\bpolicies\\b",
    "\\blegislat\\w*\\b",
    "\\bmandate\\w*\\b",
    "\\blicens\\w*\\b",
    "\\baccredit\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

power_pattern <- regex(
  paste(
    "\\bpower\\b",
    "\\bcontrol\\w*\\b",
    "\\bauthorit\\w*\\b",
    "\\bdecision-?making\\b",
    "\\bdecision making\\b",
    "\\bdecide\\w*\\b",
    "\\bchoice\\w*\\b",
    "\\bgovern\\w*\\b",
    "\\bmandate\\w*\\b",
    "\\brequir\\w*\\b",
    "\\baccountab\\w*\\b",
    "\\bownership\\b",
    "\\bautonom\\w*\\b",
    "\\bdiscretion\\b",
    "\\bapproval\\b",
    "\\bpermission\\b",
    "\\bresource allocation\\b",
    "\\bbudget author\\w*\\b",
    "\\bpolicy lever\\w*\\b",
    "\\blever\\w*\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

setting_dictionary <- tribble(
  ~setting, ~pattern,
  "State agency", "\\bstate agenc\\w*\\b|\\bstate health department\\w*\\b|\\bdepartment of health\\b|\\bpublic health department\\w*\\b|\\bgovernment agenc\\w*\\b",
  "Hospital", "\\bhospital\\w*\\b|\\bmedical center\\w*\\b|\\bhealth system\\w*\\b|\\bacute care\\b|\\binpatient\\b",
  "School district", "\\bschool district\\w*\\b|\\bdistrict\\w*\\b|\\bschool\\w*\\b|\\beducation agenc\\w*\\b|\\bteacher\\w*\\b",
  "Behavioral health system", "\\bbehavioral health\\b|\\bmental health system\\w*\\b|\\bcommunity mental health\\b|\\bsubstance use\\b|\\bSUD\\b|\\bpsychiatric\\b|\\baddiction\\b"
) |>
  mutate(pattern = map(pattern, regex, ignore_case = TRUE))

actor_dictionary <- tribble(
  ~actor_domain, ~pattern,
  "Front-line workers", frontline_pattern,
  "Organizational authority", organizational_authority_pattern,
  "System/state/payer authority", system_authority_pattern
)

df_blog7 <- df |>
  filter(!is.na(pmid), !is.na(year)) |>
  mutate(
    pmid = as.character(pmid),
    year = as.integer(year),
    title = coalesce(title, ""),
    abstract = coalesce(abstract, ""),
    text = str_squish(paste(title, abstract, sep = ". ")),
    text_lower = str_to_lower(text),
    bf_language = str_detect(text_lower, bf_pattern),
    frontline_language = str_detect(text_lower, frontline_pattern),
    organizational_authority_language = str_detect(text_lower, organizational_authority_pattern),
    system_authority_language = str_detect(text_lower, system_authority_pattern),
    authority_language = organizational_authority_language | system_authority_language,
    power_language = str_detect(text_lower, power_pattern),
    control_status = case_when(
      frontline_language & authority_language ~ "Front-line plus authority",
      frontline_language & !authority_language ~ "Front-line only",
      !frontline_language & authority_language ~ "Authority only",
      TRUE ~ "Neither front-line nor authority"),
    power_status = case_when(
      power_language & frontline_language & authority_language ~ "Power + front-line + authority",
      power_language & frontline_language & !authority_language ~ "Power + front-line only",
      power_language & !frontline_language & authority_language ~ "Power + authority only",
      power_language ~ "Power language only",
      TRUE ~ "No power language"),
    period = case_when(year <= 1999 ~ "1980-1999",
                       year <= 2009 ~ "2000-2009",
                       year <= 2019 ~ "2010-2019",
                       TRUE ~ "2020-2025")) |>
  distinct(pmid, .keep_all = TRUE) |>
  filter(year >= 1980,
         year <= 2025,
         bf_language,
         !is.na(text),
         nchar(text) > 50)

# 1. Setting and actor flags ----------------------------------------------

setting_flags <- setting_dictionary |>
  mutate(value = map(pattern, \(pat) str_detect(df_blog7$text_lower, pat))) |>
  select(setting, value) |>
  unnest_longer(value, indices_to = "row_id")

blog7_settings <- df_blog7 |>
  mutate(row_id = row_number()) |>
  left_join(setting_flags, by = "row_id") |>
  filter(value) |>
  select(-value)

actor_flags <- actor_dictionary |>
  mutate(value = map(pattern, \(pat) str_detect(df_blog7$text_lower, pat))) |>
  select(actor_domain, value) |>
  unnest_longer(value, indices_to = "row_id")

blog7_actors <- df_blog7 |>
  mutate(row_id = row_number()) |>
  left_join(actor_flags, by = "row_id") |>
  filter(value) |>
  select(-value)

# 2. Headline percentages --------------------------------------------------

blog7_summary <- df_blog7 |>
  summarise(
    n_bf_records = n(),
    pct_with_frontline_language = mean(frontline_language, na.rm = TRUE),
    pct_with_organizational_authority_language = mean(organizational_authority_language, na.rm = TRUE),
    pct_with_system_authority_language = mean(system_authority_language, na.rm = TRUE),
    pct_with_any_authority_language = mean(authority_language, na.rm = TRUE),
    pct_with_power_language = mean(power_language, na.rm = TRUE),
    pct_frontline_only = mean(frontline_language & !authority_language, na.rm = TRUE),
    pct_authority_only = mean(!frontline_language & authority_language, na.rm = TRUE),
    pct_frontline_plus_authority = mean(frontline_language & authority_language, na.rm = TRUE),
    pct_power_records_with_frontline = mean(frontline_language[power_language], na.rm = TRUE),
    pct_power_records_with_authority = mean(authority_language[power_language], na.rm = TRUE),
    pct_power_records_with_system_authority = mean(system_authority_language[power_language], na.rm = TRUE))

blog7_control_status_summary <- df_blog7 |>
  count(control_status, name = "n_records") |>
  mutate(share = n_records / sum(n_records),
         control_status = factor(control_status,
                                 levels = c("Front-line only",
                                            "Front-line plus authority",
                                            "Authority only",
                                            "Neither front-line nor authority"))) |>
  arrange(control_status)

blog7_power_status_summary <- df_blog7 |>
  count(power_status, name = "n_records") |>
  mutate(share = n_records / sum(n_records),
         power_status = factor(power_status,
                               levels = c("Power + front-line only",
                                          "Power + front-line + authority",
                                          "Power + authority only",
                                          "Power language only",
                                          "No power language" ))) |>
  arrange(power_status)

blog7_actor_summary <- blog7_actors |>
  group_by(actor_domain) |>
  summarise(
    n_records = n_distinct(pmid),
    pct_all_bf_records = n_records / n_distinct(df_blog7$pmid),
    n_with_power_language = n_distinct(pmid[power_language]),
    power_share_within_actor_domain = n_with_power_language / n_records,
    .groups = "drop") |>
  mutate(actor_domain = factor(actor_domain,
                               levels = c("Front-line workers", "Organizational authority", "System/state/payer authority"))) |>
  arrange(actor_domain)

blog7_power_actor_summary <- blog7_actors |>
  filter(power_language) |>
  group_by(actor_domain) |>
  summarise(
    n_power_records = n_distinct(pmid),
    pct_power_records = n_power_records / n_distinct(df_blog7$pmid[df_blog7$power_language]),
    .groups = "drop") |>
  mutate(
    actor_domain = factor(
      actor_domain,
      levels = c("Front-line workers", "Organizational authority", "System/state/payer authority"))) |>
  arrange(actor_domain)

# 3. Settings: state agency, hospital, school district, behavioral health ---

blog7_setting_summary <- blog7_settings |>
  group_by(setting) |>
  summarise(
    n_records = n_distinct(pmid),
    pct_all_bf_records = n_records / n_distinct(df_blog7$pmid),
    frontline_share_within_setting = n_distinct(pmid[frontline_language]) / n_records,
    organizational_authority_share_within_setting = n_distinct(pmid[organizational_authority_language]) / n_records,
    system_authority_share_within_setting = n_distinct(pmid[system_authority_language]) / n_records,
    any_authority_share_within_setting = n_distinct(pmid[authority_language]) / n_records,
    power_share_within_setting = n_distinct(pmid[power_language]) / n_records,
    frontline_only_share_within_setting = n_distinct(pmid[frontline_language & !authority_language]) / n_records,
    authority_only_share_within_setting = n_distinct(pmid[!frontline_language & authority_language]) / n_records,
    .groups = "drop") |>
  mutate(
    setting = factor(
      setting,
      levels = c("State agency", "Hospital", "School district", "Behavioral health system"))) |>
  arrange(setting)

blog7_setting_actor_long <- blog7_setting_summary |>
  select(setting,
    frontline_share_within_setting,
    organizational_authority_share_within_setting,
    system_authority_share_within_setting) |>
  pivot_longer(cols = -setting,
               names_to = "actor_domain",
               values_to = "share") |>
  mutate(
    actor_domain = recode(
      actor_domain,
      frontline_share_within_setting = "Front-line workers",
      organizational_authority_share_within_setting = "Organizational authority",
      system_authority_share_within_setting = "System/state/payer authority"),
    actor_domain = factor(
      actor_domain,
      levels = c("Front-line workers", "Organizational authority", "System/state/payer authority")))

# 4. Trends over time ------------------------------------------------------

blog7_year <- df_blog7 |>
  group_by(year) |>
  summarise(
    n_records = n(),
    frontline_share = mean(frontline_language, na.rm = TRUE),
    organizational_authority_share = mean(organizational_authority_language, na.rm = TRUE),
    system_authority_share = mean(system_authority_language, na.rm = TRUE),
    any_authority_share = mean(authority_language, na.rm = TRUE),
    power_share = mean(power_language, na.rm = TRUE),
    frontline_only_share = mean(frontline_language & !authority_language, na.rm = TRUE),
    authority_only_share = mean(!frontline_language & authority_language, na.rm = TRUE),
    frontline_plus_authority_share = mean(frontline_language & authority_language, na.rm = TRUE),
    .groups = "drop")

blog7_period <- df_blog7 |>
  group_by(period) |>
  summarise(
    n_records = n(),
    frontline_share = mean(frontline_language, na.rm = TRUE),
    organizational_authority_share = mean(organizational_authority_language, na.rm = TRUE),
    system_authority_share = mean(system_authority_language, na.rm = TRUE),
    any_authority_share = mean(authority_language, na.rm = TRUE),
    power_share = mean(power_language, na.rm = TRUE),
    frontline_only_share = mean(frontline_language & !authority_language, na.rm = TRUE),
    authority_only_share = mean(!frontline_language & authority_language, na.rm = TRUE),
    frontline_plus_authority_share = mean(frontline_language & authority_language, na.rm = TRUE),
    .groups = "drop") |>
  mutate(period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")))

blog7_control_status_period <- df_blog7 |>
  count(period, control_status, name = "n_records") |>
  group_by(period) |>
  mutate(share = n_records / sum(n_records)) |>
  ungroup() |>
  mutate(
    period = factor(period, levels = c("1980-1999", "2000-2009", "2010-2019", "2020-2025")),
    control_status = factor(
      control_status,
      levels = c(
        "Front-line only",
        "Front-line plus authority",
        "Authority only",
        "Neither front-line nor authority")))

# 5. Visuals ---------------------------------------------------------------

p_actor_power_trend <- ggplot(blog7_year, aes(x = year)) +
  geom_line(
    aes(y = frontline_share, color = "Front-line workers"),
    linewidth = 1.5,
    lineend = "round") +
  geom_line(
    aes(y = any_authority_share, color = "Organizational/system authority"),
    linewidth = 1.5,
    lineend = "round") +
  geom_line(
    aes(y = power_share, color = "Power/control language"),
    linewidth = 1.5,
    lineend = "round") +
  scale_color_manual(
    values = c("Front-line workers" = pal("blue"),
               "Organizational/system authority" = pal("teal"),
               "Power/control language" = pal("coral"))) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(breaks = seq(0,0.7,0.1),
                     labels = percent_format(accuracy = 1)) +
  labs(
    title = "Who Actually Has the Power?",
    subtitle = "Front-line workers are visible, but implementation control is often located upstream.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Authority language includes organizational leadership and system/state/payer terms.\nPower/control language includes control, authority, governance, mandates, decisions, responsibility, autonomy, approval, and related terms."
  ) +
  theme_bf_blog()

p_actor_power_trend

p_control_status_period <- blog7_control_status_period |>
  ggplot(aes(x = period, y = share, fill = control_status)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68, color = "black") +
  scale_fill_manual(values = c(
      "Front-line only" = pal("coral"),
      "Front-line plus authority" = pal("teal"),
      "Authority only" = pal("gold"),
      "Neither front-line nor authority" = pal("light_gray"))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Front-Line Problems or Authority Problems?",
    subtitle = "Records are grouped by whether they name front-line actors, authority actors, both, or neither.",
    x = NULL,
    y = "Share of BF-language records",
    caption = "Shares sum to 100% within each period.") +
  theme_bf_blog()

p_control_status_period

p_setting_actor_comparison <- blog7_setting_actor_long |>
  ggplot(aes(x = setting, y = share, fill = actor_domain)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68, color = "black") +
  scale_fill_manual(values = c(
      "Front-line workers" = pal("blue"),
      "Organizational authority" = pal("teal"),
      "System/state/payer authority" = pal("gold"))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Where Control Sits Depends on the Setting",
    subtitle = "State agencies, hospitals, school districts, and behavioral health systems name different control locations.",
    x = NULL,
    y = "Share within setting records",
    caption = "Settings are detected from title and abstract language. A record can appear in more than one setting."
  ) +
  theme_bf_blog()

p_setting_actor_comparison

p_actor_power_gap <- blog7_actor_summary |>
  ggplot(aes(x = pct_all_bf_records, y = actor_domain)) +
  geom_col(fill = "#0F766E", width = 0.68) +
  geom_point(aes(x = power_share_within_actor_domain), color = pal("coral"), size = 7) +
  scale_x_continuous(breaks = seq(0,0.7,0.1),
                     labels = percent_format(accuracy = 1)) +
  labs(
    title = "Visibility Is Not the Same as Control",
    subtitle = "Bars show actor visibility. Dots show the share of each actor domain with power/control language.",
    x = "Share of BF-language records",
    y = NULL,
    caption = "Front-line workers can be highly visible without being the actors who control budgets, policy, contracts, or mandates."
  ) +
  theme_bf_blog()

p_actor_power_gap

p_power_actor_distribution <- blog7_power_actor_summary |>
  ggplot(aes(x = pct_power_records, y = actor_domain)) +
  geom_col(fill = pal("teal"), width = 0.68) +
  geom_text(aes(label = percent(pct_power_records, accuracy = 0.1)),
    hjust = -0.12,
    size = 8,
    color = pal("ink")) +
  scale_x_continuous(breaks = seq(0,0.8,0.1),
    labels = percent_format(accuracy = 1),
    limits = c(0, max(blog7_power_actor_summary$pct_power_records, na.rm = TRUE) * 1.18)) +
  labs(
    title = "When Papers Talk About Power, Who Is Named?",
    subtitle = "Share of power/control-language records that mention each actor domain.",
    x = "Share of power/control-language records",
    y = NULL,
    caption = "Actor domains are not mutually exclusive; one record can name front-line workers, organizational authority, and system/state/payer authority."
  ) +
  theme_bf_blog()

p_power_actor_distribution

# 6. Candidate examples --

blog7_frontline_only_examples <- df_blog7 |>
  filter(frontline_language, !authority_language) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, example_text) |>
  slice_head(n = 20)

blog7_authority_examples <- df_blog7 |>
  filter(authority_language) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, example_text) |>
  slice_head(n = 20)

blog7_power_examples <- df_blog7 |>
  filter(power_language) |>
  mutate(example_text = str_squish(str_sub(text, 1, 700))) |>
  arrange(desc(year)) |>
  select(pmid, year, title, control_status, power_status, example_text) |>
  slice_head(n = 30)

blog7_setting_examples <- blog7_settings |>
  mutate(example_text = str_squish(str_sub(text, 1, 650))) |>
  arrange(setting, desc(year)) |>
  select(setting, pmid, year, title, control_status, example_text) |>
  group_by(setting) |>
  slice_head(n = 8) |>
  ungroup()

toc()
beep(5)

# 7. Compact tables -------------------------------------------

blog7_summary |> print(width = Inf)
blog7_control_status_summary
blog7_power_status_summary
blog7_actor_summary
blog7_power_actor_summary
blog7_setting_summary |> print(width = Inf)
blog7_period
blog7_year |> print(n = 60)
blog7_frontline_only_examples |> print(n = 20, width = Inf)
blog7_authority_examples |> print(n = 20, width = Inf)
blog7_power_examples |> print(n = 30, width = Inf)
blog7_setting_examples |> print(n = 40, width = Inf)

