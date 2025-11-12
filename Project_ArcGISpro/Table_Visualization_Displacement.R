library(tidyverse)
library(grid)  # for unit()

# --- 1) Enter NEW data (areas in m2; percents numeric) -----------------------
data_new <- tribble(
  ~Category,         ~Land_use,            ~HH_area,   ~HH_pct, ~LL_area,   ~LL_pct, ~Study_area, ~Study_pct,
  "Artificial use",  "Building site",      1180016,     3.2,    3401705,     8.0,    10363991,     7.7,
  "Artificial use",  "Drainage",           1055148,     2.8,     624283,     1.5,     2877729,     2.1,
  "Artificial use",  "Embankment",           36801,     0.1,      17436,     0.0,       81551,     0.1,
  "Artificial use",  "Factory site",          71065,     0.2,    1128339,     2.6,     2418420,     1.8,
  "Artificial use",  "Gas station",            4752,     0.0,       27688,     0.1,       67895,     0.1,
  "Artificial use",  "Parking lot",           63390,     0.0,       39442,     0.1,       77008,     0.1,
  "Artificial use",  "Railroad site",             0,     0.0,       44851,     0.1,      181332,     0.1,
  "Artificial use",  "Religious site",        41045,     0.1,       66492,     0.2,      181963,     0.1,
  "Artificial use",  "Road",                 933348,     2.5,     2655936,     6.2,     7209036,     5.4,
  "Artificial use",  "Warehouse",             72203,     0.2,       88718,     0.2,      241379,     0.2,
  "Artificial use",  "Water supply site",      7913,     0.0,        3780,     0.0,       25410,     0.0,
  # (Category subtotal row omitted from plotting)
  # "Artificial use",  "_SUBTOTAL_",         3408630,    10.0,     8098671,    51.0,    23725713,    51.0,
  
  "Natural use",     "Cemetery",            130298,     0.4,      178254,     0.4,      547844,     0.4,
  "Natural use",     "Farmland",           1532787,     4.1,     2465207,     5.8,     7272013,     5.4,
  "Natural use",     "Forest",            21837795,    58.9,    22403400,    52.5,    69311886,    51.4,
  "Natural use",     "Orchard",             320048,     0.9,       92149,     0.2,     1597025,     1.2,
  "Natural use",     "Park area",             2613,     0.0,      302543,     0.7,      502642,     0.4,
  "Natural use",     "Ranch",               149277,     0.4,       36001,     0.1,      328922,     0.2,
  "Natural use",     "Reservoir",           172435,     0.5,      173678,     0.4,      624519,     0.5,
  "Natural use",     "Rice paddy",         7630453,    20.6,     6959956,    16.3,    24160739,    17.9,
  "Natural use",     "River",              1504984,     4.1,     1050646,     2.5,     4130796,     3.1,
  # "Natural use",     "_SUBTOTAL_",        33280691,    89.4,    33661836,    43.1,   108476386,    43.1,
  
  "Mixed use",       "Miscellaneous land",   213728,     0.6,      474062,     1.1,     1179098,     0.9,
  "Mixed use",       "School site",          157499,     0.4,      359173,     0.8,     1213942,     0.9,
  "Mixed use",       "Sports facility",       16578,     0.0,       68793,     0.2,      138847,     0.1
  # "Mixed use",       "_SUBTOTAL_",           387805,     0.6,      902029,     5.8,     2531887,     5.8,
  # Totals row omitted (not plotted)
)

# --- 2) Long format with THREE series: HH, LL, Study -------------------------
data_long3 <- data_new %>%
  pivot_longer(
    cols = c(HH_area, HH_pct, LL_area, LL_pct, Study_area, Study_pct),
    names_to = c("Cluster_type", ".value"),
    names_pattern = "(HH|LL|Study)_(area|pct)"
  ) %>%
  mutate(
    Cluster_type = recode(Cluster_type,
                          HH = "High-High",
                          LL = "Low-Low",
                          Study = "Study area"),
    Category = factor(Category, levels = c("Artificial use", "Mixed use", "Natural use"))
  )

# --- 3) Complete missing combos; order so after coord_flip(): HH top, LL middle, Study bottom
data_agg_complete <- data_long3 %>%
  complete(
    Category, Land_use,
    Cluster_type = factor(c("Study area", "Low-Low", "High-High"),
                          levels = c("Study area", "Low-Low", "High-High")),
    fill = list(area = 0, pct = 0)
  ) %>%
  group_by(Category, Land_use) %>%
  mutate(order_key = max(pct, na.rm = TRUE)) %>%  # ordering helper
  ungroup() %>%
  transmute(
    Category,
    Land_use_group = Land_use,
    Cluster_type,
    Area_m2   = area,
    Percentage = pct,
    order_key
  )

library(tidyverse)
library(grid)

# --- 1️⃣  Identify land uses that occupy <5% in Study area --------------------
low_study_uses <- data_long3 %>%
  filter(Cluster_type == "Study area" & pct < 5) %>%
  distinct(Land_use) %>%
  pull(Land_use)

# --- 2️⃣  Aggregate those land uses into "Other (<5%)" ------------------------
data_agg_5 <- data_agg_complete %>%
  mutate(Land_use_group = ifelse(Land_use_group %in% low_study_uses,
                                 "Other (<5%)", Land_use_group)) %>%
  group_by(Category, Land_use_group, Cluster_type) %>%
  summarise(
    Area_m2 = sum(Area_m2, na.rm = TRUE),
    Percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Category, Land_use_group) %>%
  mutate(order_key = max(Percentage, na.rm = TRUE)) %>%
  ungroup()

# --- 3️⃣ Plot with custom colors ---------------------------------
pd <- position_dodge2(width = 0.72, preserve = "single", padding = 0.12)

ggplot(data_agg_5,
       aes(x = fct_reorder(Land_use_group, order_key, .desc = TRUE),
           y = Percentage,
           fill = Cluster_type)) +
  geom_col(width = 0.62, position = pd) +
  geom_text(
    aes(label = ifelse(Percentage > 0, sprintf("%.1f%%", Percentage), "")),
    position = pd,
    hjust = -0.2, size = 3.2
  ) +
  coord_flip() +
  facet_wrap(~ Category, nrow = 1, scales = "fixed") +
  scale_y_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0, 0.05))) +
  # --- 🎨 Custom color scheme ---
  scale_fill_manual(
    values = c(
      "Study area" = "#00BA39",   # green
      "Low-Low"    = "#609CFF",   # blue
      "High-High"  = "#F8766D"    # red
    )
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Land-Use Area Percentages by Cluster Type",
    subtitle = "Land uses less than 5% of study area grouped as 'Other (<5%)'",
    x = "Land-Use Group",
    y = "Percentage of Area (%)",
    fill = "Series"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.spacing.x = unit(0.8, "cm"),
    panel.spacing.y = unit(1.0, "cm")
  )
