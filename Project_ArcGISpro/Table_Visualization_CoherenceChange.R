# Load tidyverse for convenience
library(tidyverse)

# Create data frame from your table
data <- tribble(
  ~Category,       ~Land_use,             ~Cluster_type, ~Area_m2,     ~Percentage,
  "Artificial use", "Building site",       "High-High",  312466,       2.1,
  "Artificial use", "Building site",       "Low-Low",    2813017,      31.3,
  "Artificial use", "Drainage",            "High-High",  579062,       4.0,
  "Artificial use", "Drainage",            "Low-Low",    113968,       1.3,
  "Artificial use", "Embankment",          "High-High",  7988,         0.1,
  "Artificial use", "Embankment",          "Low-Low",    4451,         0.0,
  "Artificial use", "Factory site",        "High-High",  1360,         0.0,
  "Artificial use", "Factory site",        "Low-Low",    227041,       2.5,
  "Artificial use", "Gas station",         "High-High",  0,            0.0,
  "Artificial use", "Gas station",         "Low-Low",    11290,        0.1,
  "Artificial use", "Parking lot",         "High-High",  1447,         0.0,
  "Artificial use", "Parking lot",         "Low-Low",    11239,        0.1,
  "Artificial use", "Railroad site",       "High-High",  8361,         0.1,
  "Artificial use", "Railroad site",       "Low-Low",    10727,        0.1,
  "Artificial use", "Religious site",      "High-High",  4292,         0.0,
  "Artificial use", "Religious site",      "Low-Low",    18526,        0.2,
  "Artificial use", "Road",                "High-High",  519583,       3.6,
  "Artificial use", "Road",                "Low-Low",    1341666,      14.9,
  "Artificial use", "Warehouse",           "High-High",  19663,        0.1,
  "Artificial use", "Warehouse",           "Low-Low",    38785,        0.4,
  "Artificial use", "Water supply site",   "High-High",  0,            0.0,
  "Artificial use", "Water supply site",   "Low-Low",    2650,         0.0,
  "Natural use",    "Cemetery",            "High-High",  57857,        0.4,
  "Natural use",    "Cemetery",            "Low-Low",    29980,        0.3,
  "Natural use",    "Farmland",            "High-High",  873853,       6.0,
  "Natural use",    "Farmland",            "Low-Low",    491657,       5.5,
  "Natural use",    "Forest",              "High-High",  4694082,      32.3,
  "Natural use",    "Forest",              "Low-Low",    1753912,      19.5,
  "Natural use",    "Orchard",             "High-High",  113329,       0.8,
  "Natural use",    "Orchard",             "Low-Low",    191199,       2.1,
  "Natural use",    "Park area",           "High-High",  0,            0.0,
  "Natural use",    "Park area",           "Low-Low",    115336,       1.3,
  "Natural use",    "Ranch",               "High-High",  11280,        0.1,
  "Natural use",    "Ranch",               "Low-Low",    9301,         0.1,
  "Natural use",    "Reservoir",           "High-High",  30589,        0.2,
  "Natural use",    "Reservoir",           "Low-Low",    4957,         0.1,
  "Natural use",    "Rice paddy",          "High-High",  6930415,      47.6,
  "Natural use",    "Rice paddy",          "Low-Low",    1199034,      13.3,
  "Natural use",    "River",               "High-High",  288673,       2.0,
  "Natural use",    "River",               "Low-Low",    85030,        0.9,
  "Mixed use",      "Miscellaneous land",  "High-High",  59042,        0.4,
  "Mixed use",      "Miscellaneous land",  "Low-Low",    207420,       2.3,
  "Mixed use",      "School site",         "High-High",  25822,        0.2,
  "Mixed use",      "School site",         "Low-Low",    269075,       3.0,
  "Mixed use",      "Sports facility",     "High-High",  5195,         0.0,
  "Mixed use",      "Sports facility",     "Low-Low",    49366,        0.5
)

# Check structure
glimpse(data)

ggplot(data, aes(x = Land_use, y = Percentage, fill = Cluster_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Category, scales = "free_x", nrow = 1) +
  labs(title = "Percentage of Area by Land Use and Cluster Type",
       y = "Percentage (%)", x = "Land Use") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################

library(tidyverse)

# Summarize and group minor land uses (<10%)
data_agg <- data %>%
  mutate(Land_use_group = ifelse(Percentage < 10, "Other (<10%)", Land_use)) %>%
  group_by(Category, Land_use_group, Cluster_type) %>%
  summarise(Area_m2 = sum(Area_m2),
            Percentage = sum(Percentage),
            .groups = "drop")

# Verify grouping result
data_agg %>%
  arrange(Cluster_type, desc(Percentage))

# ggplot(data_agg, aes(x = reorder(Land_use_group, Percentage), 
#                      y = Percentage, 
#                      fill = Cluster_type)) +
#   geom_col(position = "dodge") +
#   facet_wrap(~ Category, scales = "fixed", nrow = 1) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.03))) + 
#   # ↑ Adds 3% padding to the top of y-axis (right side after flip)
#   labs(title = "Major Land-Use Contributions (>10%) by Cluster Type",
#        subtitle = "Minor land uses aggregated into 'Other (<10%)'",
#        x = "Land Use Group",
#        y = "Percentage of Area (%)",
#        fill = "Cluster Type") +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold"),
#     plot.title = element_text(face = "bold")
#   )
# 
# ###########################################################
# library(tidyverse)
# 
# # 1) Make sure Cluster_type order is HH (top), LL (bottom)
# data_agg <- data_agg %>%
#   mutate(Cluster_type = factor(Cluster_type, levels = c("High-High", "Low-Low")))
# 
# # 2) Ensure every Land_use_group appears for BOTH clusters (fill missing with 0)
# all_groups <- distinct(data_agg, Category, Land_use_group)
# data_agg_complete <- all_groups %>%
#   crossing(Cluster_type = factor(c("High-High", "Low-Low"),
#                                  levels = c("High-High", "Low-Low"))) %>%
#   left_join(data_agg, by = c("Category", "Land_use_group", "Cluster_type")) %>%
#   mutate(
#     Area_m2   = replace_na(Area_m2, 0),
#     Percentage = replace_na(Percentage, 0)
#   )
# 
# # 3) Order Land_use_group within each Category by its max share across clusters
# library(forcats)
# 
# # Make sure order is handled manually (no tidytext dependency)
# data_agg_complete <- data_agg_complete %>%
#   group_by(Category) %>%
#   mutate(Land_use_group = fct_reorder(Land_use_group, order_key, .desc = TRUE)) %>%
#   ungroup()
# 
# ggplot(data_agg_complete,
#        aes(x = Land_use_group,
#            y = Percentage,
#            fill = Cluster_type)) +
#   geom_col(width = 0.6) +
#   geom_text(aes(label = sprintf("%.1f%%", Percentage)),
#             hjust = -0.2,
#             size = 3.3,
#             color = "black") +
#   coord_flip() +
#   facet_grid(rows = vars(Cluster_type),
#              cols = vars(Category),
#              scales = "fixed") +
#   scale_y_continuous(limits = c(0, 100),
#                      expand = expansion(mult = c(0, 0.05))) +
#   labs(
#     title = "Major Land-Use Contributions (>10%) by Cluster Type",
#     subtitle = "Minor land uses aggregated into 'Other (<10%)'",
#     x = "Land Use Group",
#     y = "Percentage of Area (%)",
#     fill = "Cluster Type"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold"),
#     plot.title = element_text(face = "bold"),
#     panel.spacing.x = unit(0.8, "cm"),
#     panel.spacing.y = unit(1.0, "cm")
#   )


###############################################################
library(tidyverse)
library(grid)  # for unit()

# Rebuild complete dataset (unchanged logic)
data_agg_complete <- data_agg %>%
  mutate(
    Category = factor(Category, levels = c("Artificial use", "Mixed use", "Natural use")),
    # Low-Low first, High-High second -> HH appears on TOP after coord_flip
    Cluster_type = factor(Cluster_type, levels = c("Low-Low","High-High"))
  )

all_groups <- distinct(data_agg_complete, Category, Land_use_group)
data_agg_complete <- all_groups %>%
  crossing(Cluster_type = levels(data_agg_complete$Cluster_type)) %>%
  left_join(data_agg_complete,
            by = c("Category", "Land_use_group", "Cluster_type")) %>%
  mutate(
    Area_m2   = replace_na(Area_m2, 0),
    Percentage = replace_na(Percentage, 0)
  ) %>%
  group_by(Category, Land_use_group) %>%
  mutate(order_key = max(Percentage, na.rm = TRUE)) %>%
  ungroup()

data_agg_complete$Land_use_group <- factor(data_agg_complete$Land_use_group,levels=c("Road","Building site","Forest","Rice paddy","Other (<10%)"))

# Position dodge for uniform grouped bars
pd <- position_dodge2(width = 0.72, preserve = "single", padding = 0.12)

ggplot(data_agg_complete,
       aes(x = fct_reorder(Land_use_group, order_key, .desc = FALSE),
           y = Percentage,
           fill = Cluster_type)) +
  geom_col(width = 0.62, position = pd) +
  geom_text(
    aes(label = ifelse(Percentage > 0, sprintf("%.1f%%", Percentage), "")), # hide 0.0%
    position = pd,
    hjust = -0.2,
    size = 3.2
  ) +
  coord_flip() +
  facet_wrap(~ Category, nrow = 1, scales = "fixed") +
  scale_y_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Land-Use Area Percentages by Cluster Type",
       subtitle = "Minor land uses aggregated into 'Other (<10%)'",
    x = "Land Use Group",
    y = "Percentage of Area (%)",
    fill = "Cluster Type"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +   # legend shows High–High above Low–Low
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.spacing.x = unit(0.8, "cm")
  )

