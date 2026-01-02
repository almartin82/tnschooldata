## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  warning = FALSE,
  message = FALSE,
  eval = FALSE
)

## ----load-packages------------------------------------------------------------
# library(tnschooldata)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# theme_set(theme_minimal(base_size = 14))

## ----statewide-data-----------------------------------------------------------
# enr <- fetch_enr_multi(c(2006, 2010, 2015, 2020, 2024))
# 
# statewide <- enr %>%
#   filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
#   select(end_year, n_students)
# 
# statewide

## ----statewide-chart----------------------------------------------------------
# ggplot(statewide, aes(x = end_year, y = n_students / 1e6)) +
#   geom_line(color = "#FF6600", linewidth = 1.2) +
#   geom_point(color = "#FF6600", size = 3) +
#   geom_text(aes(label = scales::comma(n_students)), vjust = -1, size = 3.5) +
#   scale_y_continuous(
#     labels = scales::label_number(suffix = "M"),
#     limits = c(0.85, 1.05)
#   ) +
#   labs(
#     title = "Tennessee Public School Enrollment",
#     subtitle = "Nearly 1 million students in K-12 public schools",
#     x = "Year",
#     y = "Total Students"
#   )

## ----top-districts-data-------------------------------------------------------
# enr_2024 <- fetch_enr(2024)
# 
# top_districts <- enr_2024 %>%
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
#   arrange(desc(n_students)) %>%
#   head(10) %>%
#   select(district_name, n_students)
# 
# top_districts

## ----top-districts-chart------------------------------------------------------
# top_districts %>%
#   mutate(district_name = reorder(district_name, n_students)) %>%
#   ggplot(aes(x = n_students / 1000, y = district_name)) +
#   geom_col(fill = "#FF6600") +
#   geom_text(aes(label = scales::comma(n_students)), hjust = -0.1, size = 3.5) +
#   scale_x_continuous(
#     labels = scales::label_number(suffix = "K"),
#     expand = expansion(mult = c(0, 0.15))
#   ) +
#   labs(
#     title = "Top 10 Tennessee School Districts by Enrollment (2024)",
#     subtitle = "Shelby County: Tennessee's largest district by far",
#     x = "Students (thousands)",
#     y = NULL
#   )

## ----demographics-data--------------------------------------------------------
# demographics <- enr %>%
#   filter(is_state, grade_level == "TOTAL",
#          subgroup %in% c("white", "black", "hispanic", "asian")) %>%
#   select(end_year, subgroup, n_students) %>%
#   mutate(subgroup = factor(subgroup,
#     levels = c("white", "black", "hispanic", "asian"),
#     labels = c("White", "Black", "Hispanic", "Asian")))
# 
# demographics

## ----demographics-chart-------------------------------------------------------
# ggplot(demographics, aes(x = end_year, y = n_students / 1000, color = subgroup)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 3) +
#   scale_color_manual(values = c(
#     "White" = "#4292C6",
#     "Black" = "#807DBA",
#     "Hispanic" = "#41AB5D",
#     "Asian" = "#EF6548"
#   )) +
#   scale_y_continuous(labels = scales::label_number(suffix = "K")) +
#   labs(
#     title = "Demographic Shifts in Tennessee Public Schools",
#     subtitle = "Hispanic enrollment growing rapidly; White enrollment declining",
#     x = "Year",
#     y = "Students (thousands)",
#     color = "Race/Ethnicity"
#   ) +
#   theme(legend.position = "bottom")

## ----regional-data------------------------------------------------------------
# enr_regional <- fetch_enr_multi(c(2015, 2024))
# 
# middle_tn <- c("Davidson", "Williamson", "Rutherford", "Wilson", "Sumner")
# memphis_area <- c("Shelby")
# 
# regional <- enr_regional %>%
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
#   mutate(region = case_when(
#     grepl(paste(middle_tn, collapse = "|"), district_name) ~ "Middle TN",
#     grepl(paste(memphis_area, collapse = "|"), district_name) ~ "Memphis Area",
#     TRUE ~ "Other"
#   )) %>%
#   filter(region %in% c("Middle TN", "Memphis Area")) %>%
#   group_by(end_year, region) %>%
#   summarize(total = sum(n_students, na.rm = TRUE), .groups = "drop")
# 
# regional

## ----regional-chart-----------------------------------------------------------
# regional_wide <- regional %>%
#   pivot_wider(names_from = end_year, values_from = total) %>%
#   mutate(pct_change = round((`2024` - `2015`) / `2015` * 100, 1))
# 
# ggplot(regional, aes(x = factor(end_year), y = total / 1000, fill = region)) +
#   geom_col(position = "dodge", width = 0.7) +
#   geom_text(
#     aes(label = scales::comma(total)),
#     position = position_dodge(width = 0.7),
#     vjust = -0.5,
#     size = 3.5
#   ) +
#   scale_fill_manual(values = c("Middle TN" = "#41AB5D", "Memphis Area" = "#EF6548")) +
#   scale_y_continuous(
#     labels = scales::label_number(suffix = "K"),
#     expand = expansion(mult = c(0, 0.15))
#   ) +
#   labs(
#     title = "Middle Tennessee vs Memphis Area Enrollment",
#     subtitle = "Nashville suburbs growing rapidly; Shelby County declining",
#     x = "Year",
#     y = "Students (thousands)",
#     fill = "Region"
#   ) +
#   theme(legend.position = "bottom")

## ----growth-data--------------------------------------------------------------
# enr_growth <- fetch_enr_multi(c(2015, 2024))
# 
# growth <- enr_growth %>%
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
#   select(end_year, district_name, n_students) %>%
#   pivot_wider(names_from = end_year, values_from = n_students) %>%
#   filter(!is.na(`2015`) & !is.na(`2024`) & `2015` > 5000) %>%
#   mutate(
#     change = `2024` - `2015`,
#     pct_change = round(change / `2015` * 100, 1)
#   ) %>%
#   arrange(desc(pct_change)) %>%
#   head(10)
# 
# growth

## ----growth-chart-------------------------------------------------------------
# growth %>%
#   mutate(district_name = reorder(district_name, pct_change)) %>%
#   ggplot(aes(x = pct_change, y = district_name)) +
#   geom_col(fill = "#41AB5D") +
#   geom_text(aes(label = paste0("+", pct_change, "%")), hjust = -0.1, size = 3.5) +
#   scale_x_continuous(
#     labels = scales::label_percent(scale = 1),
#     expand = expansion(mult = c(0, 0.15))
#   ) +
#   labs(
#     title = "Fastest-Growing Tennessee Districts (2015-2024)",
#     subtitle = "Minimum 5,000 students in 2015",
#     x = "Enrollment Growth",
#     y = NULL
#   )

## ----el-data------------------------------------------------------------------
# enr_el <- fetch_enr_multi(c(2014, 2019, 2024))
# 
# el_trend <- enr_el %>%
#   filter(is_state, grade_level == "TOTAL", subgroup == "lep") %>%
#   select(end_year, n_students) %>%
#   mutate(pct_change = round((n_students / first(n_students) - 1) * 100, 1))
# 
# el_trend

## ----el-chart-----------------------------------------------------------------
# ggplot(el_trend, aes(x = end_year, y = n_students / 1000)) +
#   geom_area(fill = "#41AB5D", alpha = 0.3) +
#   geom_line(color = "#41AB5D", linewidth = 1.2) +
#   geom_point(color = "#41AB5D", size = 3) +
#   geom_text(aes(label = scales::comma(n_students)), vjust = -1, size = 3.5) +
#   scale_y_continuous(
#     labels = scales::label_number(suffix = "K"),
#     limits = c(0, NA)
#   ) +
#   labs(
#     title = "English Learners in Tennessee Schools",
#     subtitle = "Rapid growth over the past decade",
#     x = "Year",
#     y = "English Learners (thousands)"
#   )

