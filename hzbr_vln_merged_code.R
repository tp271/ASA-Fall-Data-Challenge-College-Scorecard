---
title: "STAT 310 Test Code, merged"
format:
  html:
    embed-resources: true
execute:
  warning: false
  echo: true
  error: false
---

```{r}
#| label: setup
#| include: false
library(igraph)
library(ggraph)
library(ggplot2)
library(collapsibleTree)
library(readxl)
library(dplyr)
library(tidyr)

my_data <- read_excel("Most-Recent-Cohorts-Field-of-Study.xlsx")
```

## Inspect the data

```{r}
names(my_data)
head(my_data)
str(my_data)
```

## Prepare the data

```{r}
flat_data <- as.data.frame(my_data)

hzbr_data <- flat_data %>%
  filter(!is.na(CONTROL), !is.na(CIPDESC)) %>%
  filter(grepl("Mathematics", CIPDESC, ignore.case = TRUE)) %>%
  mutate(
    CIPDESC = sub(".", "", CIPDESC),
    CIPDESC = trimws(CIPDESC)
  )
```

## Mathematics Programs: Debt-to-Earnings Ratio

```{r}
hzbr_tp20_debt2income_rt <- hzbr_data %>%
  mutate(
    debt = as.numeric(DEBT_ALL_STGP_ANY_MDN),
    earnings = as.numeric(EARN_MDN_4YR),
    debt_income_ratio = debt / earnings
  ) %>%
  filter(
    !is.na(INSTNM),
    is.finite(debt_income_ratio),
    debt >= 0,
    earnings > 0
  ) %>%
  group_by(INSTNM, CONTROL) %>%
  summarise(
    debt_income_ratio = median(debt_income_ratio, na.rm = TRUE),
    .groups = "drop" # Added quotes here
  ) %>%
  slice_min(order_by = debt_income_ratio, n = 20, with_ties = FALSE) %>%
  ggplot(aes(x = debt_income_ratio, y = reorder(INSTNM, debt_income_ratio))) +
  geom_col(fill = "#377EB8", width = 0.7) +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(
    title = "Mathematics Programs: Debt vs. Earnings",
    subtitle = "20 schools with the lowest debt-to-earnings ratios",
    x = "Debt-to-Earnings Ratio",
    y = "School",
    caption = "Source: Most-Recent-Cohorts-Field-of-Study dataset"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

hzbr_tp20_debt2income_rt
```

## School Comparisons: Violin Plots

```{r}
# Moved inside chunk and corrected to read_excel
Field <- read_excel("Most-Recent-Cohorts-Field-of-Study.xlsx") 

# instead of UNIT ID, change to institution name
# Switch UNITID for INSTNM
# Switch 243744 for Stanford University
# Switch 110635 for University of California-Berkeley

Field |> 
  drop_na(DEBT_MALE_PP_ANY_MDN, INSTNM) |> 
  filter(INSTNM %in% c("Stanford University", "University of California-Berkeley")) |> 
  mutate(
    INSTNM = factor(INSTNM, levels = c("Stanford University", "University of California-Berkeley"))
  ) |> 
  ggplot(aes(x = INSTNM, y = DEBT_MALE_PP_ANY_MDN)) + 
  geom_violin(aes(fill = INSTNM), trim = FALSE, alpha = 0.25) + # Changed fill to INSTNM
  scale_fill_viridis_d() + 
  labs(
    x = "Institution", 
    y = "Median Male Debt", 
    title = "Median Parent PLUS loan debt disbursed to males at Stanford and Cal", 
    caption = "Source: Most-Recent-Cohorts-Field-of-Study.csv"
  ) + 
  theme_minimal() + 
  geom_jitter(aes(color = INSTNM), height = 0.25) # Changed color to INSTNM
```

```{r}
# Switch DEBT_MALE_PP_ANY_MDN for DEBT_ALL_PP_ANY_MDN
Field |> 
  drop_na(DEBT_ALL_PP_ANY_MDN, INSTNM) |> 
  filter(INSTNM %in% c("Stanford University", "University of California-Berkeley")) |> 
  mutate(
    INSTNM = factor(INSTNM, levels = c("Stanford University", "University of California-Berkeley"))
  ) |> 
  ggplot(aes(x = INSTNM, y = DEBT_ALL_PP_ANY_MDN)) + 
  geom_violin(aes(fill = INSTNM), trim = FALSE, alpha = 0.25) + # Changed fill to INSTNM
  scale_fill_viridis_d() + 
  labs(
    x = "Institution", 
    y = "Median Total Debt", 
    title = "Median Debt for all..."
  ) + 
  theme_minimal() + 
  geom_jitter(aes(color = INSTNM), height = 0.25) # Changed color to INSTNM
```


Field <- read.excel("Most-Recent-Cohorts-Field-of-Study.xlsx")
```

```{r}
#instead of UNIT ID, change to institution name
#Switch UNITID for INSTNM
#Switch 243744 for "Stanford University"
#Switch 110635 for "University of California-Berkeley"
#Some of the code in this chunk was produced with the aid of ChatGPT
Field |> 
  drop_na(DEBT_MALE_PP_ANY_MDN, INSTNM) |> 
  filter(INSTNM %in% c("Stanford University", "University of California-Berkeley")) |> 
  mutate(
    INSTNM = factor(
      INSTNM,
      levels = c("Stanford University", "University of California-Berkeley")
    )) |> 
  ggplot(aes(
    x = INSTNM,
    y = DEBT_MALE_PP_ANY_MDN)) +
  geom_violin(aes(fill=DEBT_MALE_PP_ANY_MDN),
              trim = FALSE, 
              alpha=0.25) +
  scale_fill_viridis_d() +
  labs(
    x = "Institution",
    y = "Median Male Debt", 
    title= "Median Parent PLUS loan debt disbursed to males at Stanford and Cal",
    caption= "Source: Most-Recent-Cohorts-Field-of-Study.csv")+
  theme_minimal()+
  geom_jitter(aes(color=DEBT_MALE_PP_ANY_MDN), height=0.25)
```
```{r}
# Switch DEBT_MALE_PP_ANY_MDN for DEBT_ALL_PP_ANY_MDN
Field |> 
  drop_na(DEBT_ALL_PP_ANY_MDN, INSTNM) |> 
  filter(INSTNM %in% c("Stanford University", "University of California-Berkeley")) |> 
  mutate(
    INSTNM = factor(
      INSTNM,
      levels = c("Stanford University", "University of California-Berkeley")
    )) |> 
  ggplot(aes(
    x = INSTNM,
    y = DEBT_ALL_PP_ANY_MDN)) +
  geom_violin(aes(fill=DEBT_ALL_PP_ANY_MDN),
              trim = FALSE, 
              alpha=0.25) +
  scale_fill_viridis_d() +
  labs(
    x = "Institution",
    y = "Median Male Debt", title= "Median Debt for male...")+
  theme_minimal()+
  geom_jitter(aes(color=DEBT_ALL_PP_ANY_MDN), height=0.25)
```

