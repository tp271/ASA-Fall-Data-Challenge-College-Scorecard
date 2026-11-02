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

# Load data into 'Field_clean' directly to fix the object naming mismatch
Field_raw <- read_excel("Most-Recent-Cohorts-Field-of-Study.xlsx")

# Consolidate all cleanup tasks into a single data frame
Field_clean <- Field_raw |> 
  mutate(
    DEBT_MALE_PP_ANY_MDN = as.numeric(na_if(as.character(DEBT_MALE_PP_ANY_MDN), "PS")),
    EARN_MDN_HI_2YR      = as.numeric(na_if(as.character(EARN_MDN_HI_2YR), "PS")),
    EARN_MDN_5YR         = as.numeric(na_if(as.character(EARN_MDN_5YR), "PS"))
  )
```

## 1. Violin Plot: Median Earnings 2 Years After Completion

```{r}
Field_clean |> 
  drop_na(EARN_MDN_HI_2YR, CONTROL) |> 
  ggplot(aes(x = CONTROL, y = EARN_MDN_HI_2YR)) + 
  geom_violin(aes(fill = CONTROL), trim = FALSE, alpha = 0.25) + 
  scale_fill_viridis_d() + 
  labs(
    x = "Institution Type", 
    y = "Median Earnings ($)", 
    title = "Median Earnings of Graduates 2 Years After Completion", 
    caption = "Source: Most-Recent-Cohorts-Field-of-Study.xlsx"
  ) + 
  theme_minimal()
```

## 2. Violin Plot: Median Earnings 5 Years After Completion

```{r}
Field_clean |> 
  drop_na(EARN_MDN_5YR, CONTROL) |> 
  ggplot(aes(x = CONTROL, y = EARN_MDN_5YR)) + 
  geom_violin(aes(fill = CONTROL), trim = FALSE, alpha = 0.25) + 
  scale_fill_viridis_d() + 
  labs(
    x = "Institution Type", 
    y = "Median Earnings ($)", 
    title = "Median Earnings of Graduates 5 Years After Completing by Institution Type", 
    caption = "Source: Most-Recent-Cohorts-Field-of-Study.xlsx"
  ) + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 10))
```

## 3. Density Plot: Median Earnings 2 Years After Completion

```{r}
Field_clean |> 
  drop_na(EARN_MDN_HI_2YR, CONTROL) |> 
  ggplot(aes(x = EARN_MDN_HI_2YR)) + 
  geom_density(fill = "steelblue", alpha = 0.4) + 
  facet_wrap(~ CONTROL) + 
  labs(
    x = "Median Earnings ($)", 
    y = "Density", 
    title = "Distribution of Median Earnings 2 Years After Completion", 
    caption = "Source: Most-Recent-Cohorts-Field-of-Study.xlsx"
  ) + 
  theme_minimal()
```

## 4. Density Plot: Median Earnings 5 Years After Completion

```{r}
Field_clean |> 
  drop_na(EARN_MDN_5YR, CONTROL) |> 
  ggplot(aes(x = EARN_MDN_5YR)) + 
  geom_density(fill = "steelblue", alpha = 0.4) + 
  facet_wrap(~ CONTROL) + 
  labs(
    x = "Median Earnings ($)", 
    y = "Density", 
    title = "Distribution of Median Earnings 5 Years After Completion", 
    caption = "Source: Most-Recent-Cohorts-Field-of-Study.xlsx"
  ) + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 11))
```

