---
title: "STAT 310 Test Code, Premerged"
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

tree_data <- flat_data %>%
  filter(!is.na(CONTROL), !is.na(CIPDESC)) %>%
  filter(
    grepl("Mathematics", CIPDESC, ignore.case = TRUE)
  ) %>%
  mutate(
    CIPDESC = sub(".", "", CIPDESC),
    CIPDESC = trimws(CIPDESC)
  )
```

## Mathematics Programs: Debt-to-Earnings Ratio

```{r}
tree_plot <- tree_data %>%
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
    .groups = "drop"
  ) %>%
  slice_min(
    order_by = debt_income_ratio,
    n = 20,
    with_ties = FALSE
  ) %>%
  ggplot(
    aes(
      x = debt_income_ratio,
      y = reorder(INSTNM, debt_income_ratio)
    )
  ) +
  geom_col(fill = "#377EB8", width = 0.7) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
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

tree_plot
```

```{r}
#| label: save-png
#| include: false
ggsave(
  "graphname.png",
  plot = tree_plot,
  device = "png",
  width = 9,
  height = 7,
  dpi = 200
)
```

