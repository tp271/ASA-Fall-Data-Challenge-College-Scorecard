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

```{r}
names(my_data)

head(my_data)

str(my_data)
```


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

```{r}
collapsibleTree(
  tree_data,
  hierarchy = c("CONTROL", "CIPDESC"),
  fillByLevel = TRUE
) 
```
