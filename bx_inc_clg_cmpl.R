```{r}
#| label: final-income-completion
#| fig-cap: "Four-Year College Completion Rates by Student Income Group"
#| fig-width: 11
#| fig-height: 6.5
#| warning: false

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Import institution dataset
Institution <- read_excel(
  "Most-Recent-Cohorts-Institution(CaOnly)(Sheet1).xlsx"
)

# Clean and select completion variables
Income_Completion <- Institution |>
  select(
    INSTNM,
    LO_INC_COMP_ORIG_YR4_RT,
    MD_INC_COMP_ORIG_YR4_RT,
    HI_INC_COMP_ORIG_YR4_RT
  ) |>
  mutate(
    across(
      -INSTNM,
      ~ suppressWarnings(as.numeric(as.character(.x))) * 100
    )
  ) |>
  filter(
    if_all(-INSTNM, ~ !is.na(.x))
  )

# Reshape data
Income_Completion_Long <- Income_Completion |>
  pivot_longer(
    cols = -INSTNM,
    names_to = "Income_Group",
    values_to = "Completion_Rate"
  ) |>
  mutate(
    Income_Group = factor(
      Income_Group,
      levels = c(
        "LO_INC_COMP_ORIG_YR4_RT",
        "MD_INC_COMP_ORIG_YR4_RT",
        "HI_INC_COMP_ORIG_YR4_RT"
      ),
      labels = c(
        "Low Income",
        "Middle Income",
        "High Income"
      )
    )
  )

# Final visualization
ggplot(
  Income_Completion_Long,
  aes(
    x = Income_Group,
    y = Completion_Rate,
    fill = Income_Group
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.85,
    color = "black"
  ) +
  geom_jitter(
    width = 0.18,
    alpha = 0.15,
    size = 1.5,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Low Income" = "#C6DBEF",
      "Middle Income" = "#6BAED6",
      "High Income" = "#2171B5"
    )
  ) +
  labs(
    title = "Income and College Completion",
    subtitle = paste(
      "Four-year completion rates across",
      nrow(Income_Completion),
      "California institutions"
    ),
    x = "Student Income Group",
    y = "Four-Year Completion Rate (%)",
    caption = "Source: College Scorecard, California institution-level data"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 20, 15, 15)
  )
```

