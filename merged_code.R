```{r}
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
```{r}
# Import both datasets
Institution <- read_excel(
  "Most-Recent-Cohorts-Institution(CaOnly)(Sheet1).xlsx"
)

Field <- read_excel(
  "Most-Recent-Cohorts-Field-of-Study.xlsx"
)

# Institution-level completion data
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
      ~ suppressWarnings(as.numeric(as.character(.x)))
    )
  ) |>
  filter(
    if_all(-INSTNM, ~ !is.na(.x))
  ) |>
  mutate(
    Completion_Gap = (
      HI_INC_COMP_ORIG_YR4_RT -
      LO_INC_COMP_ORIG_YR4_RT
    ) * 100
  )

# Filter bachelor's degree programs
# and clean Pell earnings variables
BA_Earnings <- Field |>
  filter(CREDDESC == "Bachelor's Degree") |>
  mutate(
    across(
      c(
        EARN_PELL_WNE_MDN_5YR,
        EARN_NOPELL_WNE_MDN_5YR
      ),
      ~ suppressWarnings(as.numeric(as.character(.x)))
    )
  ) |>
  filter(
    !is.na(EARN_PELL_WNE_MDN_5YR),
    !is.na(EARN_NOPELL_WNE_MDN_5YR)
  ) |>
  mutate(
    Earnings_Gap =
      EARN_NOPELL_WNE_MDN_5YR -
      EARN_PELL_WNE_MDN_5YR
  )

# Summarize bachelor's earnings by institution
BA_Earnings_Inst <- BA_Earnings |>
  group_by(INSTNM) |>
  summarise(
    BA_Programs = n(),
    Pell_Earnings = median(EARN_PELL_WNE_MDN_5YR),
    NonPell_Earnings = median(EARN_NOPELL_WNE_MDN_5YR),
    Earnings_Gap = median(Earnings_Gap),
    .groups = "drop"
  )

# Match the two datasets by institution name
Combined_Analysis <- inner_join(
  Income_Completion,
  BA_Earnings_Inst,
  by = "INSTNM"
)

# Examine the relationship
ggplot(
  Combined_Analysis,
  aes(
    x = Completion_Gap,
    y = Earnings_Gap
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray60"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray60"
  ) +
  geom_point(
    color = "#2171B5",
    alpha = 0.75,
    size = 2.8
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "#084594",
    fill = "#9ECAE1",
    se = TRUE
  ) +
  labs(
    title = "College Completion and Earnings Disparities",
    subtitle = paste(
      nrow(Combined_Analysis),
      "matched California institutions | Bachelor's programs"
    ),
    x = "High- vs. Low-Income Completion Gap (Percentage Points)",
    y = "Non-Pell vs. Pell Earnings Gap ($)",
    caption = paste(
      "Source: College Scorecard.",
      "Earnings represent median program-level gaps",
      "summarized by institution."
    )
  ) +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

cor.test(
  Combined_Analysis$Completion_Gap,
  Combined_Analysis$Earnings_Gap,
  method = "spearman",
  exact = FALSE
)
```
```{r}

Field <- read_excel(
  "Most-Recent-Cohorts-Field-of-Study.xlsx"
)

BA_Earnings <- Field |>
  filter(CREDDESC == "Bachelor's Degree") |>
  mutate(
    across(
      c(
        EARN_PELL_WNE_MDN_5YR,
        EARN_NOPELL_WNE_MDN_5YR
      ),
      ~ suppressWarnings(as.numeric(as.character(.x)))
    )
  ) |>
  filter(
    !is.na(EARN_PELL_WNE_MDN_5YR),
    !is.na(EARN_NOPELL_WNE_MDN_5YR)
  )

Earnings_Long <- BA_Earnings |>
  select(
    EARN_PELL_WNE_MDN_5YR,
    EARN_NOPELL_WNE_MDN_5YR
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "Pell_Status",
    values_to = "Earnings"
  ) |>
  mutate(
    Pell_Status = recode(
      Pell_Status,
      EARN_PELL_WNE_MDN_5YR = "Pell recipients",
      EARN_NOPELL_WNE_MDN_5YR = "Non-Pell recipients"
    )
  )

ggplot(
  Earnings_Long,
  aes(
    x = Earnings,
    color = Pell_Status,
    fill = Pell_Status
  )
) +
  geom_density(
    alpha = 0.15,
    linewidth = 1.2,
    adjust = 1
  ) +
  scale_color_manual(
    values = c(
      "Pell recipients" = "#2171B5",
      "Non-Pell recipients" = "#E68632"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Pell recipients" = "#2171B5",
      "Non-Pell recipients" = "#E68632"
    )
  ) +
  scale_x_continuous(
    labels = scales::label_dollar()
  ) +
  labs(
    title = "Distribution of Five-Year Median Earnings by Pell Grant Status",
    x = "Five-Year Median Earnings ($)",
    y = "Density",
    color = "Pell Status",
    fill = "Pell Status",
    caption = paste(
      "Source: College Scorecard.",
      "Bachelor's degree programs with both earnings values available."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold"
    ),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

nrow(BA_Earnings)

```
