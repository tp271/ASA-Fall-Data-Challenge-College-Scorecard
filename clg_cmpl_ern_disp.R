```{r}
#| label: completion-earnings-relationship
#| fig-cap: "Relationship Between Income-Based Completion Gaps and Pell Earnings Gaps"
#| fig-width: 10
#| fig-height: 6
#| warning: false

library(readxl)
library(dplyr)
library(ggplot2)

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
