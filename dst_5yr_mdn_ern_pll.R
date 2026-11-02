```{r}
#| label: pell-earnings-density
#| fig-cap: "Distribution of Five-Year Median Earnings by Pell Grant Status"
#| fig-width: 10
#| fig-height: 6
#| warning: false

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

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
