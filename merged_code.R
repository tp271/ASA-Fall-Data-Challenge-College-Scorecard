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
  #Selects desired variables from Institution
  select( 
    #Keep institution name
    INSTNM,
    #Keep lowincome completion rate
    LO_INC_COMP_ORIG_YR4_RT,
    #Keep middle income completion rate
    MD_INC_COMP_ORIG_YR4_RT,
    #Keep high income completion rate
    HI_INC_COMP_ORIG_YR4_RT
  ) |>
#Modify selected variable columns
  mutate(
    #Apply same modification to accross multiple columns
    across(
      #exclude institution name
      -INSTNM,
      # Convert each value to numeric and multiply by 100; suppress conversion warnings
      ~ suppressWarnings(as.numeric(as.character(.x))) * 100
    )
    #Pipe to pass rsult to next step
  ) |>
#Keep Rows that meet the following condition(s)
  filter(
    #Keep rows where all selected rate columns are non-missing
    if_all(-INSTNM, ~ !is.na(.x))
  )

# Reshape data
Income_Completion_Long <- Income_Completion |>
#Convert income column group columns to long format
  pivot_longer(
    #Pivot every column group except institution name
    cols = -INSTNM,
    # Put the original column names into a new Income_Group column
    names_to = "Income_Group",
    # Put the column values into a new Completion_Rate column
    values_to = "Completion_Rate"
    #Pipe results to next step
  ) |>
#Modify the newly reshaped data
  mutate(
    # Convert Income_Group to a factor so its order and labels can be set
    Income_Group = factor(
      # Use the current Income_Group values
      Income_Group,
      #Sets levels of variable
      levels = c(
        # Set low income group first
        "LO_INC_COMP_ORIG_YR4_RT",
        # Set middle income group second
        "MD_INC_COMP_ORIG_YR4_RT",
        # Set high income group third
        "HI_INC_COMP_ORIG_YR4_RT"
      ),
      # Assign readable labels to the factor levels in the same order
      labels = c(
        "Low Income",
        "Middle Income",
        "High Income"
      ) # Finish specifying the labels
    ) # Finish converting Income_Group to a factor
  ) # Finish data modification

# Final visualization
#start by building plot
ggplot(
  # Use the reshaped income completion data
  Income_Completion_Long,
  #Create aesthetic mapping
  aes(
    #Map Income_Group to x-axis
    x = Income_Group,
    # Map Completion_Rate to y-axis
    y = Completion_Rate,
    # Fill geometry according to Income_Group
    fill = Income_Group
  )
  #Add a new map layer
) +
# Create a boxplot for each income group
  geom_boxplot(
    #Set the width of each boxplot
    width = 0.55,
    #Hide outliers
    outlier.shape = NA,
    #Set boxplot transparency
    alpha = 0.85,
    #Set boc outlines to black
    color = "black"
    #Finish and add a new layer
  ) +
#Add observations as jittered points
  geom_jitter(
    # Spread points horizontally to reduce overlap
    width = 0.18,
    # Make the points mostly transparent
    alpha = 0.15,
    #Set the point size
    size = 1.5,
    #Set the points to black
    color = "black"
    #Finish layer and add new layer
  ) +
# Set custom fill colors for the income groups
  scale_fill_manual(
    # Match each income group to a color
    values = c(
      #Set low income to "#C6DBEF"
      "Low Income" = "#C6DBEF",
      #Set middle income to "#6BAED6"
      "Middle Income" = "#6BAED6",
      #Set high income to "#2171B5"
      "High Income" = "#2171B5"
    )
    #Finish layer and add new layer
  ) +
#Add labels to plot
  labs(
    #Create title for plot
    title = "Income and College Completion",
    #Create subtitle for plot
    subtitle = paste(
      #Beginning of Subtitle
      "Four-year completion rates across",
      #paste number of rows (observations) of California institutions
      nrow(Income_Completion),
      #Finish rest of subtitle
      "California institutions"
    ),
    #Set x-axis title
    x = "Student Income Group",
    #set y-acis title
    y = "Four-Year Completion Rate (%)",
    #Create caotion for plot
    caption = "Source: College Scorecard. Most-Recent-Cohorts-Institution"
    #Finish layer and add new layer
  ) +
# Set the y-axis scale
  scale_y_continuous(
    # Place y-axis tick marks from 0 to 100 by tens
    breaks = seq(0, 100, 10),
    # Set the y-axis range from 0 to 100
    limits = c(0, 100)
    #finish layer and add new layer
  ) +
#Set plot theme to minimal and add new layer
  theme_minimal() +
#Make specific modification to plots theme
  theme(
    #set text size for title and embolden
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    #Set title position
    plot.title.position = "plot",
    #Set text size for subtitle
    plot.subtitle = element_text(size = 12),
    #Make both axis titles bold
    axis.title = element_text(face = "bold"),
    #set axis title sizes to 11
    axis.text = element_text(size = 11),
    # Remove minor grid lines
    panel.grid.minor = element_blank(),
    #Remove legend from visualization
    legend.position = "none",
    # Set plot margins: top, right, bottom, and left
    plot.margin = margin(15, 20, 15, 15)
  ) # Finish customizing the plot theme
```
```{r}
#| label: completion-earnings-scatterplot
#| fig-width: 10
#| fig-height: 6
#| warning: false

#Use Earnings_Comparison and pipe to next instructions
Earnings_Comparison |>
#Start building the plot
  ggplot(
    #Set aesthetic mapping
    aes(
      # Put the completion gap on the x-axis
      x = Completion_Gap,
      # Put earnings on the y-axis
      y = Earnings,
      # Color points and trendlines by Pell Status
      color = Pell_Status
    )
    #Finish layer and add new layer
  ) +
# Add a point for each observation
  geom_point(
    #Set size of poonts
    size = 2.8,
    #Set transparency of points
    alpha = 0.75
    #Finish layer and add new layer
  ) +
#Add trendlines
  geom_smooth(
    # Fit a linear model to create each trendline
    method = lm,
    # Hide the confidence interval around each trendline
    se = FALSE,
    ## Set the trendline thickness
    linewidth = 1.2
    #Finish layer and add new layer
  ) +
# Set custom colors for the Pell Status groups
  scale_color_manual(
    # Match each group label to a color
    values = c(
      #Set pell recipient color to "#2171B5"
      "Pell Recipients" = "#2171B5",
      #Set non-pell recipient color to "#E68632"
      "Non-Pell Recipients" = "#E68632"
    )
    #Finish layer and add new layer
  ) +
# Customize the continuous y-axis
  scale_y_continuous(
    ## Format y-axis values as dollar amounts
    labels = scales::label_dollar()
    #Finish layer and add new layer
  ) +
#Add labels to plot
  labs(
    #Create title
    title = "College Completion and Earnings by Pell Status",
    #Create subtitle
    subtitle = paste(
      #Insert the number of rows in Combined_Analysis
      nrow(Combined_Analysis),
      #add rest of subtitle
      "matched California institutions"
    ),
    #Create x-axis title
    x = "High- vs. Low-Income Completion Gap (Percentage Points)",
    #Create y-axis title
    y = "Five-Year Median Earnings ($)",
    # Set the title of the color legend
    color = "Pell Status",
    #Create Caption
    caption = paste(
      "Source: College Scorecard. Most-Recent-Cohorts-Field-of-Study"
    )
    #Finish layer and add new layer
  ) +
# Apply a black-and-white theme and add new layer
  theme_bw() +
#Create manual theme elements
  theme(
    #Bold title and set text size
    plot.title = element_text(face = "bold", size = 15),
    #Set tet size of title
    plot.subtitle = element_text(size = 11),
    #Bold axis titles
    axis.title = element_text(face = "bold"),
    #Bold Legend title
    legend.title = element_text(face = "bold"),
    #Remove minor grid lines
    panel.grid.minor = element_blank()
    #Finish Customization
  )
```
```{r}
#| label: pell-earnings-density

# Read the Excel file and save its contents as Fiel
Field <- read_excel(
  "Most-Recent-Cohorts-Field-of-Study.xlsx"
)

# Start with Field and save the cleaned result as BA_Earnings
BA_Earnings <- Field |>
# Keep rows for bachelor's degree programs
  filter(CREDDESC == "Bachelor's Degree") |>
 # Modify the earnings columns
  mutate(
    # Apply the same conversion to multiple columns
    across(
       # List the columns to convert
      c(
        # Include five-year median earnings for Pell recipients
        EARN_PELL_WNE_MDN_5YR,
        #Include five-year median earnings for non-Pell recipients
        EARN_NOPELL_WNE_MDN_5YR
        # Finish listing the columns
      ),
      ## Convert each value to numeric and suppress conversion warnings
      ~ suppressWarnings(as.numeric(as.character(.x)))
      # Finish applying the conversion
    )
    # Finish modifying the columns and pass the data to the next step
  ) |>
# Keep only rows that meet both conditions below
  filter(
    #Keep rows with a non-missing Pell recipient earnings value
    !is.na(EARN_PELL_WNE_MDN_5YR),
    #Keep rows with a non-missing non-Pell recipient earnings value
    !is.na(EARN_NOPELL_WNE_MDN_5YR)
    # Finish filtering
  )

# Start with BA_Earnings and save the reshaped data as Earnings_Long
Earnings_Long <- BA_Earnings |>
# Keep only the two earnings columns
  select(
    # Keep the Pell recipient earnings column
    EARN_PELL_WNE_MDN_5YR,
    # Keep the non-Pell recipient earnings column
    EARN_NOPELL_WNE_MDN_5YR
    #Finish and pipe to next step
  ) |>
# Reshape the earnings columns to long format
  pivot_longer(
    # Pivot all remaining columns
    cols = everything(),
    # Put the original column names in a new Pell_Status column
    names_to = "Pell_Status",
    # Put the earnings values in a new Earnings column
    values_to = "Earnings"
    #Finish step and pipe to next step
  ) |>
#Modify the Pell_Status column
  mutate(
    # Replace the original column names with readable labels
    Pell_Status = recode(
      # Recode values in the Pell_Status column
      Pell_Status,
      # Label Pell recipient earnings
      EARN_PELL_WNE_MDN_5YR = "Pell recipients",
      # Label non-Pell recipient earnings
      EARN_NOPELL_WNE_MDN_5YR = "Non-Pell recipients"
      #Finish recoding the values
    )
    #Finish modifying the data
  )

#Start building the plot
ggplot(
  #Use Earnings_Long
  Earnings_Long,
  #Set aesthetic mapping
  aes(
    #Set Earnings to x-axis
    x = Earnings,
    #Color the density curves by Pell status 
    color = Pell_Status,
    # Fill the density curves by Pell status
    fill = Pell_Status
    #Finish specifying the aesthetic mappings
  )
  #Finish layer and add new layer
) +
# Add a density curve for each Pell status group
  geom_density(
    #Set transparency
    alpha = 0.15,
    # Set the curve outline thickness
    linewidth = 1.2,
    #Use the default bandwidth adjustment for the density curves
    adjust = 1
    #Finish layer and add new layer
  ) +
#Set custom outline colors for the density curves
  scale_color_manual(
    #Match each Pell status label to a color
    values = c(
      #Set pell recipient to color "#2171B5"
      "Pell recipients" = "#2171B5",
      #set non-pell recipient to color "#E68632"
      "Non-Pell recipients" = "#E68632"
      #Finish specifying the outline colors
    )
    #Finish layer and add new layer
  ) +
#Set custom fill colors for the density curves
  scale_fill_manual(
    #Match each Pell status label to a fill color
    values = c(
      #Set pell recipient color to "#2171B5"
      "Pell recipients" = "#2171B5",
      #Set non-pell recipient color to "#E68632"
      "Non-Pell recipients" = "#E68632"
      # Finish specifying the fill colors
    )
    #Finish layer and add new layer
  ) +
# Customize the continuous x-axis
  scale_x_continuous(
    #Format x-axis values as dollar amounts
    labels = scales::label_dollar()
    #Finish layer and add new layer
  ) +
#Create labels for plot
  labs(
    #Create title
    title = "Five-Year Earnings by Pell Status",
    #Create x-axis title
    x = "Five-Year Median Earnings ($)",
    #Create y-axis title
    y = "Density",
    # Set the color legend title
    color = "Pell Status",
    # Set the fill legend title
    fill = "Pell Status",
    #Create caption
    caption = paste(
      "Source: College Scorecard.",
      "Most-Recent-Cohorts-Field-of-Study"
      #Finish building the caption
    )
    #Finish layer and add new layer
  ) +
#Set plot theme to minimal
  theme_minimal() +
#Create custom plot theme elements
  theme(
    #Customize theme elemtns for plot title
    plot.title = element_text(
      #set text size
      #Set boldness
      face = "bold",
      # Center the title
      hjust = 0.5
      # Finish styling the title
    ),
    # Make both axis titles bold
    axis.title = element_text(face = "bold"),
    #Make legend title bold
    legend.title = element_text(face = "bold"),
    #Remove minor gridlines
    panel.grid.minor = element_blank(),
    # Align the caption to the left
    plot.caption = element_text(hjust = 0)
    # Finish customizing the plot theme
  )
```

