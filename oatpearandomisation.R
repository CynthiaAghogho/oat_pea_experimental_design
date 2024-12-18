
---
  title: "oatpea"
author: "Wolfe lab"
date: "2024-12-09"
output: html_document
---
  ```{r}
# Load required library
library(dplyr)
library(magrittr)
library(ggplot2)

#inventory is the name of the file containing expected weight of seeds for pea and oat
inventory <- read.csv("oatpeadesign.csv") 

# Seed weights for 60-ft plots (as recommended)
gSeedPer6Plot_pea <- 59
gSeedPer6Plot_oat <- 24

inventory %<>% 
  mutate(
    N_6ftPlotsPossible = case_when(
      crop == "oat" ~ weight/gSeedPer6Plot_oat,  # Calculate for oats
      crop == "pea" ~  weight / gSeedPer6Plot_pea  # Calculate for peas
    ),
    # Assign values based on how many plots can be accommodated by seeds provided
    PlotCategory = case_when(
      N_6ftPlotsPossible >= 6 ~ "5", # Can accommodate at least 10 plots
      N_6ftPlotsPossible >= 2  ~ "2", # Can accommodate at least 2 plots but less than 10
      TRUE ~ "1"                      # Otherwise, it accommodates less than 2 plots
    )
  )
head(inventory) # Check the result
```
```{r}
set.seed(1234) 
n_peas <- 280      # Number of pea genotypes
n_oats <- 239      # Number of oat genotypes
total_plots <- 2000  # Total plots across 5 locations
plots_per_location <- 400
locations <- 1:5

pea_oat_combos <- expand.grid(
  Pea = inventory %>%
    filter(crop == "pea") %>%
    pull(Name),  # Extracts Name column as a vector
  Oat = inventory %>%
    filter(crop == "oat") %>%
    pull(Name)   # Extracts Name column as a vector
)

# Shuffle combinations
set.seed(13255)
pea_oat_combos <- pea_oat_combos[sample(1:nrow(pea_oat_combos)), ] # all 66,920 possible combinations of pea and oat

# Allocate plots: 80% unique, 10% within-location, 10% within-and-across
n_unique <- round(0.80 * total_plots)      # 1600 plots
n_within_loc <- round(0.10 * total_plots)  # 200 plots
n_across_loc <- total_plots - n_unique - n_within_loc  # 200 plots
```


```{r}
#Assign 80% unique combinations 
unique_combos <- pea_oat_combos[1:n_unique, ]  %>% mutate(
  combo = paste(Pea, Oat, sep = "+"),
  category = "unique_combos", Replication = "Rep 0") # add colum combo, category and replication 
unique_combos$Location <- rep(locations, length.out = n_unique) # add location

table(unique_combos$Location) # check the number of combo per location

unique_pea_count <- unique_combos %>% distinct(Pea) %>% nrow() # check the number of pea accessions 
unique_oat_count <- unique_combos %>% distinct(Oat) %>% nrow() # check the number of oat accessions 

pea_counts <- unique_combos %>%
  group_by(Pea) %>%
  summarize(Count = dplyr::n()) #check the how many times a pea accessions is appearing

oat_counts <- unique_combos %>%
  group_by(Oat) %>%
  summarize(Count = n()) # check the how many times an oat accessions is appearing
```


```{r}
# Assign 10% replicated within-location 
# Generate the base subset for the 66,920 possible combinations
# Create a base of 30 unique combos (6 per location, replicated in each of 2 reps)
set.seed(123)
base_combos <- pea_oat_combos[(n_unique + 1):(n_unique + n_within_loc / 2), ] %>% #n_unique + 1 defines the starting index for the subset # n_unique + n_within_loc / 2: defines the ending index for the subset
  mutate(combo = paste(Pea, Oat, sep = "+"))

# Ensure 30 unique combos per location, each with Rep 1 and Rep 2
within_loc_combos_rep1 <- base_combos %>%
  mutate(Location = rep(1:5, each = 20),  # Repeat location numbers so each location has 30 unique combos
         Replication = "Rep 1"
  )

within_loc_combos_rep2 <- base_combos %>%
  mutate(
    Location = rep(1:5, each = 20),  # Same locations as Rep 1
    Replication = "Rep 2"
  )

within_loc_combos <- bind_rows(within_loc_combos_rep1, within_loc_combos_rep2) %>%   # Combine Rep 1 and Rep 2 into one dataset
  mutate(category = "within_loc")

print(within_loc_combos) # View the combined dataset
table(within_loc_combos$Location) # Check the count of plots per location

within_loc_pea_count <- within_loc_combos %>% distinct(Pea) %>% nrow()
within_loc_oat_count <- within_loc_combos %>% distinct(Oat) %>% nrow()
within_loc_combo_count <- within_loc_combos %>% distinct(combo) %>% nrow() # output should be 100


# Count occurrences of each unique Oat, pea and combo to ensure combos are present in rep 1 and 2
pea_counts_within <- within_loc_combos  %>%
  group_by(Pea) %>%
  summarize(Count = dplyr::n())

oat_counts_within <- within_loc_combos %>%
  group_by(Oat) %>%
  summarize(Count = n())

combo_counts_within <- within_loc_combos %>%
  group_by(combo) %>%
  summarize(Count = n())

print(oat_counts_within)
print(pea_counts_within)
print(combo_counts_within)
```
The use of n_unique + n_within_loc / 2 + 1 as the starting index ensures that the combinations selected for "across-location" are not overlapping with the ones selected for "unique combinations" (n_unique) or the "within-location replicated combinations" (n_within_loc).

By incrementing the index (through the calculation of the row range), each subset for each location starts after the previously selected combos, making sure that the across-location combinations are unique compared to the earlier ones.

```{r}
# Assign 10% replicated across locations

# Create a single data frame for across-location combos
across_loc_combos <- bind_rows(
  lapply(1:5, function(loc) {
    pea_oat_combos[(n_unique + n_within_loc / 2 + 1):(n_unique + n_within_loc / 2 + n_across_loc / 5), ] %>%
      mutate(
        Location = as.character(loc),
        combo = paste(Pea, Oat, sep = "+"),
        category = "across_loc",  Replication = "Rep 0"  # Assigning a fixed category value
      )
  })
)

# View the across_loc_combos
print(across_loc_combos)
table(across_loc_combos$Location) # check if all location got 40 accession combo

# Ensure all Location columns are of the same type (character): location for easy merging
unique_combos <- unique_combos %>% mutate(Location = as.character(Location))
within_loc_combos <- within_loc_combos %>% mutate(Location = as.character(Location))
across_loc_combos <- across_loc_combos %>% mutate(Location = as.character(Location))

# Now combine all data into final_design
final_design <- bind_rows(
  unique_combos,
  within_loc_combos,
  across_loc_combos
)

table(final_design$Location) # expected 400 each
head(final_design)
print(final_design)
```
```{r}
# Assign plot numbers from 1 to 400 for each location at random using the function sample from base R
set.seed(1663)  # for reproducibility
final_design <- final_design %>%
  group_by(Location) %>%
  mutate(PlotNumber = sample(1:n(), size = n(), replace = FALSE)) %>%
  ungroup()

# Assign row and column numbers in a serpentine manner for 20 columns for visualization purposes
final_design <- final_design %>%
  arrange(Location, PlotNumber) %>%
  group_by(Location) %>%
  mutate(
    Row = ceiling(PlotNumber / 20),  # 20 columns per row
    Column = ifelse(Row %% 2 == 1, (PlotNumber - 1) %% 20 + 1, 20 - (PlotNumber - 1) %% 20),  # Serpentine (zigzag) column numbering
    Row = ifelse(Column == 1, Row - 0, Row)  # Adjust row for the first column
  ) %>%
  ungroup()

# View the updated dataset with row and column numbers
print(final_design)

# Save the design to a CSV file
write.csv(final_design, "pea_oat_experiment_design.csv", row.names = FALSE) 
```

# geom_tile plot with 'Location' as facets, coloring by 'category', and including 'combo' as text


```{r}
experimental_design_plot <- ggplot(final_design, aes(x = Column, y = Row, fill = category)) +
  geom_tile(color = "black", width = 1, height = 1) +  # Create rectangular tiles
  geom_text(aes(label = PlotNumber), size = 1, vjust = 0.5, hjust = 0.5, color = "black") +  # Add text inside tiles
  facet_wrap(~ Location, ncol = 5) +   # Facet by 'Location', with 5 columns of facets
  theme_minimal() +                    # Apply minimal theme
  labs(
    title = "Experimental Design",
    x = "Column",
    y = "Row",
    fill = "Category"
  ) +
  scale_fill_manual(values = c("within_loc" = "blue", "across_loc" = "red", "unique_combos" = "green")) +  # Custom colors for category
  theme(
    strip.text = element_text(size = 10),  # Control facet label size
    panel.spacing = unit(1, "lines"),      # Space between facets
    legend.position = "bottom",             # Position the legend at the bottom
    axis.text = element_text(size = 8)      # Control axis text size
  )
ggsave("experimental_design_plot.pdf", plot = experimental_design_plot, width = 10, height = 8, dpi = 300)
```

#estimate the number of seeds needed for the experiment
```{r}
final_design
pea_count <- final_design %>% distinct(Pea) %>% nrow()
oat_count <- final_design %>% distinct(Oat) %>% nrow()

pea_counts <- final_design %>%
  group_by(Pea) %>%
  summarize(Count = dplyr::n())%>%
  mutate(crop = "Pea") %>%
  rename(accessions = Pea)


oat_counts <- final_design %>%
  group_by(Oat) %>%
  summarize(Count = dplyr::n()) %>%
  mutate(crop = "Oat") %>%
  rename(accessions = Oat)

oat_counts <- as.data.frame(oat_counts)
pea_counts <- as.data.frame(pea_counts)

combined_counts <- bind_rows(pea_counts, oat_counts) 

write.csv(combined_counts, file="combined_counts.csv") 
#combined_counts <- read.csv("combined_counts.csv")

gSeedPer6Plot_pea <- 59
gSeedPer6Plot_oat <- 24

combined_counts  %<>% 
  mutate(
    N_6ftseedsneededPlots= case_when(
      crop == "Oat" ~ Count*gSeedPer6Plot_oat,  # Calculate for oats
      crop == "Pea" ~  Count*gSeedPer6Plot_pea  # Calculate for peas
    ))

inventory <- inventory [c("Name","weight")]  
final_data_count <- left_join(combined_counts, inventory, by = c("accession" = "Name"))
final_data_count1 <- final_data_count  %<>% 
  mutate(Check = ifelse(N_6ftseedsneededPlots > weight, FALSE, TRUE))
write.csv(final_data_count1, file ="final_data_count1.csv") 

```


