# Load required library
library(dplyr)

# Read the CSV file
df <- read.csv("./data/Q14-19-20-21_Alice.csv", stringsAsFactors = FALSE)

# Function to combine non-empty values
combine_values <- function(x) {
  # Remove NA and empty strings
  x <- x[!is.na(x) & x != ""]
  
  # If all values are empty/NA, return NA
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  # Get unique non-empty values
  unique_vals <- unique(x)
  
  # If only one unique value, return it
  if (length(unique_vals) == 1) {
    return(unique_vals[1])
  } else {
    # Concatenate multiple different values with separator
    return(paste(unique_vals, collapse = " | "))
  }
}

# Group by Folio and combine entries
df_combined <- df %>%
  group_by(Folio) %>%
  summarise(across(everything(), combine_values)) %>%
  ungroup()

# Display summary
cat("Original number of rows:", nrow(df), "\n")
cat("Combined number of rows:", nrow(df_combined), "\n")
cat("Number of unique folios:", n_distinct(df$Folio, na.rm = TRUE), "\n")

# Save the combined data
write.csv(df_combined, "./data/Q14-19-20-21_Alice.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nCombined data saved to: Q14-19-20-21_Alice.csv\n")

# Show a preview of the combined data
cat("\nPreview of combined data:\n")
print(head(df_combined, 5))

data_alice <- df_combined %>%
  select(Folio, X19.CAT) 


#########################################
#########################################
### Data Jocelyne


# Read the CSV file
df <- read.csv("./data/Q14-19-20-21_Jocelyne.csv", stringsAsFactors = FALSE)

# Function to combine non-empty values
combine_values <- function(x) {
  # Remove NA and empty strings
  x <- x[!is.na(x) & x != ""]
  
  # If all values are empty/NA, return NA
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  # Get unique non-empty values
  unique_vals <- unique(x)
  
  # If only one unique value, return it
  if (length(unique_vals) == 1) {
    return(unique_vals[1])
  } else {
    # Concatenate multiple different values with separator
    return(paste(unique_vals, collapse = " | "))
  }
}

# Group by Folio and combine entries
df_combined <- df %>%
  group_by(Folio) %>%
  summarise(across(everything(), combine_values)) %>%
  ungroup()

# Display summary
cat("Original number of rows:", nrow(df), "\n")
cat("Combined number of rows:", nrow(df_combined), "\n")
cat("Number of unique folios:", n_distinct(df$Folio, na.rm = TRUE), "\n")

# Save the combined data
write.csv(df_combined, "./data/Q14-19-20-21_Jocelyne.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nCombined data saved to: Q14-19-20-21_Jocelyne.csv\n")

# Show a preview of the combined data
cat("\nPreview of combined data:\n")
print(head(df_combined, 5))

data_jocelyne <- df_combined %>%
  select(Folio, X19.CAT) 



###### LAUREN
###### 
df <- read.csv("./data/Q19.csv", stringsAsFactors = FALSE)

data_lauren <- df %>%
  select(ID, Q19_CAT) 
colnames(data_lauren) <- c("Folio", "CatL")


data <- merge(data_alice, data_jocelyne, by = "Folio")
data <- merge(data, data_lauren, by = "Folio")
data <- data %>% mutate(across(where(is.character), tolower)) %>%
      mutate(X19.CAT.x = as.character(X19.CAT.x),
             X19.CAT.y = as.character(X19.CAT.y)) %>%
      mutate(across(where(is.character), ~gsub("si", "yes", .))) %>%
      mutate(across(where(is.character), ~gsub("a bit", "somewhat", .))) %>% 
      mutate(category_match = ifelse(X19.CAT.x == X19.CAT.y & X19.CAT.y == CatL, "match", "different"))

colnames(data) <- c("SampleID", "CatA", "CatJ", "CatL", "Match")

data <- data
  

data <- data %>% 
  mutate(CatJ = as.factor(CatJ),
         CatA = as.factor(CatA),
         CatL = as.factor(CatL),
         Match = as.factor(Match)) 




summary(data)


write.csv(data, "./data/Q19_combined.csv")

# modified a few manually - created a consensus column.
# Keep only rows with consensus info

df <- read.csv("./data/Q19_combined.csv", stringsAsFactors = TRUE, na.strings = c("", " "))

summary(df)
q19 <- df %>% 
  filter(Consensus != is.na(Consensus))



data <- q19
# Step 1: Create a frequency table by SES and chronic condition
plot_data <- data %>%
  group_by(Consensus) %>%
  summarize(n=n(),
    percent = 100 * n / sum(n))

# Step 2: Plot as grouped bar chart (percentage)
ggplot(plot_data, aes(x = Consensus, y = n)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black") +
  # scale_fill_manual(values = c("No" = "#fbaca7", "Yes" = "#52d8da")) +
  geom_text(aes(label = paste(round(n, 1), "", sep="")), 
            vjust = 2, 
            colour = "gray20", 
            size = 5) +
  labs(
    title = "Q19. Do you think you spend too long managing water?",
    x = "Answer",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)




write.csv(q19, "./data/Q19_consensus.csv")

