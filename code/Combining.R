library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(GGally)

df1<-read.csv("../Screening_V2.csv", na.strings = c("", "N/A", "; NA,", "NA"))
dim(df1)
head(df1)

df2<-read.csv("../Q2-3-4-5-6 (Hrs_water_supply)_.csv", na.strings = c("", "N/A", "; NA,", "NA"))
dim(df2)
head(df2)

df3 <-read.csv("../Q8-26-28_240417_all.xlsx - Form Responses 1.csv", na.strings = c("", "N/A", "; NA,", "NA"))
dim(df3)
head(df3)

df4 <-read.csv("../Q9_10_11_29_31_all.xlsx - Form Responses 1.csv", na.strings = c("", "N/A", "; NA,", "NA"))
dim(df4)
head(df4)


head(df2)
names(df2)[1] <- c("SampleID")

m1<-merge(df1, df2, by = "SampleID", all = TRUE)
m2<-merge(df3, df4, by = "SampleID", all = TRUE)

d <-merge(m1,m2, by="SampleID", all = TRUE)

# Explore dataset

colSums(is.na(d))
dim(d)

597 75

# remove duplicate rows
d1 <- d %>%
  distinct()

dim(d1)

d2 <- d1 %>%
  mutate(across(everything(), as.character))
dim(d2)

d2$SampleID<-as.numeric(d2$SampleID)

d3 <- d2 %>%
  group_by(SampleID) %>%
  summarise(across(everything(), ~ {
    unique_vals <- unique(.)
    if (length(unique_vals) == 1) {
      return(unique_vals)
    } else {
      return(paste(unique_vals, collapse = "; "))
    }
  })) 

d3$AwarenessCutoffs <- as.character(d3$AwarenessCutoffs)
colSums(is.na(d3))
dim(d3)

# remove comments
d3 <- d3 %>%
  select(-starts_with("Comm"), -Emotions_ENGLISH)

# clean data
d3[d3 == "diario"] <- "Diario"
d3[d3 == "intermitente"] <- "Intermittent"
d3[d3 == "continua"] <- "Continuous"
d3[d3 == "Intermitente"] <- "Intermittent"
d3[d3 == "Continua"] <- "Continuous"

# explore missing data
d3[d3 == "NA"] <- NA
d3[d3 == "n/a"] <- NA
d3[d3 == ""] <- NA
d3[d3 == "N/A "] <- NA
d3[d3 == "#NAME?"] <- NA
d3[d3 == "n/a "] <- NA


d3[d3 == "No; NA"] <- "No"
d3[d3 == "NA; No"] <- "No"
d3[d3 == "no "] <- "No"
d3[d3 == "no; NA"] <- "No"
d3[d3 == "NA; no"] <- "No"
d3[d3 == "no"] <- "No"
d3[d3 == "Confian en que llega el agua pero no confian en su calidad"] <- "No"
d3[d3 == "para tomar no para lo demas si"] <- "No"

d3[d3 == "NA; Si"] <- "Yes"
d3[d3 == "NA; si"] <- "Yes"
d3[d3 == "si; NA"] <- "Yes"
d3[d3 == "Si; NA"] <- "Yes"
d3[d3 == "si"] <- "Yes"
d3[d3 == "Si"] <- "Yes"
d3[d3 == "mas o menos"] <- "Unsure"
d3[d3 == "mas o menos; NA"] <- "Unsure"
d3[d3 == "mas o menos"] <- "Unsure"


d3[d3 == "NA; Same"] <- "Same"
d3[d3 == "NA; Worse"] <- "Worse"
d3[d3 == "NA; Better"] <- "Better"
d3[d3 == "Same; NA"] <- "Same"
d3[d3 == "Better; NA"] <- "Better"
d3[d3 == "Worse; NA"] <- "Worse"

d3[d3 == "Negative; Positive"] <- "Positive & Negative"
d3[d3 == "Positive; Negative"] <- "Positive & Negative"
d3[d3 == "NA; Yes se dan cuenta"] <- "Yes"
d3[d3 == "Yes se dan cuenta"] <- "Yes"
d3[d3 == "se da cuenta"] <- "Yes"
d3[d3 == "no se da cuenta"] <- "No"
d3[d3 == "no se dan cuenta"] <- "No"
d3[d3 == "podria ser"] <- "Maybe"
d3[d3 == "podria ser (q no se dan cuenta)"] <- "Maybe"
d3[d3 == "a veces"] <- "Sometimes"
d3[d3 == "siempre hay agua/no deja de caer"] <- "no response"



table(d3$AwarenessCutoffs)

d3$HrsWaterWeek <- as.numeric(d3$HrsWaterWeek)
d3$ContinIntermit <- as.factor(d3$ContinIntermit)
d3$AwarenessCutoffs <- as.factor(d3$AwarenessCutoffs)
d3$ContinIntermit <- as.factor(d3$ContinIntermit)
d3$FreqSupply4weeks <- as.factor(d3$FreqSupply4weeks)
d3$Emotion_type <- as.factor(d3$Emotion_type)


missing_counts <- d3 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count")

# View the missing counts
print(missing_counts)

ggplot(missing_counts, aes(x = reorder(Variable, -Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Missing Values Count by Column",
       x = "Variables",
       y = "Number of Missing Values") +
  theme_minimal()



# explore relevant columns
relevant <- d3 %>% select(SampleID, FreqSupply4weeks, HrsWaterWeek, ContinIntermit, Q29_ImprovementArea_categorical, ContinIntermit, X8_trust_tap_water, Q11_PaymentWaived, AwarenessCutoffs, Emotion_type, Quality_compared)

missing_counts <- relevant %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count")

# View the missing counts
print(missing_counts)

ggplot(missing_counts, aes(x = reorder(Variable, -Missing_Count), y = Missing_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Missing Values Count by Column",
       x = "Variables",
       y = "Number of Missing Values") +
  theme_minimal()
dim(relevant)




# Plots
ggplot(relevant, aes(x = HrsWaterWeek)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Hours of Water per Week",
       x = "Hours of Water per Week",
       y = "Count")





ggplot(relevant, aes(x = FreqSupply4weeks)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Frequency of Water Supply in Last 4 Weeks",
       x = "Frequency Supply (4 weeks)",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(relevant, aes(x = ContinIntermit, y = as.numeric(HrsWaterWeek))) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Comparison of Water Supply by Continuous/Intermittent Supply",
       x = "Continuous or Intermittent",
       y = "Hours of Water per Week")


ggplot(relevant, aes(x = Q29_ImprovementArea_categorical)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Improvement Areas for Water Supply",
       x = "Improvement Area",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(relevant, aes(x = Emotion_type, fill = Quality_compared)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Emotional Response by Perceived Water Quality",
       x = "Emotion Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(relevant, aes(x = FreqSupply4weeks)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Frequency of Water Supply by Awareness of Cutoffs",
       x = "Frequency of Supply (4 weeks)",
       y = "Count") +
  facet_wrap(~ AwarenessCutoffs) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Ensure that HrsWaterWeek is numeric and X8_trust_tap_water is a factor
relevant <- relevant %>%
  mutate(
    HrsWaterWeek = as.numeric(gsub("[^0-9.]", "", HrsWaterWeek)),  # Ensure HrsWaterWeek is numeric
    X8_trust_tap_water = factor(X8_trust_tap_water)  # Treat X8_trust_tap_water as a factor (categorical)
  )


# Boxplot of HrsWaterWeek by X8_trust_tap_water
ggplot(relevant, aes(x = X8_trust_tap_water, y = HrsWaterWeek)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Relationship between Trust in Tap Water and Hours of Water per Week",
       x = "Trust in Tap Water (Categorical)",
       y = "Hours of Water per Week") +
  theme_minimal()


# Jitter plot of HrsWaterWeek by X8_trust_tap_water
ggplot(relevant, aes(x = X8_trust_tap_water, y = HrsWaterWeek)) +
  geom_jitter(width = 0.2, color = "purple") +
  labs(title = "Relationship between Trust in Tap Water and Hours of Water per Week",
       x = "Trust in Tap Water (Categorical)",
       y = "Hours of Water per Week") +
  theme_minimal()


# Combined boxplot and jitter plot
ggplot(relevant, aes(x = X8_trust_tap_water, y = HrsWaterWeek)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +  # Boxplot with transparency
  geom_jitter(width = 0.2, color = "purple", alpha = 0.5) +  # Add jittered points
  labs(title = "Trust in Tap Water and Hours of Water per Week",
       x = "Trust in Tap Water (Categorical)",
       y = "Hours of Water per Week") +
  theme_minimal()



# Create a contingency table between two factors
factor_table <- table(relevant$Emotion_type, relevant$X8_trust_tap_water)

# Plot grouped bar plot
barplot(factor_table, beside = TRUE, col = c("lightblue", "pink", "lightgreen"),
        legend = rownames(factor_table),
        main = "Frequency Supply by Trust in Tap Water",
        xlab = "Trust in Tap Water",
        ylab = "# Participants")



# Create a mosaic plot to visualize two categorical variables
mosaicplot(~ FreqSupply4weeks + X8_trust_tap_water, data = relevant,
           color = TRUE,
           main = "Mosaic Plot of Water Supply Frequency and Trust in Tap Water",
           xlab = "Frequency Supply (4 weeks)",
           ylab = "Trust in Tap Water")






# Stacked bar plot to compare two categorical variables
ggplot(relevant, aes(x = FreqSupply4weeks, fill = X8_trust_tap_water)) +
  geom_bar(position = "stack") +
  labs(title = "Frequency of Water Supply by Trust in Tap Water",
       x = "Frequency Supply (4 weeks)",
       y = "Count",
       fill = "Trust in Tap Water") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(relevant, aes(x = X8_trust_tap_water, fill = FreqSupply4weeks)) +
  geom_bar(position = "stack") +
  labs(title = "Frequency of Water Supply by Trust in Tap Water",
       x = "Trust in tap water",
       y = "Count", 
       fill = "Frequency of Water supply") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate percentages for each combination of FreqSupply4weeks and X8_trust_tap_water
relevant_percent <- relevant %>%
  group_by(FreqSupply4weeks, X8_trust_tap_water) %>%
  summarise(count = n()) %>%  # Count occurrences
  mutate(percentage = count / sum(count) * 100)  # Calculate percentage for each group

# Create the bar plot with percentages
ggplot(relevant_percent, aes(x = FreqSupply4weeks, y = percentage, fill = X8_trust_tap_water)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of Water Supply by Trust in Tap Water (Percentages)",
       x = "Frequency Supply (4 weeks)",
       y = "Percentage",
       fill = "Trust in Tap Water") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Grouped bar plot
ggplot(relevant, aes(x = FreqSupply4weeks, fill = X8_trust_tap_water)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequency of Water Supply by Trust in Tap Water",
       x = "Frequency Supply (4 weeks)",
       y = "Count",
       fill = "Trust in Tap Water") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate percentages for each combination of FreqSupply4weeks and X8_trust_tap_water
relevant_percent <- relevant %>%
  group_by(FreqSupply4weeks, X8_trust_tap_water) %>%
  summarise(count = n()) %>%  # Count occurrences
  mutate(percentage = count / sum(count) * 100)  # Calculate percentage for each group

# Create the bar plot with percentages, faceted by trust in tap water
ggplot(relevant_percent, aes(x = FreqSupply4weeks, y = percentage, fill = FreqSupply4weeks)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ X8_trust_tap_water) +  # Facet by trust in tap water
  labs(title = "Frequency of Water Supply by Trust in Tap Water (Percentages)",
       x = "Frequency Supply (4 weeks)",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")  # Hide legend to avoid redundancy




# Facet plot by AwarenessCutoffs with bar plots of FreqSupply4weeks
ggplot(relevant, aes(x = FreqSupply4weeks)) +
  geom_bar(fill = "lightblue", color = "black") +
  facet_wrap(~ AwarenessCutoffs) +
  labs(title = "Frequency of Water Supply Faceted by Awareness of Cutoffs",
       x = "Frequency Supply (4 weeks)",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(relevant %>% filter(!is.na(AwarenessCutoffs)),
       aes(x = AwarenessCutoffs)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Awareness of Cutoffs",
       x = "Awareness of Cutoffs",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

