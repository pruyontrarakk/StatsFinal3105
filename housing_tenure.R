

library(tidyverse)
library(ggplot2)
library(dplyr)
library(GGally)
library(caret)
library(randomForest)
library(tidyr)
library(car)
library(maps)
library(randomForest)
library(pdp)


\newpage

# MERGE ALL THE REQUIRED DATASETS TOGETHER FOR 2010

https://data.census.gov/table/ACSST5Y2010.S2503?q=S2503%20FINANCIAL%20CHARACTERISTICS&g=010XX00US$0600000_040XX00US06$0600000

```{r}

county_data_2010 <- read.csv(
  "/Users/pruyontrarak/Downloads/ACSST5Y2010.S2503_2024-12-08T035314/ACSST5Y2010.S2503-Data.csv", 
  skip = 1, 
  header = TRUE
)


# Filter for California data
california_data_2010 <- county_data_2010 %>% filter(grepl(", California", Geographic.Area.Name))
columns_to_remove <- grep("Margin", colnames(california_data_2010), value = TRUE)
california_data_2010 <- california_data_2010[, !colnames(california_data_2010) %in% columns_to_remove]



california_58_counties_2010 <- california_data_2010 %>%
  mutate(
    County = str_extract(Geographic.Area.Name, ",\\s*[^,]+,") %>% # Extract the county name
      str_replace_all(",\\s*", "") %>%
      str_trim()
  ) %>%
  select(-Geographic.Area.Name)%>%
  distinct(County, .keep_all = TRUE)



# Check and use the correct column names
new_california_58_counties_2010 <- california_58_counties_2010 %>%
  mutate(
    PercentRenterOccupied = 100 * as.numeric(Renter.occupied.housing.units..Estimate..Occupied.housing.units) /
      as.numeric(Occupied.housing.units..Estimate..Occupied.housing.units),
    PercentOwnerOccupied = 100 * as.numeric(Owner.occupied.housing.units..Estimate..Occupied.housing.units) /
      as.numeric(Occupied.housing.units..Estimate..Occupied.housing.units)
  )



new_california_58_counties_2010 <- new_california_58_counties_2010 %>%
  rename(
    Renter_Income_Less_5k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS...Less.than..5.000",
    Renter_Income_5k_10k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....5.000.to..9.999",
    Renter_Income_10k_15k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....10.000.to..14.999",
    Renter_Income_15k_20k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....15.000.to..19.999",
    Renter_Income_20k_25k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....20.000.to..24.999",
    Renter_Income_25k_35k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....25.000.to..34.999",
    Renter_Income_35k_50k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....35.000.to..49.999",
    Renter_Income_50k_75k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....50.000.to..74.999",
    Renter_Income_75k_100k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....75.000.to..99.999",
    Renter_Income_100k_150k = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....100.000.to..149.999",
    Renter_Income_150k_or_more = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....150.000.or.more",
    Owner_Income_Less_5k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS...Less.than..5.000",
    Owner_Income_5k_10k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....5.000.to..9.999",
    Owner_Income_10k_15k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....10.000.to..14.999",
    Owner_Income_15k_20k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....15.000.to..19.999",
    Owner_Income_20k_25k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....20.000.to..24.999",
    Owner_Income_25k_35k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....25.000.to..34.999",
    Owner_Income_35k_50k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....35.000.to..49.999",
    Owner_Income_50k_75k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....50.000.to..74.999",
    Owner_Income_75k_100k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....75.000.to..99.999",
    Owner_Income_100k_150k = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....100.000.to..149.999",
    Owner_Income_150k_or_more = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS....150.000.or.more",
    Renter_MedianIncome = "Renter.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars.",
    Owner_MedianIncome = "Owner.occupied.housing.units..Estimate..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2010.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars."
  ) 



new_california_58_counties_2010 <- new_california_58_counties_2010 %>%
  select(
    County,
    PercentRenterOccupied, PercentOwnerOccupied,
    Renter_Income_Less_5k, Renter_Income_5k_10k, Renter_Income_10k_15k,
    Renter_Income_15k_20k, Renter_Income_20k_25k, Renter_Income_25k_35k,
    Renter_Income_35k_50k, Renter_Income_50k_75k, Renter_Income_75k_100k,
    Renter_Income_100k_150k, Renter_Income_150k_or_more,
    Owner_Income_Less_5k, Owner_Income_5k_10k, Owner_Income_10k_15k,
    Owner_Income_15k_20k, Owner_Income_20k_25k, Owner_Income_25k_35k,
    Owner_Income_35k_50k, Owner_Income_50k_75k, Owner_Income_75k_100k,
    Owner_Income_100k_150k, Owner_Income_150k_or_more,
    Renter_MedianIncome, Owner_MedianIncome
  )


california_58_counties_2010 <- new_california_58_counties_2010 %>%
  mutate(across(
    -County, # Exclude the 'County' column
    ~ as.integer(as.numeric(.)), # Convert to numeric first, then to integer
    .names = "{.col}"
  ))




https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html?utm_source=chatgpt.com




demographics_data_2010 <- read.csv("/Users/pruyontrarak/Downloads/CC-EST2020-ALLDATA-06.csv", header = TRUE)


# Summarize data to get one row per county
demographics_data_2010 <- demographics_data_2010 %>%
  filter(YEAR == 1) %>%  # Filter for the year 2010
  group_by(CTYNAME) %>%  # Group by county name
  summarise(
    Total_Population = sum(TOT_POP, na.rm = TRUE),
    Total_Male = sum(TOT_MALE, na.rm = TRUE),
    Total_Female = sum(TOT_FEMALE, na.rm = TRUE),
    White_Male = sum(WA_MALE, na.rm = TRUE),
    White_Female = sum(WA_FEMALE, na.rm = TRUE),
    Black_Male = sum(BA_MALE, na.rm = TRUE),
    Black_Female = sum(BA_FEMALE, na.rm = TRUE),
    Asian_Male = sum(AA_MALE, na.rm = TRUE),
    Asian_Female = sum(AA_FEMALE, na.rm = TRUE),
    Native_American_Male = sum(IA_MALE, na.rm = TRUE),
    Native_American_Female = sum(IA_FEMALE, na.rm = TRUE),
    Hispanic_Male = sum(H_MALE, na.rm = TRUE),
    Hispanic_Female = sum(H_FEMALE, na.rm = TRUE),
    Age_Under5 = sum(ifelse(AGEGRP == 1, TOT_POP, 0), na.rm = TRUE),
    Age_5_to_17 = sum(ifelse(AGEGRP %in% 2:4, TOT_POP, 0), na.rm = TRUE),
    Age_18_to_64 = sum(ifelse(AGEGRP %in% 5:14, TOT_POP, 0), na.rm = TRUE),
    Age_65_and_Over = sum(ifelse(AGEGRP %in% 15:18, TOT_POP, 0), na.rm = TRUE)
  )




# Ensure column names are consistent for merging
california_58_counties_2010 <- california_58_counties_2010 %>%
  rename(CTYNAME = County)

california_58_counties_2010 <- california_58_counties_2010 %>%
  mutate(CTYNAME = gsub(", California", "", CTYNAME))
merged_2010_data <- merge(demographics_data_2010, california_58_counties_2010, by = "CTYNAME")

colnames(merged_2010_data) <- paste0(colnames(merged_2010_data), "_2010")







\newpage

# MERGE ALL THE REQUIRED DATASETS TOGETHER FOR 2020

\newpage

# Californa Data Set Up for 2020

https://data.census.gov/table/ACSST5Y2020.S2503?q=tenure%20California%20counties%202020&g=010XX00US$0600000_040XX00US06$0600000

```{r}

county_data_2020 <- read.csv(
  "/Users/pruyontrarak/Downloads/ACSST5Y2020.S2503_2024-11-25T162155/ACSST5Y2020.S2503-Data.csv", 
  skip = 1, 
  header = TRUE
)




# Filter for California data
california_data_2020 <- county_data_2020 %>% filter(grepl(", California", Geographic.Area.Name))
columns_to_remove <- grep("Margin", colnames(california_data_2020), value = TRUE)
california_data_2020 <- california_data_2020[, !colnames(california_data_2020) %in% columns_to_remove]
california_58_counties_2020 <- california_data_2020[1:58, ]

# Convert columns to numeric
convert_to_numeric <- function(column) {
  as.numeric(gsub("[^0-9.-]", "", column)) # Remove non-numeric characters and convert to numeric
}
california_58_counties_2020[, -c(1, 2)] <- lapply(california_58_counties_2020[, -c(1, 2)], convert_to_numeric)




# Check and use the correct column names
new_california_58_counties_2020 <- california_58_counties_2020 %>%
  mutate(
    PercentRenterOccupied = 100 * as.numeric(Estimate..Renter.occupied.housing.units..Occupied.housing.units) /
      as.numeric(Estimate..Occupied.housing.units..Occupied.housing.units),
    PercentOwnerOccupied = 100 * as.numeric(Estimate..Owner.occupied.housing.units..Occupied.housing.units) /
      as.numeric(Estimate..Occupied.housing.units..Occupied.housing.units)
  )




# Rename long column names to shorter ones
california_58_counties_2020 <- new_california_58_counties_2020 %>%
  rename(
    Total_renter = "Estimate..Renter.occupied.housing.units..Occupied.housing.units",
    Total_owner = "Estimate..Owner.occupied.housing.units..Occupied.housing.units",
    Renter_Income_Less_5k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS...Less.than..5.000",
    Renter_Income_5k_10k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....5.000.to..9.999",
    Renter_Income_10k_15k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....10.000.to..14.999",
    Renter_Income_15k_20k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....15.000.to..19.999",
    Renter_Income_20k_25k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....20.000.to..24.999",
    Renter_Income_25k_35k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....25.000.to..34.999",
    Renter_Income_35k_50k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....35.000.to..49.999",
    Renter_Income_50k_75k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....50.000.to..74.999",
    Renter_Income_75k_100k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....75.000.to..99.999",
    Renter_Income_100k_150k = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....100.000.to..149.999",
    Renter_Income_150k_or_more = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....150.000.or.more",
    Owner_Income_Less_5k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS...Less.than..5.000",
    Owner_Income_5k_10k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....5.000.to..9.999",
    Owner_Income_10k_15k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....10.000.to..14.999",
    Owner_Income_15k_20k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....15.000.to..19.999",
    Owner_Income_20k_25k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....20.000.to..24.999",
    Owner_Income_25k_35k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....25.000.to..34.999",
    Owner_Income_35k_50k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....35.000.to..49.999",
    Owner_Income_50k_75k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....50.000.to..74.999",
    Owner_Income_75k_100k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....75.000.to..99.999",
    Owner_Income_100k_150k = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....100.000.to..149.999",
    Owner_Income_150k_or_more = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS....150.000.or.more",
    Renter_MedianIncome = "Estimate..Percent.renter.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars.",
    
    Owner_MedianIncome = "Estimate..Percent.owner.occupied.housing.units..Occupied.housing.units..HOUSEHOLD.INCOME.IN.THE.PAST.12.MONTHS..IN.2020.INFLATION.ADJUSTED.DOLLARS...Median.household.income..dollars.",
    
  ) %>%
  select(
    Geographic.Area.Name,
    PercentRenterOccupied, PercentOwnerOccupied, 
    Renter_Income_Less_5k, Renter_Income_5k_10k, Renter_Income_10k_15k,
    Renter_Income_15k_20k, Renter_Income_20k_25k, Renter_Income_25k_35k,
    Renter_Income_35k_50k, Renter_Income_50k_75k, Renter_Income_75k_100k,
    Renter_Income_100k_150k, Renter_Income_150k_or_more,
    Owner_Income_Less_5k, Owner_Income_5k_10k, Owner_Income_10k_15k,
    Owner_Income_15k_20k, Owner_Income_20k_25k, Owner_Income_25k_35k,
    Owner_Income_35k_50k, Owner_Income_50k_75k, Owner_Income_75k_100k,
    Owner_Income_100k_150k, Owner_Income_150k_or_more,
    Renter_MedianIncome, Owner_MedianIncome
  )



# Looking at second dataset to see how race and age and gender group play into this

https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-county-detail.html?utm_source=chatgpt.com



demographics_data_2020 <- read.csv("/Users/pruyontrarak/Downloads/CC-EST2020-ALLDATA-06.csv", header = TRUE)


# Summarize data to get one row per county
demographics_data_2020 <- demographics_data_2020 %>%
  filter(YEAR == 10) %>%  # Filter for the year 2020
  group_by(CTYNAME) %>%  # Group by county name
  summarise(
    Total_Population = sum(TOT_POP, na.rm = TRUE),
    Total_Male = sum(TOT_MALE, na.rm = TRUE),
    Total_Female = sum(TOT_FEMALE, na.rm = TRUE),
    White_Male = sum(WA_MALE, na.rm = TRUE),
    White_Female = sum(WA_FEMALE, na.rm = TRUE),
    Black_Male = sum(BA_MALE, na.rm = TRUE),
    Black_Female = sum(BA_FEMALE, na.rm = TRUE),
    Asian_Male = sum(AA_MALE, na.rm = TRUE),
    Asian_Female = sum(AA_FEMALE, na.rm = TRUE),
    Native_American_Male = sum(IA_MALE, na.rm = TRUE),
    Native_American_Female = sum(IA_FEMALE, na.rm = TRUE),
    Hispanic_Male = sum(H_MALE, na.rm = TRUE),
    Hispanic_Female = sum(H_FEMALE, na.rm = TRUE),
    Age_Under5 = sum(ifelse(AGEGRP == 1, TOT_POP, 0), na.rm = TRUE),
    Age_5_to_17 = sum(ifelse(AGEGRP %in% 2:4, TOT_POP, 0), na.rm = TRUE),
    Age_18_to_64 = sum(ifelse(AGEGRP %in% 5:14, TOT_POP, 0), na.rm = TRUE),
    Age_65_and_Over = sum(ifelse(AGEGRP %in% 15:18, TOT_POP, 0), na.rm = TRUE)
  )





# Ensure column names are consistent for merging
california_58_counties_2020 <- california_58_counties_2020 %>%
  rename(CTYNAME = Geographic.Area.Name)


california_58_counties_2020 <- california_58_counties_2020 %>%
  mutate(CTYNAME = gsub(", California", "", CTYNAME))

merged_2020_data <- merge(demographics_data_2020, california_58_counties_2020, by = "CTYNAME")



colnames(merged_2020_data) <- paste0(colnames(merged_2020_data), "_2020")



merged_2010_data <- merged_2010_data %>%
  mutate(across(
    -CTYNAME_2010, # Exclude the 'County' column
    ~ as.integer(as.numeric(.)), # Convert to numeric first, then to integer
    .names = "{.col}"
  ))

merged_2020_data <- merged_2020_data %>%
  mutate(across(
    -CTYNAME_2020, # Exclude the 'County' column
    ~ as.integer(as.numeric(.)), # Convert to numeric first, then to integer
    .names = "{.col}"
  ))



# Add Total_{race} variables to merged_2020_data
merged_2020_data <- merged_2020_data %>%
  mutate(
    Total_White_2020 = White_Male_2020 + White_Female_2020,
    Total_Black_2020 = Black_Male_2020 + Black_Female_2020,
    Total_Asian_2020 = Asian_Male_2020 + Asian_Female_2020,
    Total_Native_American_2020 = Native_American_Male_2020 + Native_American_Female_2020,
    Total_Hispanic_2020 = Hispanic_Male_2020 + Hispanic_Female_2020
  )

# Add Total_{race} variables to merged_2010_data
merged_2010_data <- merged_2010_data %>%
  mutate(
    Total_White_2010 = White_Male_2010 + White_Female_2010,
    Total_Black_2010 = Black_Male_2010 + Black_Female_2010,
    Total_Asian_2010 = Asian_Male_2010 + Asian_Female_2010,
    Total_Native_American_2010 = Native_American_Male_2010 + Native_American_Female_2010,
    Total_Hispanic_2010 = Hispanic_Male_2010 + Hispanic_Female_2010
  )





\newpage


# PLOTTING


\newpage



# Example 1: Compare Percent Renter vs. Owner Occupied Housing (2010 vs. 2020)
housing_tenure <- merged_2010_data %>%
  select(CTYNAME_2010, PercentRenterOccupied_2010, PercentOwnerOccupied_2010) %>%
  rename(
    CTYNAME = CTYNAME_2010, 
    PercentRenterOccupied = PercentRenterOccupied_2010,
    PercentOwnerOccupied = PercentOwnerOccupied_2010
  ) %>%
  mutate(Year = 2010) %>%
  bind_rows(
    merged_2020_data %>%
      select(CTYNAME_2020, PercentRenterOccupied_2020, PercentOwnerOccupied_2020) %>%
      rename(
        CTYNAME = CTYNAME_2020,
        PercentRenterOccupied = PercentRenterOccupied_2020,
        PercentOwnerOccupied = PercentOwnerOccupied_2020
      ) %>%
      mutate(Year = 2020)
  )

# Convert data to long format for comparison
housing_tenure_long <- housing_tenure %>%
  pivot_longer(cols = c(PercentRenterOccupied, PercentOwnerOccupied),
               names_to = "HousingType",
               values_to = "Percentage")

# Plot renter vs. owner-occupied housing
ggplot(housing_tenure_long, aes(x = as.factor(Year), y = Percentage, fill = HousingType)) +
  geom_boxplot() +
  scale_fill_manual(values = c( "PercentRenterOccupied" = "#FDD5D5", "PercentOwnerOccupied" = "#C3B1E1"),
                    labels = c("Owner-Occupied", "Renter-Occupied")) +
  labs(title = "Comparison of Renter vs. Owner-Occupied Housing by Year",
       x = "Year",
       y = "Percentage of Housing Type",
       fill = "Housing Type") +
  theme_minimal()




# Example 2: Median Income Comparison Between Renters and Owners (2010 vs. 2020)
median_income <- merged_2010_data %>%
  select(CTYNAME_2010, Renter_MedianIncome_2010, Owner_MedianIncome_2010) %>%
  rename(
    CTYNAME = CTYNAME_2010,
    Renter_MedianIncome = Renter_MedianIncome_2010,
    Owner_MedianIncome = Owner_MedianIncome_2010
  ) %>%
  mutate(Year = 2010) %>%
  bind_rows(
    merged_2020_data %>%
      select(CTYNAME_2020, Renter_MedianIncome_2020, Owner_MedianIncome_2020) %>%
      rename(
        CTYNAME = CTYNAME_2020,
        Renter_MedianIncome = Renter_MedianIncome_2020,
        Owner_MedianIncome = Owner_MedianIncome_2020
      ) %>%
      mutate(Year = 2020)
  )

# Transform data to long format
median_income_long <- median_income %>%
  pivot_longer(
    cols = c(Renter_MedianIncome, Owner_MedianIncome),
    names_to = "HousingType",
    values_to = "MedianIncome"
  ) %>%
  mutate(HousingType = case_when(
    HousingType == "Owner_MedianIncome" ~ "Owners",
    HousingType == "Renter_MedianIncome" ~ "Renters"
    
  ))

# Plot with prettier colors
ggplot(median_income_long, aes(x = as.factor(Year), y = MedianIncome, fill = HousingType)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("Owners" = "#C3B1E1", "Renters" = "#FDD5D5"), # Light pink and purple
    labels = c("Owner-Occupied", "Renter-Occupied")
  ) +
  labs(
    title = "Comparison Median Income Renters & Owners (2010 vs. 2020)",
    x = "Year",
    y = "Median Income",
    fill = "Housing Type"
  ) +
  theme_minimal()






# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(maps)

# Use a map of California counties
california_map <- map_data("county") %>% filter(region == "california")

# Summarize total renter income for each year
# Assuming `merged_2010_data` and `merged_2020_data` have the same structure

# Summing renter income columns for 2010 and 2020
merged_2010_data <- merged_2010_data %>%
  mutate(Total_Renter_Income_2010 = rowSums(select(., starts_with("Renter_Income_")), na.rm = TRUE))

merged_2020_data <- merged_2020_data %>%
  mutate(Total_Renter_Income_2020 = rowSums(select(., starts_with("Renter_Income_")), na.rm = TRUE))

# Prepare `merged_2010_data` for joining with map data
new_merged_2010_data <- merged_2010_data %>%
  mutate(
    CTYNAME_2010 = gsub(" county$", "", CTYNAME_2010, ignore.case = TRUE), # Remove "County" at the end
    CTYNAME_2010 = tolower(CTYNAME_2010) # Convert to lowercase
  ) %>%
  rename(County = CTYNAME_2010)

# Prepare `merged_2020_data` for joining with map data
new_merged_2020_data <- merged_2020_data %>%
  mutate(
    CTYNAME_2020 = gsub(" county$", "", CTYNAME_2020, ignore.case = TRUE), # Remove "County" at the end
    CTYNAME_2020 = tolower(CTYNAME_2020) # Convert to lowercase
  ) %>%
  rename(County = CTYNAME_2020)

# Merge map data with renter income for 2010
california_renter_map_2010 <- left_join(california_map, new_merged_2010_data, by = c("subregion" = "County"))

# Merge map data with renter income for 2020
california_renter_map_2020 <- left_join(california_map, new_merged_2020_data, by = c("subregion" = "County"))

# Plot heat map for 2010 renter income
renter_income_2010_plot <- ggplot(data = california_renter_map_2010, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Total_Renter_Income_2010), color = "white") +
  scale_fill_gradient(low = "lightpink", high = "darkblue", name = "Renter Income (2010)") +
  labs(title = "Renter Income Distribution in California (2010)") +
  theme_minimal()

# Plot heat map for 2020 renter income
renter_income_2020_plot <- ggplot(data = california_renter_map_2020, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Total_Renter_Income_2020), color = "white") +
  scale_fill_gradient(low = "lightpink", high = "darkblue", name = "Renter Income (2020)") +
  labs(title = "Renter Income Distribution in California (2020)") +
  theme_minimal()

# Display the plots
print(renter_income_2010_plot)
print(renter_income_2020_plot)




# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(maps)

# Use a map of California counties
california_map <- map_data("county") %>% filter(region == "california")

# Summarize total renter and owner income for each county
# Assuming `merged_2010_data` and `merged_2020_data` have the same structure

# Summing renter income columns for 2010
merged_2010_data <- merged_2010_data %>%
  mutate(Total_Renter_Income_2010 = rowSums(select(., starts_with("Renter_Income_")), na.rm = TRUE),
         Total_Owner_Income_2010 = rowSums(select(., starts_with("Owner_Income_")), na.rm = TRUE))

# Combine `merged_2010_data` with map data
california_map <- california_map %>%
  mutate(subregion = tolower(subregion)) # Ensure lowercase for joining

new_merged_2010_data <- merged_2010_data %>%
  mutate(
    CTYNAME_2010 = gsub(" county$", "", CTYNAME_2010, ignore.case = TRUE), # Remove "County" at the end
    CTYNAME_2010 = tolower(CTYNAME_2010) # Convert to lowercase
  )

new_merged_2010_data <- new_merged_2010_data %>%
  rename(County = CTYNAME_2010)


new_merged_2010_data$County
# Merge map data with renter income
california_renter_map <- left_join(california_map, new_merged_2010_data, by = c("subregion" = "County"))

# Plot heat map for renter income
ggplot(data = california_renter_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Total_Renter_Income_2010), color = "white") +
  scale_fill_gradient(low = "lightpink", high = "darkblue", name = "Renter Income") +
  labs(title = "Renter Income Distribution in California (2010)") +
  theme_minimal()

# Merge map data with owner income
california_owner_map <- left_join(california_map, new_merged_2010_data, by = c("subregion" = "County"))

# Plot heat map for owner income
ggplot(data = california_owner_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Total_Owner_Income_2010), color = "white") +
  scale_fill_gradient(low = "lightpink", high = "darkblue", name = "Owner Income") +
  labs(title = "Owner Income Distribution in California (2010)") +
  theme_minimal()




library(tidyverse)
library(reshape2)

# Subset relevant columns for race and gender by year and group (renter/owner)
race_gender_cols <- c("White_Female", "White_Male", "Black_Female", "Black_Male",
                      "Asian_Female", "Asian_Male", "Hispanic_Female", "Hispanic_Male",
                      "Native_American_Female", "Native_American_Male")

# Add "_2010" and "_2020" suffixes for renter and owner
renter_cols_2010 <- paste0(race_gender_cols, "_2010")
owner_cols_2010 <- paste0(race_gender_cols, "_2010")
renter_cols_2020 <- paste0(race_gender_cols, "_2020")
owner_cols_2020 <- paste0(race_gender_cols, "_2020")

# Combine 2010 and 2020 datasets into one for comparison
renter_data_2010 <- merged_2010_data %>% select(CTYNAME_2010, all_of(renter_cols_2010)) %>%
  mutate(Year = "2010", Tenure = "Renter")
owner_data_2010 <- merged_2010_data %>% select(CTYNAME_2010, all_of(owner_cols_2010)) %>%
  mutate(Year = "2010", Tenure = "Owner")
renter_data_2020 <- merged_2020_data %>% select(CTYNAME_2020, all_of(renter_cols_2020)) %>%
  mutate(Year = "2020", Tenure = "Renter")
owner_data_2020 <- merged_2020_data %>% select(CTYNAME_2020, all_of(owner_cols_2020)) %>%
  mutate(Year = "2020", Tenure = "Owner")

# Rename columns for consistency
colnames(renter_data_2010) <- colnames(owner_data_2010) <- colnames(renter_data_2020) <-
  colnames(owner_data_2020) <- c("County", race_gender_cols, "Year", "Tenure")

# Combine all data into one
combined_data <- bind_rows(renter_data_2010, owner_data_2010, renter_data_2020, owner_data_2020)

# Melt data for visualization
long_data <- melt(combined_data, id.vars = c("County", "Year", "Tenure"),
                  variable.name = "Race_Gender", value.name = "Population")


# Aggregate data by Race_Gender, Year, and Tenure
overall_summary <- long_data %>%
  group_by(Race_Gender, Year, Tenure) %>%
  summarise(Total_Population = sum(Population, na.rm = TRUE))

# Create a single bar plot
overall_plot <- ggplot(overall_summary, aes(x = Race_Gender, y = Total_Population, fill = interaction(Year, Tenure))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Overall Race-Gender Distribution (2010 vs 2020)",
    x = "Race & Gender",
    y = "Total Population",
    fill = "Year & Tenure"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.7, "cm")
  )


print(overall_plot)






# Filter data for renters and owners separately
renter_summary <- overall_summary %>% filter(Tenure == "Renter")
owner_summary <- overall_summary %>% filter(Tenure == "Owner")

# Create bar plot for renters
renter_plot <- ggplot(renter_summary, aes(x = Race_Gender, y = Total_Population, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Race-Gender Distribution for Renters (2010 vs 2020)",
    x = "Race & Gender",
    y = "Total Population",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.7, "cm")
  )

# Create bar plot for owners
owner_plot <- ggplot(owner_summary, aes(x = Race_Gender, y = Total_Population, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Race-Gender Distribution for Owners (2010 vs 2020)",
    x = "Race & Gender",
    y = "Total Population",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.7, "cm")
  )

# Print the plots
print(renter_plot)
print(owner_plot)





\newpage
# MODELS
\newpage

#delta values (changes from 2010 to 2020)

# Calculate delta for numeric columns only
delta_data <- merged_2020_data %>%
  select(-CTYNAME_2020) %>%  # Exclude the county name column from calculation
  rename_with(~ gsub("_2020", "", .)) %>%  # Rename columns to remove "_2020"
  mutate(across(everything(), ~ . - merged_2010_data[[paste0(cur_column(), "_2010")]], .names = "Delta_{.col}"))

# Combine deltas with county names
delta_data <- cbind(CTYNAME = merged_2010_data$CTYNAME_2010, delta_data)



#Baseline Model - Linear Regression

baseline_model <- lm(
  Delta_PercentRenterOccupied ~ Delta_Total_Population + Delta_Renter_MedianIncome + Delta_Total_White +
    Delta_Total_Male,
  data = delta_data
)

colnames(delta_data)

summary(baseline_model)


\newpage

#Normal Model - Linear Regression



normal_model <- lm(
  Delta_PercentRenterOccupied ~ Delta_Total_Population + Delta_Renter_MedianIncome + 
    Delta_Total_White + Delta_Total_Black + Delta_Total_Asian + Delta_Renter_Income_75k_100k +
    Delta_Renter_Income_100k_150k + Delta_Hispanic_Female + Delta_White_Male +
    Delta_Age_18_to_64 + Delta_Age_65_and_Over,
  data = delta_data
)

summary(normal_model)







\newpage

#COMPARING MODELS


summary(baseline_model)$r.squared
summary(normal_model)$r.squared

AIC(baseline_model)

AIC(normal_model)


par(mfrow = c(2, 2))  # Divide plot area
plot(baseline_model, which = 1:2, main = "Baseline Model Residuals")
plot(normal_model, which = 1:2, main = "Normal Model Residuals")





# Predictions for baseline model
baseline_predictions <- predict(baseline_model, newdata = delta_data)
baseline_rmse <- sqrt(mean((delta_data$Delta_PercentRenterOccupied - baseline_predictions)^2))

# Predictions for normal model
normal_predictions <- predict(normal_model, newdata = delta_data)
normal_rmse <- sqrt(mean((delta_data$Delta_PercentRenterOccupied - normal_predictions)^2))

cat("Baseline Model RMSE:", baseline_rmse, "\n")
cat("Normal Model RMSE:", normal_rmse, "\n")






plot(baseline_model$fitted.values, delta_data$Delta_PercentRenterOccupied,
     pch = 19, xlab = "Fitted Values", ylab = "Actual Values",
     main = "Baseline Model: Fitted vs. Actual")
abline(a = 0, b = 1, col="red")

plot(normal_model$fitted.values, delta_data$Delta_PercentRenterOccupied,
     pch = 19, xlab = "Fitted Values", ylab = "Actual Values",
     main = "Normal Model: Fitted vs. Actual")
abline(a = 0, b = 1, col="red")


plot(baseline_model$fitted.values, residuals(baseline_model),
     pch=19, xlab="Fitted Values", ylab="Residuals",
     main="Baseline Model Residuals vs. Fitted")
abline(h=0, col="red")

plot(normal_model$fitted.values, residuals(normal_model),
     pch=19, xlab="Fitted Values", ylab="Residuals",
     main="Normal Model Residuals vs. Fitted")
abline(h=0, col="red")



\newpage


# Test Robustness

Adding random noise:
  

# Create a perturbed version of the data
perturbed_data <- delta_data
n <- nrow(delta_data)
perturbed_data$Delta_Total_Population <- perturbed_data$Delta_Total_Population + rnorm(n, mean = 0, sd = 100)

# Refit the baseline model on perturbed data
perturbed_noise_model <- lm(
  Delta_PercentRenterOccupied ~ Delta_Total_Population + Delta_Renter_MedianIncome + 
    Delta_Total_White + Delta_Total_Black + Delta_Total_Asian + Delta_Renter_Income_75k_100k +
    Delta_Renter_Income_100k_150k + Delta_Hispanic_Female + Delta_White_Male +
    Delta_Age_18_to_64 + Delta_Age_65_and_Over,
  data = perturbed_data
)

summary(perturbed_noise_model)
```

Scaling / Transforming Predictors:
  
  ```{r}
# Scale one predictor (e.g., Delta_Total_Population)
scaled_data <- delta_data
scaled_data$Delta_Total_Population <- scaled_data$Delta_Total_Population * 1.1  # Increase by 10%

# Refit the model
perturbed_scaled_model <- lm(
  Delta_PercentRenterOccupied ~ Delta_Total_Population + Delta_Renter_MedianIncome + 
    Delta_Total_White + Delta_Total_Black + Delta_Total_Asian + Delta_Renter_Income_75k_100k +
    Delta_Renter_Income_100k_150k + Delta_Hispanic_Female + Delta_White_Male +
    Delta_Age_18_to_64 + Delta_Age_65_and_Over,
  data = scaled_data
)

summary(perturbed_scaled_model)



# Remove an influential data point
reduced_data <- delta_data[c(-19, -30, -34, -37, -38), ] 
# Refit the model
perturbed_reduced_model <- lm(
  Delta_PercentRenterOccupied ~ Delta_Total_Population + Delta_Renter_MedianIncome + 
    Delta_Total_White + Delta_Total_Black + Delta_Total_Asian + Delta_Renter_Income_75k_100k +
    Delta_Renter_Income_100k_150k + Delta_Hispanic_Female + Delta_White_Male +
    Delta_Age_18_to_64 + Delta_Age_65_and_Over,
  data = reduced_data
)

# Compare to the original model
summary(perturbed_reduced_model)




# Calculate RMSE and AIC for normal_model
rmse_norm <- sqrt(mean(residuals(normal_model)^2))
aic_norm <- AIC(normal_model)


# Calculate RMSE and AIC for perturbed_noise_model
rmse_noise <- sqrt(mean(residuals(perturbed_noise_model)^2))
aic_noise <- AIC(perturbed_noise_model)

# Calculate RMSE and AIC for perturbed_scaled_model
rmse_scaled <- sqrt(mean(residuals(perturbed_scaled_model)^2))
aic_scaled <- AIC(perturbed_scaled_model)

# Calculate RMSE and AIC for perturbed_reduced_model
rmse_reduced <- sqrt(mean(residuals(perturbed_reduced_model)^2))
aic_reduced <- AIC(perturbed_reduced_model)


# Print the results
cat("Normal Model:\n")
cat("  RMSE:", rmse_norm, "\n")
cat("  AIC:", aic_norm, "\n\n")



# Print the results
cat("Perturbed Noise Model:\n")
cat("  RMSE:", rmse_noise, "\n")
cat("  AIC:", aic_noise, "\n\n")

cat("Perturbed Scaled Model:\n")
cat("  RMSE:", rmse_scaled, "\n")
cat("  AIC:", aic_scaled, "\n\n")

cat("Perturbed Reduced Model:\n")
cat("  RMSE:", rmse_reduced, "\n")
cat("  AIC:", aic_reduced, "\n")




# Create a data frame for comparison
model_comparison <- data.frame(
  Model = c("Original Model", "Perturbed Noise", "Perturbed Scaled", "Perturbed Reduced"),
  RMSE = c(rmse_norm, rmse_noise, rmse_scaled, rmse_reduced),
  AIC = c(aic_norm, aic_noise, aic_scaled, aic_reduced)
)

print(model_comparison)






# Simulating strong assumption

# -------------------------


set.seed(42)

# Define the number of counties
n <- nrow(delta_data)

original_residuals <- resid(normal_model)


# Generate a random dependency matrix (e.g., using normal distribution)
dependency_matrix <- matrix(rnorm(n * n, mean = 0, sd = 1), nrow = n, ncol = n)

# Symmetrize the matrix to mimic spatial-like relationships
dependency_matrix <- (dependency_matrix + t(dependency_matrix)) / 2

# Normalize to row-standardized weights
dependency_matrix <- dependency_matrix / rowSums(abs(dependency_matrix))




# Simulate residuals with Brownian-inspired dependency
rho <- 0.8  # Spatial correlation parameter
epsilon <- rnorm(n, mean = 0, sd = sd(original_residuals))  # Random noise

# Solve for spatially correlated residuals
spatial_residuals <- solve(diag(n) - rho * dependency_matrix) %*% epsilon
delta_data$spatial_residuals <- as.numeric(spatial_residuals)




delta_data$Delta_PercentRenterOccupied_simulated <- fitted(normal_model) + delta_data$spatial_residuals




simulated_model <- lm(
  Delta_PercentRenterOccupied_simulated ~ Delta_Total_Population + Delta_Renter_MedianIncome + 
    Delta_Total_White + Delta_Total_Black + Delta_Total_Asian + Delta_Renter_Income_75k_100k +
    Delta_Renter_Income_100k_150k + Delta_Hispanic_Female + Delta_White_Male +
    Delta_Age_18_to_64 + Delta_Age_65_and_Over,
  data = delta_data
)

summary(simulated_model)




# Coefficient comparison
summary(simulated_model)

# Residual comparison
par(mfrow = c(1, 2))
plot(resid(normal_model), main = "Original Model Residuals")
plot(resid(simulated_model), main = "Simulated Model Residuals")

# Model comparison
anova(normal_model, simulated_model)






# Calculate RMSE for normal_model
rmse_normal <- sqrt(mean(resid(normal_model)^2))

# Calculate RMSE for simulated_model
rmse_simulated <- sqrt(mean(resid(simulated_model)^2))

# Print the RMSE values
cat("RMSE for normal_model:", rmse_normal, "\n")
cat("RMSE for simulated_model:", rmse_simulated, "\n")



