Trashy_CMR
================
Randi Sims
2023-11-26

# Setup and Cleaning

All data originated from the two provided excel documents.

Formulas came from the biomass_equation calcs excel sheet. Previously
calculated by Kea Payton.

``` r
# Clean formulas sheet
data_formulas <- data_formulas %>% 
  select(Taxa, term, Model, estimate) %>%
  # Replacing terms to be uniform
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept",
                          term == "Length" ~ "Length_1",
                          term == "Width" ~ "Width_1",
                          term == "poly(Length, 2)1" ~ "Length_1",
                          term == "poly(Length, 2)2" ~ "Length_2",
                          term == "poly(Length, 3)1" ~ "Length_1",
                          term == "poly(Length, 3)2" ~ "Length_2",
                          term == "poly(Length, 3)3" ~ "Length_3",
                          term == "poly(Width, 2)1" ~ "Width_1",
                          term == "poly(Width, 2)2" ~ "Width_2",
                          term == "poly(Width, 3)1" ~ "Width_1",
                          term == "poly(Width, 3)2" ~ "Width_2",
                          term == "poly(Width, 3)3" ~ "Width_3")) %>%
  # Separating model column 
  separate(Model, c('Taxa_Level', 'Model')) %>%
  # Filtering rows that contain higher-level polynomials 
  filter(Model != "WPOLY3" & Model != "LPOLY3") %>%
  pivot_wider(names_from = term, values_from = estimate)

# Clean key sheet and then combine
equations <- data_key %>%
  # Combine sheets when taxa, taxa_level, and model type match
  left_join(data_formulas, data_key, 
            by = c("Taxa", "Taxa_Level", "Model")) %>%
  # Make the sheet look pretty
  select(Taxa, Taxa_Level, Model, Median, Intercept, Length_1, Width_1, Length_2, Width_2) %>%
  # If there are both codes and subcodes, only keep subcodes
  distinct_at(vars(-"Taxa_Level")) %>%
  # Only "POLY" has both, removing the main code to keep only the subcode
  filter(!(Taxa == "POLY" & Model == "LPOLY")) %>%
  # For all absent models, add median
  mutate(Model = replace_na(Model, "MEDIAN"))

# Add in model from equations to data
data <- data %>%
  left_join(select(equations, Taxa, Model), by = c("Subcode" = "Taxa"))

# Check to ensure all taxa are accounted for - these are the ones missing Biomass Calcs
data$Subcode[!data$Subcode %in% equations$Taxa]
```

    ##  [1] "CHIT" "DICH" "DICH" "DROM" "CHIT" "CHIT" "CRAB" "CRAB" "ANME" "CHIT"
    ## [11] "DROM" "PSP"  "AGRS" "PSP"  "MSPP" "DROM" "DROM" "OSTR" "PSP"  "AXDE"

Future iterations may want to check the above taxa for any available
biomass data.

------------------------------------------------------------------------

# Functions

The following are functions used to calculate biomass data with
constants from the biomass equation calculation datasheet and inputs
from measured taxa from the CMR lab.

``` r
# Linear model function
lm_func <- function(id){
  # Find the taxa
  taxa <- data$Subcode[which(data$ID == id)]
  # Find the taxa's length
  org_length <- data$Length[which(data$ID == id)]
  # Find the constants for the equation
  row <- equations[equations$Taxa == taxa, ]
  # Calculate Biomass
  biomass = row$Intercept + row$Length_1 + org_length
  return(biomass)
}

# Test 
lm_func(197)
```

    ## [1] 2.55159

``` r
# WLM function
wlm_func <- function(id){
  # Find the taxa
  taxa <- data$Subcode[which(data$ID == id)]
  # Find the taxa's length
  org_width <- data$Width[which(data$ID == id)]
  # Find the constants for the equation
  row <- equations[equations$Taxa == taxa, ]
  # Calculate Biomass
  biomass = row$Width_1 * org_width + row$Intercept
  return(biomass)
}

# Test 
wlm_func(286)
```

    ## [1] 0.48483

``` r
# LPOLY function
lpoly_func <- function(id){
  # Find the taxa
  taxa <- data$Subcode[which(data$ID == id)]
  # Find the taxa's length
  org_length <- data$Length[which(data$ID == id)]
  # Find the constants for the equation
  row <- equations[equations$Taxa == taxa, ]
  # Calculate Biomass
  biomass = row$Intercept + row$Length_1*org_length + row$Length_2*org_length^2
  return(biomass)
}

# Test
lpoly_func(4)
```

    ## [1] 1.117624

``` r
# WPOLY function
wpoly_func <- function(id){
  # Find the taxa
  taxa <- data$Subcode[which(data$ID == id)]
  # Find the taxa's length
  org_width <- data$Width[which(data$ID == id)]
  # Find the constants for the equation
  row <- equations[equations$Taxa == taxa, ]
  # Calculate Biomass
  biomass = row$Intercept + row$Width_1*org_width + row$Width_2*org_width^2
  return(biomass)
}

# Test
wpoly_func(104)
```

    ## [1] 0.2158742

``` r
# All functions run 
biomass_func <- function(id, model){
  
  if (is.na(model)) {
    biomass <- "NA"
      # Look for linear model
    } else if(model == "LM") {
      # Obtain biomass
      biomass <- lm_func(id)
      # Add biomass to the appropriate row in the dataframe
      
      # Look for LPOLY model
    } else if(model == "LPOLY") {
      biomass <- lpoly_func(id)
      
      # Look for WPOLY model
    } else if(model == "WPOLY") {
      biomass <- wpoly_func(id)
      
      # Look for WLM model
    } else if(model == "WLM") {
      biomass <- wlm_func(id)
      
      # Look for taxa that do not have model
    } else if(model == "MEDIAN") {
      # Find the taxa
      taxa <- data$Subcode[which(data$ID == id)]
      # Take the median from the equations data frame
      median <- equations$Median[which(equations$Taxa == taxa)]
      # Add biomass to the appropriate row
      biomass <- median
    } 
  return(biomass)
}

id <- data[20, "ID"]$ID
model <- data[20, "Model"]$Model
biomass_func(id, model)
```

    ## [1] 0.225606

------------------------------------------------------------------------

# For loop for dataset

All data was calculated using the created functions and then exported to
a csv file for future use.

``` r
# For loop to run throw rows in data
for(i in 1:nrow(data)){
  
  # Find the ID
  id <- data[i, "ID"]$ID
  
  # Find the Model
  model <- data[i, "Model"]$Model
  
  # If model is x model - perform function
  biomass <- biomass_func(id, model)
  
  # Add biomass equation to data column
  data$Biomass[which(data$ID == id)] <- biomass
}
```
