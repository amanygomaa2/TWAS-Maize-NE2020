library(tidyverse)
library(SpATS)
library(dplyr)


# Read in data 
NE2020 <- read.csv("NE2020_Spatial_v2.3_NO_SP.csv")
colnames(NE2020)[1] <- "plot"
colnames(NE2020)


# Define function
# Define function
getSpatialCorrections <- function(data, response)
{
  #data <- NE2021
  #response <- "DaysToPollen"
  # Declare empty df and levels of locs
  df.sp <- tibble(plot = NULL, '{response}':= NULL)
  loc.df <- filter(data, !is.na(Row) & !is.na(Column) & !is.na(.data[[response]])) %>%
    mutate(as.factor(plot))
  
  ColumnKnots <- floor(max(loc.df$Column, na.rm = TRUE)/2) + 1
  RowKnots <- floor(max(loc.df$Row, na.rm = TRUE)/2) + 1
  model <- SpATS(response, genotype = 'plot', genotype.as.random = TRUE,
                 spatial = ~ SAP(Column, Row, nseg = c(ColumnKnots, RowKnots)),
                 data = loc.df)
  
  # Save the coeff spatial trend plot
  #png_filename <- paste0("output.bc/NE2020.orderSampleIncluded/SpATS_coeff_trend_", response, ".png")
  #png(png_filename)
  #plot.SpATS(model, main = response)
  #dev.off()
  
  # Extract BLUPS
  intercept <- model$coeff['Intercept']
  sp <- as_tibble(model$coeff, rownames = 'plot') %>%
    filter(!is.na(plot)) %>%
    rowwise() %>%
    mutate(value = value + intercept) %>%
    rename('{response}':= value)
  # Bind to df
  df.sp <- bind_rows(df.sp, sp) %>%
    filter(!str_detect(plot, "[a-zA-Z]")) %>%  # Keep only rows without letters in `plot`
    mutate(plot = as.numeric(plot))  # Convert `plot` to numeric
  # Return df
  return(df.sp)
}


for(j in c('Anthesis', 'Silking', 'LeafAngle', 'HundredKernelMassGrams', 'CobWeightGrams', 'TotalGrainMassGrams','BushelAcreEquivalent',
'GrainPercentMoisture','EarLengthCM', 'EarWidthCM', 'EarFilledLengthCM', 'KernelRowNumber','KernelsPerRow', "PercentFill",'SouthernRustSeverityScore', 'LeafAreaIndex', 'LeafLengthCM', 'LeafWidthCM', 'PlantHeightCM', 'ExtantLeafNumber1', 'NodesWithBraceRoots', 'ExtantLeafNumber2', 'TasselSpikeLengthCM' ))
{
  NE2020 <- full_join(NE2020, getSpatialCorrections(NE2020, j), join_by(plot), suffix = c('', '.sp'), keep = FALSE)
}

write.table(NE2020, 'NE2020_Spatial_v2.3_SP_AG.csv', sep = ',', row.names = FALSE, col.names = TRUE)
