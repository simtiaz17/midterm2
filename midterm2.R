library(rio)
library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(labelled)
library(doBy)
# importing dataset 
inequality_data <- import('inequality.xlsx') 
# checking out the data 
head(inequality_data)
# 3. this dataset is cross-sectional because it's only 2015 

# 4. inequality_gini for denmark 28.2 and sweden 
# making subsets of sweden and denmark 
inequality_se <- subset(inequality_data, country == c( 'Sweden'))
inequality_dk <- subset(inequality_data, country == c('Denmark'))
# looking at only inequality_gini
gini_se <- gini_scores$inequality_gini
gini_dk <- inequality_dk$inequality_gini
# printing values
gini_se
gini_dk 


# 5. 
# same process as above 
inequality_br <- subset(inequality_data, country == c('Brazil'))
gini_br <- inequality_br$inequality_gini
gini_br


# 6. higher is better 

# 7. looking at dataframe 
head(inequality_data)

# 8. removing accents on Belarus 
accent.remove <- function(s) {
  oldu <- 'ú' #original u with the accent 
  newu <- 'u' #replacement 
  su <- chartr(oldu, newu, s)
  for(i in seq_along(oldu)) su <- gsub(oldu[i], newu[i], s, fixed = TRUE)
  #loop to get rid of the u with accent 
  su #return su 
}
#applying accent.remove 
inequality_data$country =  accent.remove(inequality_data$country)

# 9. sorting by inequality_gini 
inequality_data = inequality_data[order(inequality_data$inequality_gini),]
# looking at result 
head(inequality_data, 5)

# 10. mean gini = 36.81375
# because of missing values need to tell function to ignore those 
mean <- mean(inequality_data$inequality_gini, na.rm = TRUE)

# 11. ifelse 
# making the dummy variables 


inequality_data <- 
inequality_data %>%
  mutate(high_inequality = ifelse(inequality_gini > 36.8,
                                  1,
                                  0))
inequality_data <- 
inequality_data %>% 
  mutate(low_inequality = ifelse(inequality_gini < 36.8,
                                  1,
                                  0))


# 12. crosstab 
summaryBy(inequality_gini ~ high_inequality, 
          data = inequality_data, 
          FUN=c(mean, length))
summaryBy(inequality_gini ~ low_inequality, 
          data = inequality_data, 
          FUN = c(mean, length))

# 13. for loop 
actors <- c('World Bank', 'African Development Bank', 'Bill and Melinda Gates Foundation')
for (i in actors){
  print(i)
}
# updating WDI R package 
remotes::install_github('vincentarelbundock/WDI')
library(WDI)
# 14. finding indicator 
WDIsearch('poverty')
# 15. importing variable 
povheadcount = WDI(country = 'all', 
                      indicator = c('SI.POV.DDAY'), 
                      start = 2015, end = 2015, extra = FALSE, cache = NULL)

# 16. renamee variable
setnames(povheadcount, 'SI.POV.DDAY', 'poverty_ratio')
# 17. merge variable into dataset using left_join to keep rows from inequality_data
merged_df <- dplyr::left_join(x=inequality_data,
                 y=povheadcount,
                 by = c('country', 'year'))
# remove .x .y
merged_df <- 
  merged_df %>%
  select(-c('iso2c.y')) %>%
  rename('iso2c' = 'iso2c.x')

# 18. remove NAs 
merged_df <- na.omit(merged_df, select = c('inequality_gini', 'poverty_ratio'))

# 19. filter and piping to keep gini>30 
data_greater_30 <- 
  merged_df %>%
  dplyr::filter(inequality_gini>30)

# 20. 
grep('ai', data_greater_30)

# 21. sum of inequality_gini in data_greater_30 
sum <- apply(data_greater_30, 2, sum)
sum

# 22. label variables 
var_label(merged_df) <- list(`iso2c` = 'ISO-2 Country Code',
                             `country` = 'Country',
                             `inequality_gini` = 'GINI Score',
                             `year` = 'Year', 
                             `poverty_ratio` = 'Poverty Headcount Ratio')
export(merged_df, file = 'final_data.dta')

