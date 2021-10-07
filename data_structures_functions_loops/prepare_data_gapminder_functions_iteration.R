# Prepare data for functions/iteration section

library(tidyverse)

# read data
gapminder <- read_csv("https://github.com/tavareshugo/r-eda-gapminder/raw/gh-pages/_episodes_rmd/data/raw/gapminder1960to2010_socioeconomic.csv")

# fix typos etc.
gapminder <- gapminder %>%
  # fix typos in main_religion and world region
  mutate(main_religion = str_to_title(str_squish(main_religion)),
         world_region = str_to_title(str_replace_all(world_region, "_", " "))) %>%
  # fit typos in income groups, which needs more steps
  mutate(income_groups = str_remove(income_groups, "_income")) %>%
  mutate(income_groups = str_to_title(str_replace_all(income_groups, "_", " "))) %>%
  # fix/create numeric variables
  mutate(life_expectancy_female = as.numeric(life_expectancy_female),
         life_expectancy_male = ifelse(life_expectancy_male == -999, NA, life_expectancy_male))


# # Easy version ----------------------
#
# # split by country
# gapminder_nested <- gapminder %>%
#   select(country, world_region, year, income_per_person) %>%
#   group_nest(country, keep = TRUE)
#
# # output directory
# dir.create("data/gapminder_by_country_easy", recursive = TRUE)
#
# for(i in 1:nrow(gapminder_nested)){
#   write_csv(gapminder_nested$data[[i]],
#             paste0("data/gapminder_by_country_easy/", gapminder_nested$country[i], ".csv"))
# }


# Harder version ---------------------


# split by country
gapminder_nested <- gapminder %>%
  select(country, world_region, year, income_per_person) %>%
  group_nest(country, world_region)

# output directory
dir.create("data/multiple_files", recursive = TRUE)

for(i in 1:nrow(gapminder_nested)){
  # write world region as the first line of the file
  write_lines(paste0("Region:", gapminder_nested$world_region[i], "\n"),
              paste0("data/multiple_files/", gapminder_nested$country[i], ".csv"))

  # write the rest as a CSV
  write_csv(gapminder_nested$data[[i]],
            paste0("data/multiple_files/", gapminder_nested$country[i], ".csv"),
            col_names = TRUE,
            append = TRUE)
}


