#data manipulation


######################
##########CH1#########
######################

counties %>%
  select(state, county, population, poverty)

counties_selected <- counties %>%
  select(state, county, population, private_work, public_work, self_employed)

counties_selected %>%
  arrange(desc(public_work))

counties_selected %>%
  filter(state == "California", population > 1000000)

counties_selected %>%
  filter(state == "Texas", population > 10000) %>%
  arrange(desc(private_work))

counties_selected %>%
  mutate(public_workers = public_work * population / 100) %>%
  arrange(desc(public_workers))

counties_selected <- counties %>%
  select(state, county, population, men, women)

counties_selected %>%
  mutate(proportion_women = women / population)

counties %>%
  # Select the five columns 
  select(state, county, population, men, women) %>%
  # Add the proportion_men variable
  mutate(proportion_men = men / population) %>%
  # Filter for population of at least 10,000
  filter(population >= 10000) %>%
  # Arrange proportion of men in descending order 
  arrange(desc(proportion_men))



######################
##########CH2#########
######################

# Use count to find the number of counties in each region
counties_selected %>%
  count(region, sort = TRUE)

# Find number of counties per state, weighted by citizens
counties_selected %>%
  count(state, wt = citizens, sort = TRUE)

counties_selected %>%
  # Add population_walk containing the total number of people who walk to work 
  mutate(population_walk = population * walk / 100) %>%
  # Count weighted by the new column
  count(state, wt = population_walk, sort = TRUE)

# Summarize to find minimum population, maximum unemployment, and average income
counties_selected %>%
  summarize(min_population = min(population), max_unemployment = max(unemployment),
            average_income = mean(income))

# Add a density column, then sort in descending order
counties_selected %>%
  group_by(state) %>%
  summarize(total_area = sum(land_area),
            total_population = sum(population)) %>%
  mutate(density = total_population / total_area) %>%
  arrange(desc(density))

# Calculate the average_pop and median_pop columns 
counties_selected %>%
  group_by(region, state) %>%
  summarize(total_pop = sum(population)) %>%
  summarize(average_pop = mean(total_pop),
            median_pop = median(total_pop))

# Group by region and find the greatest number of citizens who walk to work
counties_selected %>%
  group_by(region) %>%
  top_n(1, walk)

counties_selected %>%
  group_by(region, state) %>%
  # Calculate average income
  summarize(average_income = mean(income)) %>%
  # Find the highest income state in each region 
  top_n(1)


counties_selected %>%
  group_by(state, metro) %>%
  summarise(total_pop = sum(population)) %>%
  top_n(1, total_pop) %>%
  ungroup() %>%
  count(metro)


######################
##########CH3#########
######################