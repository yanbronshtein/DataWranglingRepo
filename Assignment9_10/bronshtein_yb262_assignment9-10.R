library(sna)
library(tidyverse)
library(igraph)
library(intergraph)
library(GGally)
library(rvest)
library(choroplethr)
library(Hmisc)
library(choroplethrMaps)
library(tidycensus)
library(gridExtra)
#PROBLEM 1:
  
#  Generate an undirected random graph with 20 vertices and probability of a tie
#(forming an edge) equal 0.7. Calculate the mean degree from these graphs. 
#Call it MD20.
#Now, keeping the probability of a tie fixed at 0.7, repeat the above step with 
#30, 40, 50, 60, 70, 80, 90 and 100 vertices, by writing a simple R function. 
#In each case calculate the average mean degree. Denote them by MD30, ..., MD 100.
#Plot the average mean degrees against the number of vertices. 
#Is the plot result consistent with your expectation? Explain.
set.seed(1)
net <- rgraph(n = 20, tprob = 0.7, mode = "graph")
MD20 <- mean(sna::degree(net, gmode = "graph"))
MD20

get_mean_degree <- function(num_vertices) {
  set.seed(1)
  net <- rgraph(n = num_vertices, tprob = 0.7, mode = "graph")
  return(mean(sna::degree(net, gmode = "graph")))
}
mean_list <- NULL
for (i in seq(from = 20, to = 100, by = 10)) {
  mean_list <- c(mean_list, get_mean_degree(i))
}
mean_degrees <- c("MD20","MD30", "MD40", "MD50", "MD60", "MD70", "MD80", "MD90", "MD100")
graph_means_t <- tibble(Vertices = seq(from = 20, to = 100, by = 10), 
                        MeanDegrees = mean_degrees, 
                        MeanValues = mean_list)
graph_means_t
ggplot(data = graph_means_t, mapping = aes(x = Vertices, y = MeanValues)) +
  geom_point() +
  geom_line()
#todo: fix labels

#The results indicate a linear relationship. As the number of vertices increase,
# the meanValues grows


#PROBLEM 2:
  
#1.Download the data frames "Dataset1-Media-Example-NODES.csv and 
#"Dataset1-Media-Example-EDGES.csv" from 
#https:// (Links to an external site.)net/network-visualization 
#(Links to an external site.), and create an igraph network object.
#2.Convert the igraph to a network object in the sna package
#3. Plot the network using, using separate colors for the nodes based on the 
#vertex attribute media.type and make the size of the nodes proportional to the 
#vertex attribute audience.size. [Hint: Use network::get.vertex.attribute]
#4. Calculate the mean degree and density of the network using appropriate 
#functions in the sna package.
nodes <- read_csv("C:\\Users\\Julia\\Downloads\\Dataset1-Media-Example-NODES.csv")
links <- read_csv("C:\\Users\\Julia\\Downloads\\Dataset1-Media-Example-EDGES.csv")
net_2 <- graph_from_data_frame(d=links, vertices = nodes, directed = TRUE) %>% #directed is default
  simplify(remove.multiple = F, remove.loops = T)

#2.Convert igraph to network object in the sna package
network_obj <- asNetwork(net_2)
#3. Plot the network using, using separate colors for the nodes based on the 
#vertex attribute media.type and make the size of the nodes proportional to the 
#vertex attribute audience.size. [Hint: Use network::get.vertex.attribute]

g_size <- network::get.vertex.attribute(network_obj, "audience.size")
g_color <- network::get.vertex.attribute(network_obj, "media.type")

ggnet2(network_obj, arrow.size = 5, arrow.gap = 0.02, size = g_size, 
       label = FALSE,  color = g_color, palette = "Set2")

#4. Calculate the mean degree and density of the network using appropriate 
#functions in the sna package.
density <- network.density(network_obj)
density

mean_degree <- mean(sna::degree(network_obj, gmode = "digraph"))
mean_degree

#PROBLEM 3:
  
#Scrape the country-wise population data from 
#https://www.worldometers.info/world-population/population-by-country/ 
#Plot the population density (P/Km2) obtained from this table on a 
#country-wise choropleth map. Make sure to

#1.Clean the data to make it compatible with the country-wise world choropleth map
#2.Maximize the overlap between the two data frames (the one obtained from the 
#scraped data and the choropleth country data frame), 
#i.e., if a country appears in both data frames, possibly with different names, it must be plotted
#3.List the countries, if any, in the scraped data frame that do not appear in 
#the choropleth country data frame (after appropriate cleaning)
#4.List the countries, if any, in the choropleth country data frame that do 
#not appear in the scraped data frame (after appropriate cleaning)
data("country.regions")
url <- "https://www.worldometers.info/world-population/population-by-country/"
worldometer_data <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

population_density_t <- worldometer_data[[1]] %>% dplyr::select(-1)

clean_column <- function(data, col_name) {
  #Cast data as a tibble and extract the col_name as a vector
  col_vec <- as_tibble(data) %>% 
    pull(col_name)
  
  #Check if the vector is numeric. If not, the data requires processing
  #All commas and + need to be removed, the vector needs to be converted to numeric,
  #and the NA's need to be replaced with 0's
  if (col_name == "value") {
    return(
      col_vec %>%
        str_replace_all("[,]", "") %>%
        as.numeric())
  } 
  #If the data is already numeric, only the NA's need to be replaced with 0's
  else {  
    return(
      col_vec %>% tolower()
    )
  }
} 


cleaned_t <- population_density_t %>% 
  dplyr::select(1, 5)
names(cleaned_t)[1] <- "region"
names(cleaned_t)[2] <- "value"


cleaned_t <- cleaned_t %>% 
  mutate(region = clean_column(cleaned_t, "region"), 
         value = clean_column(cleaned_t, "value")) %>%
  arrange(region)

###########################################################################
country_regions <- country.regions
misfits = anti_join(country_regions, cleaned_t)
misfits
###########################################################################


#countries not represented

choropleth_countries <- c("united states of america","republic of serbia",
               "united republic of tanzania","the bahamas",
               "democratic republic of the congo",
               "republic of congo","czech republic","guinea bissau",
               "ivory coast","macedonia","east timor","swaziland")
worldometer_countries <- c("united states","serbia","tanzania","bahamas","dr congo",
                 "congo","czech republic (czechia)","guinea-bissau",
                 "côte d'ivoire","north macedonia","timor-leste","eswatini")


#Replace scraped named with choropleth names
for (i in seq(worldometer_countries)) {
  cleaned_t$region[cleaned_t$region==worldometer_countries[i]] <- 
    choropleth_countries[i]
}

#countries not present in choropleth table
misfits_choro <- anti_join(cleaned_t, country_regions)
misfits_choro

choroplethr::country_choropleth(cleaned_t, title = "Population Density by Country")

#PROBLEM 4:
  
#Obtain 2015-2019 5-year aggregated ACS tract-wise data on NJ median household 
#income and rental. Combine them into a single data frame.

#1.Plot the tract-wise rental against the median household income and comment.
#2.Fit a linear regression equation of rental against median household income 
#and report the summary.
#3.Looking at the plot, suggest ways to improve the model fit. 
#Fit the improved model and report the R2.
#4.Obtain the rental data for year=2019 (5-year aggregate from 2015-2019) 
#and year=2014 (5-year aggregate from 2010-2014), and plot the percentage 
#changes for each county in a column or bar diagram, in increasing or
#decreasing order of percentage increase.

my_census_key <- "68b91a0cb82e99d93fcf934623a6b9ad5ba04986"
census_api_key(my_census_key, install = TRUE, overwrite = TRUE)


v19 <- load_variables(2019, "acs5", cache = TRUE)
nj_median_household_income_and_rental <- get_acs(geography = "tract",
                                      variables = c(medincome = "B19013_001", 
                                                    medrental = "B25064_001"),
                                      state = "NJ", geometry = TRUE)

income_t <- filter(nj_median_household_income_and_rental, variable == "medincome")
rental_t <- filter(nj_median_household_income_and_rental, variable == "medrental")

p_income <- ggplot(data = income_t, mapping = aes(fill = estimate, color = estimate)) +
  geom_sf() + 
  labs(title = "Income distribution in New Jersey")

p_rental <- ggplot(data = rental_t, mapping = aes(fill = estimate, color = estimate)) +
  geom_sf() + 
  labs(title = "
       Rental distribution in New Jersey")
grid.arrange(p_income, p_rental, ncol = 2)

#1.Plot the tract-wise rental against the median household income and comment.


acs_data_t <- get_acs(geography = "tract",
                      variables = c(medincome = "B19013_001", 
                                    medrental = "B25064_001"),
                      state = "NJ")

acs_data_t <- acs_data_t %>% select(NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

acs_data_t <- acs_data_t %>% filter(!is.na(medrental), !is.na(medincome))
ggplot(data = acs_data_t, mapping = aes(x = medincome, y = medrental)) +
  geom_point() + 
  geom_smooth(method = "lm")

#It seems as though the two variables are positively correlated
#2.Fit a linear regression equation of rental against median household income 
acs_lm <- lm(medrental ~ medincome, data = acs_data_t)

summary_lm <- summary(acs_lm)
summary_lm
lm_rsq <- summary_lm$r.squared
lm_rsq
#3.Looking at the plot, suggest ways to improve the model fit. 
#Fit the improved model and report the R2.
acs_poly <- lm(medrental ~ poly(medincome, 5), data = acs_data_t)
summary_poly <- summary(acs_poly)
poly_rsq <- summary_poly$r.squared
poly_rsq

#Fitting a poly model with order=5 increased the R^2 by almost 0.01



#4.Obtain the rental data for year=2019 (5-year aggregate from 2015-2019) 
#and year=2014 (5-year aggregate from 2010-2014), and plot the percentage 
#changes for each county in a column or bar diagram, in increasing or
#decreasing order of percentage increase.

rental_14 <- get_acs(geography = "county",
                     variables = c(medrental= "B25064_001"), 
                     year = 2014, state = "NJ") %>% 
  rename(region = NAME, value = estimate)


rental_19 <- get_acs(geography = "county",
                     variables = c(medrental= "B25064_001"), 
                     year = 2019, state = "NJ") %>% 
  rename(region = NAME, value = estimate)


percent_change_t <- choroplethr::calculate_percent_change(rental_14, rental_19)

ggplot(data = percent_change_t,
       mapping = aes(x = value, y = fct_reorder(region,value))) + 
  geom_col(fill = "magenta") +
  labs(y="County",
       x="% Change in rent",
       title = "Percentage change of rentals in NJ 2014-2019")




