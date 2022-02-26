library("tidyverse")
library("maps")

#Converting CSV file into main_prison data frame
main_prison_data <- read.csv("incarceration_trends.csv")

#Creating filtered summary info data frame for relevant values
summary_info_data <- main_prison_data %>% 
                     filter(state == "WA")

summary_info_data_temp1 <- summary_info_data %>% 
                           select(black_jail_pop, white_jail_pop, total_jail_pop)
                           
summary_info_data_temp1 <- na.omit(summary_info_data_temp1)

#Highest African American Jail population
relevant_value_1 <- max(summary_info_data_temp1$black_jail_pop) 

#Ratio between Highest African American Jail population and Highest White American Jail population
relevant_value_2 <- relevant_value_1/max(summary_info_data_temp1$white_jail_pop) 

#Ratio between Highest African American Jail population and Total Jail population
relevant_value_3 <- relevant_value_1/max(summary_info_data_temp1$total_jail_pop)

summary_info_data_temp2 <- summary_info_data %>% 
  filter(year == "2018") %>% 
  select(black_jail_pop)

summary_info_data_temp2 <- na.omit(summary_info_data_temp2)

#Difference between Highest 2001 and 2018 African American Jail population
relevant_value_4 <- relevant_value_1 - max(summary_info_data_temp2$black_jail_pop)

summary_info_data_temp3 <- summary_info_data %>% 
  filter(county_name == "Pierce County") %>% 
  select(black_jail_pop)

summary_info_data_temp3 <- na.omit(summary_info_data_temp3)

#Difference between Highest King County and Pierce County African American Jail population
relevant_value_5 <- relevant_value_1 - max(summary_info_data_temp3$black_jail_pop) 

#Creating filtered data frame for trend_over_time chart
trend_over_time_data <- main_prison_data %>% 
                        filter(county_name == "King County" | county_name == "Pierce County" | county_name == "Snohomish County" | county_name == "Spokane County" | county_name == "Clark County") %>% 
                        filter(state == "WA") %>% 
                        select(year, state, county_name, black_jail_pop)

#Getting rid of null values
final_trend_over_time_data <- na.omit(trend_over_time_data)

#Creating trend_over_time chart
trend_over_time_chart <- ggplot(final_trend_over_time_data, aes(x = year, y = black_jail_pop, colour = county_name)) +
  geom_line() +
  ggtitle("Jail Population Count for African Americans from 1985 - 2018") +
  xlab("Year") +
  ylab("African American Jail Population") + 
  labs(color = "Top 5 Largest Counties in WA")

#Creating filtered data frame for variable_comparison_data chart
variable_comparison_data <- main_prison_data %>% 
                            filter(county_name == "King County") %>% 
                            filter(state == "WA") %>% 
                            select(black_jail_pop, white_jail_pop)  

#Getting rid of null values
final_variable_comparison_data <- na.omit(variable_comparison_data)

#Creating variable_comparison_chart chart
variable_comparison_chart <- ggplot(final_variable_comparison_data, aes(x = white_jail_pop, y = black_jail_pop)) +
  geom_point() +
  ggtitle("Jail Population for African vs. White Americans in King County, WA") +
  ylab("African American Jail Population") +
  xlab("White American Jail Population") +
  geom_smooth(method = "lm", color = "red") 

#Creating filtered data frame for map
initial_map_data <- main_prison_data %>% 
                    filter(year == "2018") %>% 
                    filter(state == "WA") %>% 
                    select(fips, state, black_jail_pop, total_jail_pop) %>% 
                    mutate(black_to_total_jail_pop_ratio = round(black_jail_pop / total_jail_pop, digits = 2))

#Creating data frame that can convert fips to longitude and latitude
county_shapes <-map_data("county") %>% 
                unite(polyname, region, subregion, sep = ",") %>% 
                left_join(county.fips, fips, by = "polyname")

#Combining both data frames to create map
final_map_data <- county_shapes %>% 
                  left_join(initial_map_data, by = "fips") %>% 
                  filter(state == "WA")

#Method to make map look "minimalist"
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.border = element_blank()
  )

#Creating map chart
final_map <- ggplot(final_map_data) +
  geom_polygon(
    mapping = aes(x= long, y = lat, group = group, fill = black_to_total_jail_pop_ratio),
    color = "black", size = 0.3
  ) + 
  coord_map() +
  scale_fill_continuous(limits = c(0, max(final_map_data$black_to_total_jail_pop_ratio)), na.value = "white", low = "white", high = "blue", name = "Ratio") +
  blank_theme +
  ggtitle("Ratio of African Population to Total Population in Prison in WA during 2018") 