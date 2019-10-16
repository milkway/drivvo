# Packages
library(tidyverse)

# Shell:  csplit --silent --elide-empty-files --prefix=drivvo_data --suffix-format="_%d.csv" 20191016_171458_DRIVVO.csv "/##/" "{*}" 

refuelling <- read_csv("data/drivvo_data_0.csv", 
                       col_names = c("Odometer (km)", "Date", "Fuel", "Price / L", "Total cost", "Volume", "Filled tank completely", "Second fuel", "Price / L_1",  "Total cost_1",  "Volume_1",  
                                     'Filled tank completely" 2,"Third fuel', "Price / L_2", "Total cost_2", "Volume_2", 'Filled tank completely" 3,"Fuel efficiency', "Gas station", "Reason",
                                     "Notes", "X20", "X21"),
                       col_types = cols(
                         `Odometer (km)` = col_double(),
                         Date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                         Fuel = col_character(),
                         `Price / L` = col_double(),
                         `Total cost` = col_double(),
                         Volume = col_double(),
                         `Filled tank completely` = col_character(),
                         `Second fuel` = col_logical(),
                         `Price / L_1` = col_double(),
                         `Total cost_1` = col_double(),
                         Volume_1 = col_double(),
                         `Filled tank completely" 2,"Third fuel` = col_character(),
                         `Price / L_2` = col_logical(),
                         `Total cost_2` = col_double(),
                         Volume_2 = col_double(),
                         `Filled tank completely" 3,"Fuel efficiency` = col_double(),
                         `Gas station` = col_character(),
                         Reason = col_character(),
                         Notes = col_character(),
                         X20 = col_skip(), 
                         X21 = col_skip()
                       ), 
                       skip = 2) 


refuel_clean <- refuelling %>% 
  select("Odometer (km)", "Date", "Fuel", "Price / L", "Total cost", "Volume", "Filled tank completely", "Gas station", "Reason", "Notes") %>% 
  separate(Reason, into = c("Efficiency", "Unit"), sep = "[:space:]") %>% mutate(Efficiency = as.numeric(Efficiency)) 

refuel_clean %>% 
  select(Date, Fuel, Efficiency, Station = Notes) %>%
  mutate(`Gas Station` = lead(Station), `Fuel Used` = lead(Fuel)) %>% 
  filter(Efficiency > 0) %>% 
  ggplot() +
  geom_point(aes(x = Date, y = Efficiency, color = `Gas Station`, shape = `Fuel Used`), size = 2) +
  geom_line(aes(x = Date, y = Efficiency), alpha = .25, linetype = 3)

refuel_clean %>% 
  select(Date, Fuel, Efficiency, Station = Notes) %>%
  mutate(`Gas Station` = lead(Station), `Fuel Used` = lead(Fuel)) %>% 
  filter(Efficiency > 0) %>% 
  group_by(Station, `Fuel Used`) %>% 
  summarise(Min = min(Efficiency), Mean = mean(Efficiency), Median = median(Efficiency),  Max = max(Efficiency), SD = sd(Efficiency), n = n())

# Kernels: "gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"
refuel_clean %>% 
  select(Date, Fuel, Efficiency, Station = Notes) %>%
  mutate(`Gas Station` = lead(Station), `Fuel Used` = lead(Fuel)) %>% 
  filter(Efficiency > 0) %>% 
  ggplot() + 
  geom_density(aes(x = Efficiency, fill = `Gas Station`), alpha = .5, kernel = "gaussian", n = 64) + facet_wrap( . ~ `Fuel Used`,nrow = 2) +
  theme(legend.position="bottom")

refuel_clean %>% 
  select(Date, Fuel, Efficiency, Station = Notes) %>%
  mutate(`Gas Station` = lead(Station), `Fuel Used` = lead(Fuel)) %>% 
  filter(Efficiency > 0) %>% 
  ggplot() + 
  geom_boxplot(aes(x = `Gas Station`, y = Efficiency,  fill = `Gas Station`), alpha = .5) + facet_wrap( . ~ `Fuel Used`,nrow = 2) +
  theme(legend.position="bottom")


refuel_clean %>% 
  select(Date, Fuel, Efficiency, Station = Notes) %>%
  mutate(`Gas Station` = lead(Station), `Fuel Used` = lead(Fuel)) %>% 
  filter(Efficiency > 0, `Fuel Used` == "Gas Premium") %>% 
  ggplot() + 
  geom_boxplot(aes(x = `Gas Station`, y = Efficiency,  fill = `Gas Station`), alpha = .75) +
  theme(legend.position="top") +
  labs(y = "Efficiency (Km/L)")


refuel_clean %>% 
  select(Date, Fuel, Efficiency, Station = Notes) %>%
  mutate(`Gas Station` = lead(Station), `Fuel Used` = lead(Fuel)) %>% 
  filter(Efficiency > 0, `Fuel Used` == "Gas Premium") %>% 
  select(Efficiency, `Gas Station`) %>% 
  t.test(Efficiency ~ `Gas Station`, data = .)
   
