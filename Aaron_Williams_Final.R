#Aaron Williams
#ISTA 320
#5/4/2021
#Final Project

library(tidyverse)
library(ggplot2)
library(lubridate)


# Reading in the data

cpu = read_csv("https://docs.google.com/spreadsheets/d/1L15RpFZ5_ITkWMtfbCJTNlRmfblD5ar2Oxf2J61L1t8/gviz/tq?tqx=out:csv")

glimpse(cpu)

# Preprocessing

cpu_filter = cpu %>%
  filter(!is.na(cpu$date))  #Filtering out NA data

cpu_finished = cpu_filter %>%
  select(-created_at, -updated_at, -manufacturer_id, -processor_family_id,
         -microarchitecture_id, -code_name_id, -technology_id, -cache_on_id,  # Removing data
         -cache_off_id, - die_photo_id, -max_clock, -source) %>%
  mutate(threads = hw_nthreadspercore * hw_ncores) %>%
  mutate(cores = hw_ncores)

cpu_finished = cpu_finished %>%
  select(-hw_nthreadspercore, -hw_ncores)


cpu_finished = cpu_finished %>%    # Filtering data for the final df.
  filter(!is.na(date)) %>%
  filter(date > as.Date('1990-01-01')) %>%
  filter(cores < 50) %>%
  filter(clock < 15000) %>%
  filter(!is.na(transistors))

cpu_transistor = cpu_finished %>%
  filter(date >= as.Date('1996-01-01'))

# Scatter plot showing the increase in transistors over time.

ggplot(cpu_transistor,
       aes(x = date, y = transistors)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = 'Clock speed over time', x = 'Year', y = "Transistor Count") +
  theme(plot.title = element_text(color = 'Blue', hjust = 0.5, size = 16), text = element_text(size = 14))

# Scatter plot showing the improvments in CPU's over time.

ggplot(cpu_finished,
       aes(x = date, y = clock, size = threads)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + 
  labs(title = 'Clock speed and amount of threads over time', x = 'Year', y = 'Clock Speed (Ghz)') +
  theme(plot.title = element_text(color = 'Blue', hjust = 0.5, size = 16), text = element_text(size = 14))


