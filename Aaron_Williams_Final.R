#Aaron Williams
#ISTA 320
#5/4/2021
#Final Project

library(tidyverse)
library(ggplot2)
library(lubridate)


# DATA DESCRIPTION:

# I found this data on Kaggle. The dataset contains all computer processing units and their attributes including clock speed, core count, thread count, and much more. I am planning on using this data to see how computer processors have improved over time.



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
  filter(clock < 15000)

cpu_transistor = cpu_finished %>%
  filter(date >= as.Date('1996-01-01')) %>%
  filter(!is.na(transistors))


ggplot(cpu_transistor,
       aes(x = date, y = transistors)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = 'Transistor count over time', x = 'Year', y = "Transistor Count", caption = "Scatter plot showing the increase in transistors present in the CPU's over time. I also added in geom_smooth() to see the trends in the increase in transistors for CPU's released over the last 26 years.") +
  theme(plot.title = element_text(color = 'Blue', hjust = 0.5, size = 16), text = element_text(size = 14))


ggplot(cpu_finished,
       aes(x = date, y = clock, size = threads)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + 
  labs(title = 'Clock speed and amount of threads over time', x = 'Year', y = 'Clock Speed (Ghz)', caption = "Scatter plot showing the improvments in CPU's over time by judging the date they were released, the clock speed in GHz, as well as the number of threads present in each CPU.") +
  theme(plot.title = element_text(color = 'Blue', hjust = 0.5, size = 16), text = element_text(size = 14))


