"
Aaron Williams
ISTA 320

5/5/2021

Shiny Graph
"

library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

# Reading in the data

cpu = read_csv("https://docs.google.com/spreadsheets/d/1L15RpFZ5_ITkWMtfbCJTNlRmfblD5ar2Oxf2J61L1t8/gviz/tq?tqx=out:csv")


# DATA DESCRIPTION:

# I found this data on data.gov. The dataset contains all computer processing units and their attributes including clock speed, core count, thread count, and much more. I am planning on using this data to see how computer processors have improved over time.



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

# Shiny Graph
#-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Aaron Williams - CPU Analysis"),
  
  # Sidebar with a slider input for number of threads to display 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Thread_Count",
                  "Number of Threads:",
                  min = 2,
                  max = 64,
                  value = 64)
    ),
    
    # Show a plot of the generated distribution of threads in csv
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a bar plot
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    cpu_finished %>%
      top_n(as.numeric(input$Thread_Count)) %>%
      ggplot(aes(x = threads)) +
      geom_bar() +
      labs(title = "Count of CPU's with a selected number of threads", x = 'Threads', y = "Count of CPU's", caption = "Bar graph that shows the count of CPU's with a selected number of threads through the sliding input")
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)