#
# Shiny web app that plots housing data around UC campuses.
# Currently uses American Community Survey 5-year data from the
# US Census Bureau for 2020. There are a lot of different
# possible rental estimates: explore more here 
#   https://censusreporter.org/topics/housing/
#   |  "Contract rent is the monthly rent agreed to without adjustments
#   |  for utilities or other payments. Gross rent is similar to selected
#   |  monthly owner costs. It is the sum of contract rent and the average
#   |  cost of the utilities (electricity, gas, and water and sewer) and fuel
#   |  (oil, coal, kerosene, wood, etc)."
#   "Median (gross) rent, all types = "B25064_001",
#     https://censusreporter.org/tables/B25064/
#   "Median (gross) rent by number of bedrooms"
#     "0 bedroom" = "B25031_002",
#     "1 bedroom" = "B25031_003",
#     "2 bedroom" = "B25031_004",
#     "3 bedroom" = "B25031_005",
#     "4 bedroom" = "B25031_006"
#
#   "Median (contract) rent, all types = "B25058_001",
#     https://censusreporter.org/tables/B25058/
#   Contract rents are not further broken down by housing type.
#

library(shiny)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggforce)
library(sf)
library(cowplot)

# Housing data gets generated and written in associated R files, not here.
housing_data_acs5 = st_read("acs5.shp")
housing_data_acs1 = st_read("acs1_puma.shp")


# Dataframe that contains campus coordinates (in lat/lon)
campuses <- tribble(
  ~campus, ~latitude, ~longitude,
  "Mission Bay", 37.767020, -122.391227,
  "Parnassus", 37.763414, -122.458140,
  "UC Berkeley", 37.871848, -122.258862,
  "UC Davis", 38.537829, -121.760940,
  "UC Irvine", 33.641692, -117.843427,
  "UC Los Angeles", 34.068877, -118.445546,
  "UC Merced", 37.365209, -120.423764,
  "UC Riverside", 33.973501, -117.328086,
  "UC Santa Barbara", 34.410980, -119.849365,
  "UC Santa Cruz", 36.991074, -122.062240,
  "UC San Diego", 32.879763, -117.231106,
  "Stanford", 37.427560, -122.170738
)

campuses_sf <- campuses %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Main UI for app
ui <- fluidPage(

    titlePanel("Rent burden tester"),

    # Options sidebar
    sidebarLayout(
        sidebarPanel(
            sliderInput("burden",
                        "Rent burden (% of salary):",
                        min = 0,
                        max = 100,
                        value = 30),
            sliderInput("salary",
                        "Gross salary:",
                        min = 0,
                        max = 100000,
                        value = 54000),
            sliderInput("window",
                        "Width (mi):",
                        min = 1,
                        max = 50,
                        value = 20),
            sliderInput("distance",
                        "Circle radius (mi):",
                        min = 0,
                        max = 50,
                        value = 10),
            selectInput("campus",
                        "Campus:",
                        c("Mission Bay" = 1,
                          "Parnassus" = 2,
                          "UC Berkeley" = 3,
                          "UC Davis" = 4,
                          "UC Irvine" = 5,
                          "UC Los Angeles" = 6,
                          "UC Merced" = 7,
                          "UC Riverside" = 8,
                          "UC Santa Barbara" = 9,
                          "UC Santa Cruz" = 10,
                          "UC San Diego" = 11,
                          "*Stanford" = 12
                        )),
            selectInput("beds",
                        "Housing type:",
                        c("All housing (gross)" = "B25064_001",
                          "All housing (contract)" = "B25058_001",
                          "0 bedroom (gross)" = "B25031_002",
                          "1 bedroom (gross)" = "B25031_003",
                          "2 bedroom (gross)" = "B25031_004",
                          "3 bedroom (gross)" = "B25031_005",
                          "4 bedroom (gross)" = "B25031_006"
                        )),
            radioButtons("year", "Year:",
                         c("2020 (ACS 5-year tract-level data)" = 1,
                           "2021 (ACS 1-year PUMA-level data)" = 2),
                         selected = 1),
            selectInput("color_scale",
                        "Color scale:",
                        c("Magma" = "magma",
                          "Inferno" = "inferno",
                          "Plasma" = "plasma",
                          "Civids" = "civids",
                          "Mako" = "mako",
                          "Rocket" = "rocket",
                          "Turbo" = "turbo")),
            checkboxInput("invert_scale", "Inverted scale", value = FALSE),
            checkboxInput("shadow", "Draw all tracts", value = TRUE),
            checkboxInput("circle", "Draw circle", value = TRUE)
        ),

        # Draws the output
        mainPanel(
          verbatimTextOutput("clientdataText"),
          plotOutput("burdenPlot", height = "auto")
        )
    )
)

server <- function(input, output, session) {

    output$burdenPlot <- renderPlot({
      
      if(input$year == 1) {
        housing_data <- housing_data_acs5
        year = "2020"
      } else if(input$year == 2) {
        housing_data <- housing_data_acs1
        year = "2021"
      }
      
      housing_variable = input$beds
      
      color_scale = input$color_scale
      direction = 1 - 2*input$invert_scale
      
      alpha = input$shadow * 0.3

      # Window and Distance are in miles
      distance = input$distance
      window = input$window
      campus = slice(campuses, strtoi(input$campus))
      
      xmin = campus$longitude - window/(2*54.6)
      xmax = campus$longitude + window/(2*54.6)
      
      ymin = campus$latitude - window/(2*69)
      ymax = campus$latitude + window/(2*69)
      
      # Calculate the (monthly) rent burden threshold based on input
      burden = input$burden/100
      salary = input$salary
      cutoff = (salary*burden)/12
      
      #scale coordinates
      # Scale will be 1/5th of the window
      scale_x_start = xmin + (window/20)/(54.6)
      scale_x_end = scale_x_start + (window/5)/(54.6)
      scale_y = ymin + (window/20)/69
      
      scale_text = paste((window/5), "mi")

      title = paste(campus$campus, ", ", burden*100, "% burden at $", salary, ", from ", year, sep = "")

      main_plot <- ggplot() + 
        geom_sf(data = housing_data %>% filter(variable == housing_variable), aes(fill = estimate), color = "black", alpha = alpha,) + 
        geom_sf(data = housing_data %>% filter(variable == housing_variable & estimate < cutoff), aes(fill = estimate), legend.title = 'Median rent (dollars)',) + 
        scale_fill_viridis_c(option = color_scale, direction = direction, na.value = "green") +
        xlim(xmin, xmax) +
        ylim(ymin, ymax) +
        geom_point(data = campus, aes(x = longitude, y = latitude), size=4) + 
        geom_segment(aes(x = scale_x_start, xend = scale_x_end, y = scale_y, yend = scale_y)) +
        geom_segment(aes(x = scale_x_start, xend = scale_x_start, y = scale_y, yend = scale_y + (window/50)/(54.6))) +
        geom_segment(aes(x = scale_x_end, xend = scale_x_end, y = scale_y, yend = scale_y + (window/50)/(54.6))) +
        annotate("text", x = (scale_x_start + scale_x_end)/2, y = scale_y - (window/50)/69, size = 6, label = scale_text) +
        theme_classic() +
        ggtitle(title) + 
        labs(fill = "Median rent (dollars)") +
        theme(legend.position='bottom',
              legend.key.width = unit(0.1, 'npc'),
              legend.title = element_text(size=22),
              legend.text = element_text(size=16),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      

      if(input$circle) {
        main_plot <- main_plot + geom_ellipse(data = campus, linetype = "solid", aes(x0 = longitude, y0 = latitude, b = distance/69, a = distance/54.6, angle=0))
      }
      
      return(main_plot)
      
    }, height = reactive(session$clientData$output_burdenPlot_width), width = reactive(session$clientData$output_burdenPlot_width))
}

# Run the application 
shinyApp(ui = ui, server = server)

