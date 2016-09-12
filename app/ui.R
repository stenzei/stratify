library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Fluid row layout 
  fluidRow(
    column(width = 12, class = "well",
      sliderInput("numb.strata", "number of strata",
                 min = 3, max = 9, step = 1, value = 4, width = 200),
      helpText("Click inside the blue framed strata to 
               see the stratification of Parameter 3 and 4 below."),
      plotOutput("plot1", height = 300, width = 500,
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                  id = "plot1_brush",
                  resetOnNew = TRUE)),
      plotOutput("plot2", height = 300, width = 500)
    )
  )
)
