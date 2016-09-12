rm(list = ls())

library(shiny)
library(ggplot2)
library(tibble)
library(tidyr)
library(dplyr)
library(lazyeval)
source('app/stratify-data.R')

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Create dependent data.
  len <- 2 * 1e+3
  dist1 <- runif(len, min = 1, max = 5) 
  dist2 <- rnorm(len, mean = 0, sd = 3)
  dist3 <- rnorm(len, mean = 4, sd = 1) 
  dist4 <- rlnorm(len, meanlog = 0, sdlog = 0.5)
  l <- list("var1" = dist1 * dist3,
            "var2" = dist2,
            "var3" = dist3 * dist2,
            "var4" = dist4 * dist3 + dist2)
  original.data <- as_data_frame(l)

  #----------------------------------------------------------------------------# 
  # Stratify the data:
  numb.strat <- reactive({
    input$numb.strata
  })
  stratified.data <- reactive({
    stratify(original.data, numb.strat())
  })

  #----------------------------------------------------------------------------#
  # Preparing the data plottet in the first plot:
  # Calculate the coordinates of vertical lines, 
  # which represent the strata of the first variable.
  var1.vlines <- reactive({
    stratified.data() %>%                       # take the stratified data,
      select(var1) %>%                          # select the first column,
      distinct() %>%                            # and take only distinct rows,
      slice(-1)                                 # remove the first row, because
  })                                            # it is not needed for the plot
  # Calculate the coordinates of horizontal segments,
  # which represent the strata of the secound variable. 
  var2.segments <- reactive({
    n <- numb.strat()
    max.org.data <- max(original.data$var1)
    stratified.data() %>%                       # take the stratified data,
      select(var1, var2) %>%                    # select the first two columns,
      distinct() %>%                            # take only distinct rows,
      mutate(var1.end = lead(var1,              # create a variable, which is
                             n = n)) %>%        # var1 shifted by n=numb.strat,
      mutate(var1.end = ifelse(is.na(var1.end), # replace NA values by the maxi-
                               max.org.data,    # mum value of the 1. variable
                               var1.end)) %>%   # remove the first and last line
      slice(seq(from = 1,                       # of each stratum, so that they
                to = numb.strat() ^ 2,          # are not plotted
                by = numb.strat()) * -1)        
  })
  # Prepare data points for the plot. As original.data
  # does not change, this does not need to be reactive.
  dta <- original.data %>%                      # take only distinct rows of the           
      distinct(var1, var2)                      # first two variables
  
  #----------------------------------------------------------------------------#
  # Make the variables reactive to double clicks in the first plot.
  # These variables are used to calculate the limits of the variables
  # shown in the second plot.
  point <- reactiveValues(x = NULL, y = NULL)   # the double-clicked point,
  se.exp <- reactiveValues(var1.leq.x = NULL,   # standard evaluation 
                           max.var1 = NULL,     # expressions, which depend on 
                           var2.leq.y = NULL,   # the clicked point,
                           max.var2 = NULL)
  stratum <- reactiveValues(limits1 = NULL,     # limits define the stratum,
                            limits.upper.1 = NULL, 
                            limits.upper.2 = NULL)
  # Observe if a point is double-clicked. 
  observeEvent(input$plot1_dblclick, {
    dbl.click <- input$plot1_dblclick
    if (!is.null(dbl.click)) {                  # if there is a double click
      point$x <- dbl.click$x                    # assign its coordinates to 
      point$y <- dbl.click$y                    # the point variable,
    }
    # Construct standard evaluation (SE) expressions 
    # using the coordinates of the double clicked point.
    if (!is.null(point$x)) {
      se.exp$var1.leq.x <- interp(~p < x,                  # take all data 
                                  p = as.name("var1"),     # points, which are
                                  x = as.double(point$x))  # smaller than x and
      se.exp$max.var1 <- interp(~p == max(p),              # take than the 
                                p = as.name("var1"))       # maximum of them;
      se.exp$var1.gr.x <- interp(~p > x,                   # than take all the
                                 p = as.name("var1"),      # data points which
                                 x = as.double(point$x))   # are greater than x
      se.exp$min.var1 <- interp(~p == min(p),              # and take than the 
                                p = as.name("var1"))       # smallest of them;
      se.exp$var2.leq.y <- interp(~p <= y,                 # do the same as
                                  p = as.name("var2"),     # above with the 
                                  y = as.double(point$y))  # second parameter;
      se.exp$max.var2 <- interp(~p == max(p),              # this way one gets
                                p = as.name("var2"))       # the limits of the 
      se.exp$var2.gr.y <- interp(~p >= y,                  # stratum
                                 p = as.name("var2"),
                                 y = as.double(point$y))
      se.exp$min.var2 <- interp(~p == min(p), 
                                p = as.name("var2"))
      # Use the SE to calculate the limits of
      # the strata of variable 3 and 4.
      stratum$limits1 <- stratified.data() %>%       # take the stratified data,
        filter_(se.exp$var1.leq.x) %>%               # filter out the 
        filter_(se.exp$max.var1) %>%                 # rows corresponding
        filter_(se.exp$var2.leq.y) %>%               # to the stratum using the 
        filter_(se.exp$max.var2)                     # expressions above
      stratum$limits.upper.1 <- stratified.data() %>%  
        filter_(se.exp$var1.gr.x) %>%                # filter out the 
        filter_(se.exp$min.var1)                     # rows corresponding
      stratum$limits.upper.2 <- stratified.data() %>%  
        filter_(se.exp$var2.gr.y) %>%                # to the stratum using the 
        filter_(se.exp$min.var2)                     # expressions above
    }
  })
  
  #----------------------------------------------------------------------------#
  # Prepare the data for the second plot as a reactive.
  # Calculate the coordinates of vertical lines, 
  # which represent the strata of the third variable.
  var3.vlines <- reactive({
    if (is.null(stratum)) return()              # no point double clicked
    stratum$limits1 %>%                         # clicked in plot 1,
      select(var3) %>%                          # select the variariables to be 
      distinct() %>%                            # plotted and take only distinct
      slice(-1)                                 # rows, remove the first line
  })  
  # Calculate the coordinates of horizontal segments,
  # which represent the strata of the fourth variable. 
  var4.segments <- reactive({
    if (is.null(stratum)) return()
    n <- numb.strat()
    max.org.data <- max(original.data$var3)
    min.org.data <- min(original.data$var3)
    stratum$limits1 %>%                         # clicked in plot 1,
      select(var3, var4) %>%                    # select the first two columns,
      distinct() %>%                            # take only distinct rows,
      mutate(var3.end = lead(var3,              # create a new variable, which
                           n = n)) %>%          # is var1 shifted by numb.strat,
      mutate(var3.end = ifelse(is.na(var3.end), # replace NA values by the maxi-
                               max.org.data,    # mum value of the 1. variable 
                               var3.end)) %>%   
      slice(seq(from = numb.strat(),
                to = numb.strat() ^ 2,
                by = numb.strat()) * -1) %>% 
      mutate(var3 = replace(var3, var3 == min(var3), 
                            min.org.data))
  })
  # Prepare data points to mark the selected stratum.
  # It is also needed for the second plot to draw data
  # points of variable 3 and variable 4.
  dta2 <- reactive({
    if (is.null(stratum)) return()
    line.left <- stratum$limits1$var1[1]
    line.bottom <- stratum$limits1$var2[1]
    line.right <- stratum$limits.upper.1$var1[1]
    line.top <- stratum$limits.upper.2$var2[1]
    if (is.na(line.right)) line.right = Inf
    if (is.na(line.top)) line.top = Inf
    original.data %>% 
      filter(var1 > line.left) %>% 
      filter(var2 > line.bottom) %>% 
      filter(var1 < line.right) %>% 
      filter(var2 < line.top)
  })
  
  #----------------------------------------------------------------------------#
  # Generating the first plot. Credits to Oskar Forsmo on SO:
  # http://stackoverflow.com/questions/33312817/updating-plot-output-in-shiny-r
  plot1.layer <- reactiveValues(layer1 = NULL)
  observe({
    output$plot1 <- renderPlot({
      ggplot() + 
        geom_point(data = dta, aes(var1, var2), 
                   colour = "lightblue", alpha = 0.5) +
        geom_vline(xintercept = var1.vlines()$var1, colour = "blue") +
        geom_segment(data = var2.segments(), aes(x = var1, xend = var1.end, 
                                                 y = var2, yend = var2), 
                     colour = "blue") +
        theme(legend.position = "none") + 
        labs(x = "Parameter 1", y = "Parameter 2") +
        plot1.layer$layer1
    })
  })
  observeEvent(input$plot1_dblclick, {
    if (is.null(input$plot1_dblclick$x)) return()
    plot1.layer$layer1 <- geom_point(data = dta2(), aes(var1, var2), 
                                     colour = "darkblue", alpha = 0.2) 
  })

  #----------------------------------------------------------------------------#
  # Generate the second plot.
  plot2.layer <- reactiveValues(layer1 = NULL, layer2 = NULL, layer3 = NULL)
  observe({
    if (is.null(original.data)) return()
    output$plot2 <- renderPlot({
      ggplot() + 
        geom_point(data = original.data, aes(var3, var4), 
                   colour = "gray", alpha = 0.2) +
        theme(legend.position = "none") +
        labs(x = "Parameter 3", y = "Parameter 4") +
        plot2.layer$layer1 +
        plot2.layer$layer2 +
        plot2.layer$layer3
    })
  })
  observeEvent(input$plot1_dblclick, {
    if (is.null(dta2())) return()  
    plot2.layer$layer1 <- geom_point(data = dta2(), aes(var3, var4), 
                                     colour = "red", alpha = 0.2)
    plot2.layer$layer2 <- geom_vline(xintercept = var3.vlines()$var3, 
                                     colour = "darkred")
    plot2.layer$layer3 <- geom_segment(data = var4.segments(), 
                                       aes(x = var3, xend = var3.end, 
                                           y = var4, yend = var4),
                                       colour = "darkred")
  })
}
