library(shiny)
library(ggvis)
library(ggplot2)
library(reshape2)
library(plotly)

setwd('~/Documents/Data Viz/')

df <- read.csv('dataset_Facebook.csv',na.strings="",header = TRUE, sep = ';')
ui = fluidPage(
  
  #  Title
  tags$h3("Facebook Metric Visualisation"),
  #titlePanel("Visualisation of Facebook Metrics"),
  
  # Sidebar with slider and controls for animation
  sidebarLayout(
    # sidebar with slider
    
    # Show the animated graph
    mainPanel(
      tabsetPanel(
        tabPanel("Bubble Plot",plotlyOutput(outputId="plot_bubble")),
        tabPanel("Scatter Plot",plotOutput(outputId="plot_scatter"), hover = "plot_hover"),
        verbatimTextOutput("info"),
        tabPanel("Parallel Coordinate Plot",plotlyOutput(outputId="plot_parallel"))
      )
    ),
    sidebarPanel(
      # Slider with looping
      sliderInput("theMonth", "Month", 1, 12, 12, step = 1, ticks=TRUE),
      width=4
      # Slider with looping
    #  sliderInput("thePop", "Population", 10, 30, 20, step = 1,ticks=FALSE)
    )
  )
)

# server section
server = function(input, output) {
  output$plot_bubble <- renderPlotly({
    dat_sub <- df[df$Post.Month==input$theMonth,]
    # create the graph
   ggplot(dat_sub, aes(x=comment, y=Total.Interactions)) +
               geom_point(aes(size=like,colour=share))  +
               xlab("# of Comment") + ylab("Total Interactions") + scale_size(range=c(0,10))
              
    
  }) 
  output$plot_scatter <- renderPlot({
    dat_sub <- df[df$Post.Month==input$theMonth,]
    # wrap("cor", size = 3) # runs ggally_cor, but sets the arg size to 3
    ggpairs(dat_sub, 8:12, mapping = ggplot2::aes(color = Type),
            axisLabels = "show",
            upper = list(
              continuous = wrap("cor", size = 4)))
    
  },width=750,height=500) 
  output$info <- renderText({
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      return (e$x)
    }
    
    paste0(
      "hover: ", xy_range_str(input$plot_hover)
    )
  })
  output$plot_parallel <- renderPlotly({
    dat_sub <- df[df$Post.Month==input$theMonth,]
    # wrap("cor", size = 3) # runs ggally_cor, but sets the arg size to 3
    ggplotly(ggparcoord(dat_sub, 16:19, groupColumn = "Type",alphaLines = 0.4,showPoints = TRUE
    )+  xlab('Columns') + ylab('Values' ))
  })
}
shinyApp(ui = ui, server = server)
