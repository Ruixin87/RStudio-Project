wine <- read.csv("C:/Users/xiaox/OneDrive/1.Study/R/winequality-red.csv")
library(dplyr)
library(ggplot2) # Plotting
library(shiny)   # Develop an interactive visualisation
library(shinythemes)
library(reshape2) # Melt data for plotting

wine$wine_quality <- ifelse(wine$quality <= 5, "bad", "good") #create the new category attribute wine_quality
head(wine)

#Question 4.6: To analysis three variables, 1 category variable and 2 numeric variables, show the good and bad wines with alcohol Vs.Volatile Acidity 
#Solution: use Scatter plot, the x-axis can represent alcohol, the y-axis can represent volatile_acidity, and the points can be colored based on wine_quality (good or bad).

ui <- fluidPage(
  headerPanel("Exploration of Red Wines DataSet"),
  
  pageWithSidebar(
    headerPanel("Red Wines Analysis"),
    sidebarPanel(
      h4('The variables are as follows:'),
      h6('1.    fixed acidity   : Amount of tartaric acid (in grams) per decimeter cubed of wine (dm^3) [g/dm^3]'),
      h6('2.    volatile acidity     : Amount of acetic acid (in grams) per decimeter cubed of wine (dm^3) [g/dm^3]'),
      h6('3.    citric acid     : Amount of citric acid (in grams) per decimeter cubed of wine (dm^3) [g/dm^3]'),
      h6('4.    residual sugar   : Amount of residual sugar (in grams) per decimeter cubed of wine (dm^3) [g/dm^3]'),
      h6('5.    chlorides : Amount of sodium chloride (in grams) per decimeter cubed of wine (dm^3) [g/dm^3]'),
      h6('6.    free sulfur dioxide       : Amount of free sulfur dioxide (in milligrams) per decimeter cubed of wine (dm^3) [mg/dm^3]'),
      h6('7.    total sulfur dioxide       : Amount of total sulfur dioxide (in milligrams) per decimeter cubed of wine (dm^3) [mg/dm^3]'),
      h6('8.    density        : Density of wine [g/cm^3]'),
      h6('9.    pH    : Acidity/Alkalinity of the wine'),
      h6('10.   sulphates     : Amount of potassium sulphate (in grams) per decimeter cubed of wine (dm^3) [g/dm^3]'),
      h6('11.   alcohol       : Amount of Alcohol by percent volume'),
      h6('12.   quality    : A score between 0 and 10 based on Sensory Data'),
      h6('13.   wine_quality    : categore of quality'),
      br(),
      
      
      h4('Select different parameters:'),
      sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(wine), value=min(1, nrow(wine)), step=100, round=0),
      selectInput('x', 'X Axis Measure', names(wine), selected = names(wine)[[11]]),
      selectInput('y', 'Y Axis Measure', names(wine), selected = names(wine)[[2]]),
      selectInput('color', 'Measure Color', c('None', names(wine)), selected = names(wine)[[13]]),
      textInput('caption', 'Plot Caption', value='Scatter plot for Alcohol vs. Volatile Acidity')
    ),
    mainPanel(
      h4('Introduction & How to use'),
      p("Red wines dataset contains the different attributes of wines and have almost 1599 rows in the dataset. Using this shiny application we can plot different attributes and App will plot those. This is very interactive application and gives us idea of wines dataset and relation between different attributes."),
      br(),
      plotOutput('plot')
    )
  )
)

server <- function(input, output) {
  dataset <- reactive({
    wine[sample(nrow(wine), input$sampleSize),]
  })
  
  # Plot based on user inputs
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color = input$color)
    
    
    
    p <- p + ggtitle(input$caption)
    
    print(p)
    
  }, height=800)
  
}

shinyApp (ui = ui , server = server )



