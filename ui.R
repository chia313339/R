library(shiny)
shinyUI(fluidPage(
  titlePanel("´ú¸Õ¨Ï¥Î"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = "Choices", choices = list("Choice 1" = 1, "Choice 2" = 2)),
      sliderInput("slider1", label = "Slider", min = 0, max = 100, value = 50)
    ),
    mainPanel("deqwfwef")
  )
))