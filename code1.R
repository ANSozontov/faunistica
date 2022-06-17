library(shiny)
library(tidyverse)
species<- readxl::read_xlsx("DB1.xlsx", sheet = "taxa")
adm    <- readxl::read_xlsx("DB1.xlsx", sheet = "adm")
users  <- readxl::read_xlsx("DB1.xlsx", sheet = "users")
publ   <- readxl::read_xlsx("DB1.xlsx", sheet = "publications")

ui <- fluidPage(
    navbarPage(title = "Input your data", 
        tabPanel("One record", textInput(input$txt1, "your text1")),
        tabPanel("One species, a few ", textInput(input$txt1, "your text1")),
        tabPanel("One record", textInput(input$txt1, "your text1"))
    ),
    title = "Faunistica: input data",
    lang = "ru"
)