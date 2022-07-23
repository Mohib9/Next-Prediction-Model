##############################
library(shiny)
library(plotly)
library(shinyWidgets)

shinyUI(fluidPage(
    setBackgroundColor("maroon"),

        titlePanel("The Predictor"),
        
        sidebarLayout(
            sidebarPanel(
                setBackgroundColor("ghostwhite"),
                textInput("sentenceInput", "Enter with a sentence: "),
                
                h3("User Instructions"), 
                p("Enter a sentence or a single word and get the requisite predicted words on the right side."),
                p("Please give a little time to the app to run, it can sometimes take around a minute to give results."),
                h3("Written By:"),
                p("Mohib for Data Specialization of John Hopkins University provided on Coursera.")
            ),
            mainPanel(
                h3("Next possible words:"),
                plotlyOutput("sentenceOutput"),
                setBackgroundColor("black")
            )
        ),

))