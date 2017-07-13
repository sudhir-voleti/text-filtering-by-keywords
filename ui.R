#################################################
#              Text Filtering by Keywords            #
#################################################

library(shiny)
library(NLP)
library(magrittr)
library(tidytext)
library(dplyr)


shinyUI(fluidPage(
  
 titlePanel("Text Filtering by Keywords"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    fileInput("keywords", "Upload Key words text file"),
    
     
    tags$b("And/Or"),
   
    br(), br(),
    textInput("keywords2",('Enter key words seperated by comma (,)'),value = ''),
   
    br(),    
    numericInput("num", label = h3("Num of Neighboring words"), value = 10)    
    
  ),
  
  # Main Panel:
  mainPanel( 
        tabsetPanel(type = "tabs",
                #
                tabPanel("Overview", 
                         h4(p("Why use this App")),
                         p("Suppose you have a list of keywords, say 'customer', 'brand' etc., 
                             which you want to search every instance of, in some corpus (say, a consumer insights report). 
                             Furthermore, you want some context around each keyword's occurence - say 20 odd words before and after each keyword. 
                             Now one can use Ctrl + F directly on the corpus. 
                             But if you want these context laden text chunks all neatly in one place, then this app can help.", align = "justify"),
                         
                         h4(p("How to use this App")),
                         
                         p("To use this app you'll need a few things: [1] A document corpus to search in, in .txt file format. Make sure each document is 
                           separated from another document with a new line character. [2] A keywords list again as a .txt file. Each Key Word should be separated by a new line. 
                           [3] Alternately, enter oen or more keywords separated by commas in the second input textbox (in left side bar panel).", align = "justify"),
    
                          h4(p("Test Drive the App with the example dataset")),
        
                          p(" Note: You can download the sample files from the 'Example dataset' tab and see how the app works. Also you can enter the keywords in left side bar panel. If you are 
                           entering keywords in the left side bar panel, than each key word should be separated by comma (,) 
                           without any space.", align = "justify")
                         )
                ,
    
                tabPanel("Example dataset", 
                         
                         h4(p("Download Sample text file")), 
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews text file'),
                         br(),
                         
                         h4(p("Download Sample Key Words text file")), 
                         downloadButton('downloadData2', 'Download Sample Key Words text file'),br(),
                         br(),
                         
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png")),
    
    
                tabPanel("Filtered Corpus",
                         h4(p("Download Filtered Corpus text file")), 
                         downloadButton('downloadData3', 'Download Filtered Corpus text file'),
                         br(),
                         br(),
                         verbatimTextOutput("filter_corp"))
                )
           )
       )
    )

