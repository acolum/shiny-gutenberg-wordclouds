### An R Shiny application to form a word cloud from any book on Project Gutenberg
### Inspired by http://shiny.rstudio.com/gallery/word-cloud.html

library(shiny)
library(dplyr)
library(readr)
library(gutenbergr)
library(shinythemes)
library(wordcloud)
library(tm)
library(memoise)
library(tidytext)

gutenberg_works()[2] %>%
  na.omit() -> titles

getTermMatrix <- memoise(function(book) {
text <- read_lines(toString(book))
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  sort(rowSums(m), decreasing = TRUE)
})
######################################## Define UI ################################################
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    navbarPage(
      "Gutenberg Word Clouds",
      tabPanel("Choose a Work", icon = icon("book"),
        sidebarLayout(
          sidebarPanel(
              selectizeInput("gutenberg_work", "Select your favorite work from this list.", titles,
                selected = "The Wonderful Wizard of Oz", 
                multiple = FALSE, options = NULL),
              sliderInput("freq", "Minimum Frequency:",
                          min = 1,  max = 100, value = 25),
              sliderInput("max", "Maximum Number of Words:",
                          min = 1,  max = 500,  value = 25)
             ),
          mainPanel(plotOutput("plot")
            )
          )
        ),
      tabPanel("References", icon = icon("file-text"), 
        h5(a("Project Gutenberg", href = "https://www.gutenberg.org/", target = "_blank")),
        h5("My session information:"), verbatimTextOutput("sessionInfo")),
      tabPanel("Contact", icon = icon("envelope"),
        h3("Any bugs/issues?"),
        h4(tags$ul(tags$li(a("Send me a message", href = "mailto:hello@alyssacolumbus.com", target = "_blank")))),
        h3("Want to connect?"),
        h4(tags$ul(tags$li("Follow me on", a("GitHub", href = "https://github.com/acolum", target = "_blank")),
        tags$li(a("Tweet Me", href = "https://twitter.com/alycolumbus", target = "_blank"))))
        )
      )
    )
######################################## Define server ##########################################
server <- function(input, output, session) {
  terms <- reactive({
    id <- gutenberg_works(title == input$gutenberg_work)[1]
    gutenberg_download(id)[2] %>%
      unnest_tokens(word, text) -> gutenberg_work
      
    isolate({withProgress({
      setProgress(message = "Loading...")
      getTermMatrix(gutenberg_work)})
    })
  })
  output$plot <- renderPlot({
    v <- terms()
    wordcloud(names(v), v, scale=c(5,0.5), random.order = F, rot.per = 0.35, use.r.layout = F,
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))    
  })
### Session Information
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
}
################################## Run the application ##########################################
shinyApp(ui = ui, server = server)
