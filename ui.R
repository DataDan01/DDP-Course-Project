shinyUI(
  pageWithSidebar(
    
    ##Title
    headerPanel("Wordcount Application"),
    
    sidebarPanel(
    
    textInput("manual.input",label=h4("Paste your own (full) link below!"),
                value="http://textfiles.com/directory.html"),  
      
    #selectInput("text.select", "Or choose a text:", 
                  #choices = c("My Custom Input (above)", "R.wiki",
                              #"MLK I Have A Dream Speech",
                              #"Atlas Shrugged by Ayn Rand")),
    
    sliderInput("no.words", "Number of words to plot:", 
                min=5, max=50, value=25),
    
    checkboxInput("filler.check", label="Remove filler words?", value = FALSE)
    
    ),
  
    mainPanel(
      plotOutput("wordplot"),
      plotOutput("wordcloud")
      
        
    )
    
    
    )
)