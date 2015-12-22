##Large list of English words to be used in the function below.
dictionary<-readLines("./dictionary.txt")

##Top "filler" words used in the English language.
filler.words<-read.table("./fillerwords.txt")

##Function to get a dataframe of word counts.
clean.word.count<-function(url){
  
  ##Changing https to http
  if(substr(url,5,5)=="s")
  {url<-sub("^....(.)","http",url)}
  
  ##Read in the text from the source.
  text <- readLines(url)  
    
  ##Collpase the text into one long character vector.
  text<-paste(text,collapse=" ")
    
  ##Get rid of numbers and punctuation. Make everything lower case.
  text<-gsub("[[:punct:]]", "", text)
  text<-gsub("[[:digit:]]", "", text)
  text<-tolower(text)
    
  ##Split large character vector into substrings.
  text<-strsplit(text,split=" ")
    
  ##Create a frequency table and make it into a dataframe. 
  ##Get rid of the empty space count.
  text.df<-as.data.frame(table(text))
  text.df<-text.df[-(which(text.df[,1]==c(""))),]
    
  ##Checking that the words are in English.
  text.df<-text.df[(text.df$text %in% dictionary),]
    
  ##Sort by count and turning text into a factor.
  text.df<-text.df[order(text.df$Freq,decreasing=TRUE),] 
  text.df$text<-factor(text.df$text, levels = text.df$text)
    
  return(text.df)
}

##Downloading some examples.
#Atlas.Shrugged.url<-"https://raw.githubusercontent.com/blueconcept/Data-Compression-using-Huffman/master/Rand%2C%20Ayn%20-%20Atlas%20Shrugged.txt"
#Atlas.Shrugged<-clean.word.count(Atlas.Shrugged.url)
##
#R.wiki.page.url<-"https://en.wikipedia.org/wiki/R_(programming_language)"
#R.wiki<-clean.word.count(R.wiki.page.url)
##
#MLK.speech.url<-"http://www.let.rug.nl/usa/documents/1951-/martin-luther-kings-i-have-a-dream-speech-august-28-1963.php"
#MLK.speech<-clean.word.count(MLK.speech.url)


library(ggplot2)
##Plotting function.
word.freq.plot<-function(text.df,no.words=5,filler.rm=F){
 
  ##Remove filler words if the user wants to. 
  if(filler.rm==TRUE)
  {text.df<-text.df[!(text.df$text %in% filler.words$V2),]}
  
  freq.plot<-ggplot(text.df[1:no.words,],aes(x=text,y=Freq)) +
    geom_bar(aes(fill=text),stat="identity") +
    ggtitle("Word Frequency Count") +
    theme(plot.title=element_text(face="bold",size=20)) +
    ylab("Frequency") +
    xlab("Word") +
    theme(axis.text.x=element_text(angle=90,hjust=0,vjust=0.25)) +
    theme(axis.text.y=element_text(vjust=-0.25)) +
    theme(legend.position="none") +
    theme(axis.text.x=element_text(size=(18-.2/3*no.words)),
          axis.text.y=element_text(size=16),
          axis.title=element_text(size=14,face="bold"))
  
  return(freq.plot)              
}

library(wordcloud)
##Word cloud plotting function.
word.cloud.funct<-function(text.df,no.words=5,filler.rm=F){

  ##Remove filler words if the user wants to.
  if(filler.rm==TRUE)
  {text.df<-text.df[!(text.df$text %in% filler.words$V2),]}
  
  word.cloud<-wordcloud(words=text.df$text,freq=text.df$Freq,
                        max.words=no.words,colors=brewer.pal(8,"Dark2"),
                        rot.per=0.35,random.order=FALSE,
                        scale=c((7-no.words/50),(2-no.words/50)))
  title("Word Cloud")
  
  return(word.cloud)          
}


library(shiny)

shinyServer(function(input, output){
  
  #appInput <- reactive({
    #switch(input$text.select,
           #"My Custom Input (above)" = clean.word.count(input$manual.input),
           #"Atlas Shrugged by Ayn Rand" = Atlas.Shrugged,
           #"R.wiki" = R.wiki,
           #"MLK I Have A Dream Speech" = MLK.speech
           #)
  
  #})
  
  output$wordplot <- renderPlot({
    word.freq.plot(clean.word.count(input$manual.input),input$no.words,filler.rm=input$filler.check)
  })   
    
  output$wordcloud <- renderPlot({
    word.cloud.funct(clean.word.count(input$manual.input),input$no.words,filler.rm=input$filler.check)
  })
  

})
#runApp(display.mode="showcase")