library(shiny)
library(shinythemes)

# Define UI for application that searches keywords in jop posts and plot the word_cloud
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  shinythemes::themeSelector(),
                  # Application title
                  titlePanel("Keyword IDENTIFIER"),("Made by: Luis Fernando Perez Armas"),br(),
                  
                  
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(
                          sidebarPanel(br(),
                                       
                                  h1("English & FRANÇAIS"),
                                  textInput("inputText", "ENTER THE TEXT OF THE JOB DESCRIPTION HERE | ENTREZ ICI LE TEXTE DE LA DESCRIPTION DE POSTE",value = ""),
                                  
                                  
                                  h3("Keywords | Mots-clés"),
                                  br(),
                                  tableOutput("prueba3"),
                                  br(),
                                  h3("Binary combination of Keywords | Combinaison de mots-clés Binaire"),
                                  br(),
                                  tableOutput("prueba2"),
                                  br(),
                                  h3("Triples combination of Keywords | combinaisons de mots-clés Triples "),
                                  br(),
                                  tableOutput("prueba4")
                                  
                          ),
                          
                          
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  h3("Keywords Cloud | Toile de Mots"),
                                  plotOutput("wordcloud1"),
                                  br(),
                                  br(),
                                  br(),
                                  
                                  h3("Binary Keywords combination Cloud | Toile de Mots Combinaison Binaire "),
                                  plotOutput("wordcloud2"),
                                  br(),
                                  
                                  em(h4("Original Job Description Text | Texte original de la description de travail")),
                                  em(textOutput("original"))
                                  )
                                  
                                  
                          )
                  )
)

