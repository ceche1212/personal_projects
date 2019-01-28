#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(DescTools)
library(shinythemes)

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("slate"),
                shinythemes::themeSelector(),
        
        # App title ----
        titlePanel("Area Calculation Perfilometry"),
        
        h4("Made By: Luis F. Perez Armas & Stephania Kossman"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        h3("Please upload the CSV Y-X data file"),
                        # Input: Select a file ----
                        fileInput("file1", "Choose CSV File",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),
                        
                        # Input: Select separator ----
                        radioButtons("sep", "Separator",
                                     choices = c(Comma = ",",
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = ","),
                        
                        # Input: Select quotes ----
                        radioButtons("quote", "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = '"'),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        
                        
                        
                        h3("Plane" ),
                        
                        sliderInput("Plane", "Enter cutting plane", min = -0.1, max=0.1,
                                    value=0, step=0.002),
                        
                        h3("Smoothing Parameter DF" ),
                        
                        sliderInput("degreesF", "Enter smoothing parameter", min = 1, max=150,
                                    value=20, step=1),
                        
                        
                        
                        h1("Area"),
                        
                        h2(textOutput("Area_calc")),
                        
                        h3("NewData"),
                        
                        # Input: Select number of rows to display ----
                        radioButtons("disp", "Click (All) to display all values of New Data",
                                     choices = c(Header = "header",
                                                 All = "all"),
                                     selected = "header"),
                        
                        h4(tableOutput("newdata"))
                        
                        
                        
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        # Output: Data file ----
                        
                        h3("Original Data"),
                        
                        plotOutput("data_original"),
                        
                        h3("Smooth Data"),
                        
                        plotOutput("spline"),
                        
                        h3("Final Calculations Area plot"),
                        
                        plotOutput("Area_plot")
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                )
                
        )
)

#funciones externas





# Define server logic to read selected file ----
server <- function(input, output) {
        
        output$contents <- renderTable({
                
                # input$file1 will be NULL initially. After the user selects
                # and uploads a file, head of that data file by default,
                # or all rows if selected, will be shown.
                
                req(input$file1)
                
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,skip = 2)
                
                names(df)<-c("x","y")
                
                if(input$disp == "header") {
                        return(head(df))
                }
                else {
                        return(df)
                }
                
        })
        
        
        output$data_original<-renderPlot({
                
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,skip = 2)
                
                names(df)<-c("x","y")
                
                return(plot(df$y~df$x,col="red",type="l",
                            main="Original Data",xlab="X",ylab = "Y"))
                
                
        })
        
        output$spline<-renderPlot({
                
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,skip = 2)
                
                names(df)<-c("x","y")
                
                tabla_spline<-smooth.spline(x = df$x, y = df$y, df = input$degreesF)
                
                
                temp_data<-cbind(tabla_spline$x,tabla_spline$y)
                temp_data<-as.data.frame(temp_data)
                names(temp_data)<-c("x","y")
                
                
                return(plot(temp_data$y~temp_data$x,type="o",col="black",lwd=2,main="Spline",xlab="X",ylab = "Y"))
                
                
                
        })
        
        
        output$Area_calc<-renderText({
                
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,skip = 2)
                
                names(df)<-c("x","y")
                
                tabla_spline<-smooth.spline(x = df$x, y = df$y, df = input$degreesF)
                
                
                temp_data<-cbind(tabla_spline$x,tabla_spline$y)
                temp_data<-as.data.frame(temp_data)
                names(temp_data)<-c("x","y")
                
                minimo_valle_y<-min(temp_data$y)
                which(temp_data$y==minimo_valle_y)
                
                mitad1<-temp_data[1:which(temp_data$y==minimo_valle_y),]
                mitad2<-temp_data[(which(temp_data$y==minimo_valle_y)+1):nrow(temp_data),]
                
                plano<-input$Plane
                
                
                mitad1<-filter(mitad1,mitad1$y<=plano)
                
                x=mitad1$x
                
                distancia_x<-data.frame()
                distancia<-c(NA)
                
                for (i in seq(x)) {
                        
                        distancia[i]<-x[i+1]-x[i]
                        distancia_x<-rbind(distancia_x,distancia[i])
                        
                }
                
                names(distancia_x)<-c("distancia_x")
                
                mitad1$distancia_x<-distancia_x[[1]]
                
                
                mitad1<-filter(mitad1,mitad1$distancia_x<=median(mitad1$distancia_x,na.rm = T),mitad1$x>=mean(mitad1$x))
                
                mitad2<-filter(mitad2,mitad2$y<=plano)
                
                # calcular distancia entre puntos
                
                x2=mitad2$x
                
                distancia_x2<-data.frame()
                distancia2<-c(NA)
                
                for (i in seq(x2)) {
                        
                        distancia2[i]<-x2[i+1]-x2[i]
                        distancia_x2<-rbind(distancia_x2,distancia2[i])
                        
                }
                
                names(distancia_x2)<-c("distancia_x")
                
                mitad2$distancia_x<-distancia_x2[[1]]
                
                
                mitad2<-filter(mitad2,mitad2$distancia_x<=median(mitad2$distancia_x,na.rm = T),mitad2$x<mean(mitad2$x))
                
                newdata<-rbind(mitad1,mitad2)
                
                
                
                newdata$y<-newdata$y*(-1)
                
                
                Spline<-AUC(x=newdata$x,y=newdata$y,method = "trapezoid")
                
                
                return(Spline)
                

        })
        
        
        output$newdata<-renderTable({
                
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,skip = 2)
                
                names(df)<-c("x","y")
                
                tabla_spline<-smooth.spline(x = df$x, y = df$y, df = input$degreesF)
                
                
                temp_data<-cbind(tabla_spline$x,tabla_spline$y)
                temp_data<-as.data.frame(temp_data)
                names(temp_data)<-c("x","y")
                
                minimo_valle_y<-min(temp_data$y)
                which(temp_data$y==minimo_valle_y)
                
                mitad1<-temp_data[1:which(temp_data$y==minimo_valle_y),]
                mitad2<-temp_data[(which(temp_data$y==minimo_valle_y)+1):nrow(temp_data),]
                
                plano<-input$Plane
                
                
                mitad1<-filter(mitad1,mitad1$y<=plano)
                
                x=mitad1$x
                
                distancia_x<-data.frame()
                distancia<-c(NA)
                
                for (i in seq(x)) {
                        
                        distancia[i]<-x[i+1]-x[i]
                        distancia_x<-rbind(distancia_x,distancia[i])
                        
                }
                
                names(distancia_x)<-c("distancia_x")
                
                mitad1$distancia_x<-distancia_x[[1]]
                
                
                mitad1<-filter(mitad1,mitad1$distancia_x<=median(mitad1$distancia_x,na.rm = T),mitad1$x>=mean(mitad1$x))
                
                mitad2<-filter(mitad2,mitad2$y<=plano)
                
                # calcular distancia entre puntos
                
                x2=mitad2$x
                
                distancia_x2<-data.frame()
                distancia2<-c(NA)
                
                for (i in seq(x2)) {
                        
                        distancia2[i]<-x2[i+1]-x2[i]
                        distancia_x2<-rbind(distancia_x2,distancia2[i])
                        
                }
                
                names(distancia_x2)<-c("distancia_x")
                
                mitad2$distancia_x<-distancia_x2[[1]]
                
                
                mitad2<-filter(mitad2,mitad2$distancia_x<=median(mitad2$distancia_x,na.rm = T),mitad2$x<mean(mitad2$x))
                
                newdata<-rbind(mitad1,mitad2)
                
                
                
                newdata$y<-newdata$y*(-1)
                
                newdata<-select(newdata,x,y)
                
                if(input$disp == "header") {
                        return(head(newdata))
                }
                else {
                        return(newdata)
                }
        
                
                
                
                
        })
        
        
        
        output$Area_plot<-renderPlot({
                
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote,skip = 2)
                
                names(df)<-c("x","y")
                
                tabla_spline<-smooth.spline(x = df$x, y = df$y, df = input$degreesF)
                
                
                temp_data<-cbind(tabla_spline$x,tabla_spline$y)
                temp_data<-as.data.frame(temp_data)
                names(temp_data)<-c("x","y")
                
                minimo_valle_y<-min(temp_data$y)
                which(temp_data$y==minimo_valle_y)
                
                mitad1<-temp_data[1:which(temp_data$y==minimo_valle_y),]
                mitad2<-temp_data[(which(temp_data$y==minimo_valle_y)+1):nrow(temp_data),]
                
                plano<-input$Plane
                
                
                mitad1<-filter(mitad1,mitad1$y<=plano)
                
                x=mitad1$x
                
                distancia_x<-data.frame()
                distancia<-c(NA)
                
                for (i in seq(x)) {
                        
                        distancia[i]<-x[i+1]-x[i]
                        distancia_x<-rbind(distancia_x,distancia[i])
                        
                }
                
                names(distancia_x)<-c("distancia_x")
                
                mitad1$distancia_x<-distancia_x[[1]]
                
                
                mitad1<-filter(mitad1,mitad1$distancia_x<=median(mitad1$distancia_x,na.rm = T),mitad1$x>mean(mitad1$x))
                
                mitad2<-filter(mitad2,mitad2$y<=plano)
                
                # calcular distancia entre puntos
                
                x2=mitad2$x
                
                distancia_x2<-data.frame()
                distancia2<-c(NA)
                
                for (i in seq(x2)) {
                        
                        distancia2[i]<-x2[i+1]-x2[i]
                        distancia_x2<-rbind(distancia_x2,distancia2[i])
                        
                }
                
                names(distancia_x2)<-c("distancia_x")
                
                mitad2$distancia_x<-distancia_x2[[1]]
                
                
                mitad2<-filter(mitad2,mitad2$distancia_x<=median(mitad2$distancia_x,na.rm = T),mitad2$x<mean(mitad2$x))
                
                newdata<-rbind(mitad1,mitad2)
                
                
                
                newdata$y<-newdata$y*(-1)
                
                
                return(plot(newdata$y~newdata$x,type="o",col="blue",lwd=2,main="Final curve for area calculation",xlab="X",ylab = "Y"))
                
        })
        
        
        output$downloadData <- downloadHandler(
                filename = "newdata.csv",
                content = function(file) {
                        write.csv(DESCARGAR(), file)
                }
        )
       
        
}
# Run the app ----
shinyApp(ui, server)