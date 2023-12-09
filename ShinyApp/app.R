#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(treemapify)
library(dplyr)

df=readxl::read_excel("C:\\Users\\dell\\Documents\\rrr\\Book1.xlsx")
df$Drought=ifelse(is.na(df$Drought),ceiling(mean(df$Drought,na.rm = TRUE)),df$Drought)
df$Earthquake=ifelse(is.na(df$Earthquake),ceiling(mean(df$Earthquake,na.rm = TRUE)),df$Earthquake)
df$`Extreme temperature`=ifelse(is.na(df$`Extreme temperature`),ceiling(mean(df$`Extreme temperature`,na.rm = TRUE)),df$`Extreme temperature`)
df$`Extreme weather`=ifelse(is.na(df$`Extreme weather`),ceiling(mean(df$`Extreme weather`,na.rm = TRUE)),df$`Extreme weather`)
df$Flood=ifelse(is.na(df$Flood),ceiling(mean(df$Flood,na.rm = TRUE)),df$Flood)
df$Landslide=ifelse(is.na(df$Landslide),ceiling(mean(df$Landslide,na.rm = TRUE)),df$Landslide)
df$`Mass movement (dry)`=ifelse(is.na(df$`Mass movement (dry)`),ceiling(mean(df$`Mass movement (dry)`,na.rm = TRUE)),df$`Mass movement (dry)`)
df$`Volcanic activity`=ifelse(is.na(df$`Volcanic activity`),ceiling(mean(df$`Volcanic activity`,na.rm = TRUE)),df$`Volcanic activity`)
df$Wildfire=ifelse(is.na(df$Wildfire),ceiling(mean(df$Wildfire,na.rm = TRUE)),df$Wildfire)

ui <- fluidPage(

    navbarPage( "Disaster predictor",
                tabPanel("Data pattern",
                         sidebarLayout(
                           sidebarPanel(
                             radioButtons("rad", h3("Select chart:"),
                                         c("Bar chart","Pie chart","Tree map"),selected = "Bar chart"),
                             sliderInput("year", h3("Year:"),
                                         min = 1900, max = 2018, value = 2000),
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotOutput("Plot"),
                             
                           ))),
                tabPanel("Predict disasters",
    sidebarLayout(
        sidebarPanel(
          selectInput("var", h3("Select natural disaster:"),
                      c("All natural disasters","Drought","Earthquake","Extreme temperature",
                        "Extreme weather","Flood","Landslide","Mass movement(dry)","Volcanic activity"
                        ,"Wildfire"),selected = "All natural disasters"),
          numericInput("txt1", h3("Year:"), "2020"),
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           h3(textOutput("head")),
           h3(textOutput("pred")),
           
        )
    )
)))

server <- function(input, output) {
  
  output$Plot=renderPlot({
    
    
    df3=df %>% filter(Year==input$year)
    df4=as.data.frame(t(df3))
    df5=as.data.frame(df4[-c(1,2),])
    df5$Disaster=c("Drought","Earthquake","Extreme temperature",
                   "Extreme weather","Flood","Landslide","Mass movement(dry)","Volcanic activity"
                   ,"Wildfire")
    colnames(df5)[1]=c("Value")
    
    if(input$rad=="Pie chart")
      ggplot(df5, aes(x="", y=Value, fill=Disaster))+geom_bar(stat="identity", width=1, color="white")+coord_polar("y", start=0)+theme_void()
    else if(input$rad=="Tree map")
      ggplot(df5,aes(area=Value,fill=Disaster,label=Disaster))+geom_treemap(layout="squarified")+ 
      geom_treemap_text(place = "centre",size = 17)
    else
      ggplot(df5,aes(y=Disaster,x=Value,fill=Disaster))+geom_bar(stat="identity")+theme_minimal()
    
  })

    output$distPlot <- renderPlot({
      
      y_value=switch(input$var,"All natural disasters"=df$`All natural disasters`,
                     "Drought"=df$Drought,"Earthquake"=df$Earthquake,
                     "Extreme temperature"=df$`Extreme temperature`,
                     "Extreme weather"=df$`Extreme weather`,"Flood"=df$Flood,
                     "Landslide"=df$Landslide,"Mass movement(dry)"=df$`Mass movement (dry)`,
                     "Volcanic activity"=df$`Volcanic activity`,"Wildfire"=df$Wildfire)
      y_lab=switch(input$var,"All natural disasters"="All natural disasters",
               "Drought"="Drought","Earthquake"="Earthquake",
               "Extreme temperature"="Extreme temperature",
               "Extreme weather"="Extreme weather","Flood"="Flood",
               "Landslide"="Landslide","Mass movement(dry)"="Mass movement (dry)",
               "Volcanic activity"="Volcanic activity","Wildfire"="Wildfire")
      
      ggplot(df,aes(x=Year,y=y_value))+geom_point()+labs(x="Year",y=y_lab)+geom_smooth()+geom_smooth(method="lm")+theme_minimal()
    })
    
    output$head=renderText({
      paste(input$var," in ",input$txt1," :")
    })
    output$pred=renderPrint({
      y_value=switch(input$var,"All natural disasters"=df$`All natural disasters`,
                     "Drought"=df$Drought,"Earthquake"=df$Earthquake,
                     "Extreme temperature"=df$`Extreme temperature`,
                     "Extreme weather"=df$`Extreme weather`,"Flood"=df$Flood,
                     "Landslide"=df$Landslide,"Mass movement(dry)"=df$`Mass movement (dry)`,
                     "Volcanic activity"=df$`Volcanic activity`,"Wildfire"=df$Wildfire)
      
      model=lm(y_value~Year,data=df)
      p=data.frame(Year=c(input$txt1))
      ans=predict(model,p)
      print(ceiling(ans))
    })
}
library(writexl)
write_xlsx(df,"C:\\Users\\dell\\Documents\\new.xlsx")
# Run the application 
shinyApp(ui = ui, server = server)
