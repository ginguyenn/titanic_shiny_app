library(shiny)
library(shinythemes)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"),  # theme apply
  
  # Application title
  titlePanel("Titanic Predictation"),
  
  #Navbar
  navbarPage(" ",
             tabPanel("Sex",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("sex_plot", "Sex",
                                       choices= c("Female","Male", "Both"),
                                       selected = "Both")
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
             ),
             tabPanel("Passenger Class",plotOutput("pclass_plot")),
             tabPanel("Age", plotOutput("age_plot")),
             tabPanel("Embarked Port", plotOutput("port_plot")),
             tabPanel("Fare", plotOutput("fare_plot")),
             tabPanel("Family Size", plotOutput("famsize_plot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$plot <- renderPlot({

    if(input$sex_plot == "Female"){
      ggplot(female_data, aes(x= Sex, fill = Survived))+
        theme_bw()+
        geom_bar()+
        labs(y = "Female Passenger Count",
             title = "Total Survival Rates by Female"
        )
    }
    else if(input$sex_plot == "Male"){
      ggplot(male_data, aes(x= Sex, fill = Survived))+
        theme_bw()+
        geom_bar()+
        labs(y = "Male Passenger Count",
             title = "Total Survival Rates by Male")
    }
    else{
      ggplot(titanic, aes(x = Sex, fill = Survived))+
        theme_bw()+
        geom_bar()+
        labs(y = "Passenger Count",
             title = "Total Survival Rates by Sex")
    }
  })
  # Render the Pclass plot when "Passenger Class" tab is selected
  output$pclass_plot <- renderPlot({
    #Age distribution
    ggplot(titanic, aes( x= Pclass, fill= Survived))+
      theme_bw()+
      geom_bar()+
      labs(x ="Passenger Classes",
           y ="Passenger Count", 
           title = "Survival Rate by Cabin Class")+
      scale_fill_manual(values = c("0"= "lightblue", "1"= "orange2"))
  })
  #Render the Age plot when "Age" tab is selected
  output$age_plot <- renderPlot({
    ggplot(titanic_age, aes(x = age_group,fill = titanic$Survived))+
      theme_dark()+
      geom_bar()+
      labs(y ="Passenger Count",
           x="Age Group",
           title = "Survival Rate by Age Group")+
      scale_fill_manual(values = c("0"="deeppink3", "1"="cyan2"))
    
  })
  #Render the Port plot when "Embarked Port" tab is selected
  output$port_plot <- renderPlot({
    ggplot(titanic, aes(x = Embarked, fill = Survived))+
      theme_bw()+
      geom_bar()+
      labs(x="Embarked ",
           y ="Passenger Count",
           title = "Survival Rate by Embarked Port")+
      scale_fill_manual(values = c("0"="firebrick", "1" = "palegreen"))
  })
  #
  output$fare_plot <- renderPlot({
    
    
    # Vẽ biểu đồ
    ## Tính mean, median và std của Fare
    fare_mean <- mean(titanic$Fare, na.rm = TRUE) #giá trị na bị loại bỏ
    fare_std <- sd(titanic$Fare, na.rm = TRUE)
    fare_med <- median(titanic$Fare, na.rm = TRUE)
    #Graph
    ggplot(titanic, aes(x= Fare))+
      geom_density(fill="tan")+
      theme_bw()+
      labs(x = "",
           title = "Fare Distribution")
      
  })
  #
  output$famsize_plot <- renderPlot({
    ggplot(titanic, aes(x = titanic$fam_size, fill = Survived))+
      theme_bw()+
      geom_bar()+
      labs(x = "Family Size",
           y =" Passenger Count",
           title = "Survival Rate by Family Size")+
      scale_fill_manual(values = c("0"= "tomato1", "1"= "royalblue2"))
  })
}

shinyApp(ui = ui, server = server)
