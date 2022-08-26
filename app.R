library("shiny")
library("shinythemes")
library("tidyverse")
library("corrplot")
library("readr")
library("ggthemes")
library("DT")
library("markdown")

# install.packages("markdown")

bank_cleaned <- read_csv("bank_cleaned.csv")
 # view(bank_cleaned)
# unique(bank_cleaned$age)

ui <-  fluidPage(theme = shinytheme("sandstone"),
                 navbarPage(
                   "Market Analysis -- Be Happy",
                   tabPanel(
                     "Data Visualization",
                     
                     sidebarLayout(position = "right",
                                   sidebarPanel(
                                     h1("About the Dataset"),
                                     tags$small(
                                       "The information relates to direct marketing activities run by a Portuguese bank. ",
                                       "On phone conversations, the marketing campaigns were based. ",
                                       " In order to determine if the product (bank term deposit) would be subscribed ('yes') or not ('no'), it was frequently necessary to make more than one contact with the same client."
                                     ),
                                     
                                     selectInput(
                                       width = "100%",
                                       inputId = "Graph",
                                       label = "Select the Marketing report",
                                       choices = c(
                                         "Employees with suitable experience and training",
                                         "Departments with seasoned personnel",
                                         "Senior staff in the investment industry",
                                         "Marital status of employees",
                                         "Employees who took out loans"
                                       )
                                     ),
                                     
                                     sliderInput(
                                       inputId = "bins",
                                       label = "Choose the age range: ",
                                       min = 0,
                                       max = 100,
                                       value = 80,
                                       step = 2
                                     ),
                                     plotOutput("job")),#sidebar panel
                       
                       mainPanel(
                         plotOutput("plot"),
                         
                                 )
                       
                     )#sidebar layout
                     
                   ),#Tab Panel "Data Viz" Closed
                   navbarMenu(
                     "Datasets and More",
                     tabPanel("Dataset",
                              DT::dataTableOutput("table"),
                              fluidRow(
                                includeMarkdown("about2.md")
                              )),
                     tabPanel(
                       "About",
                       fluidRow(
                         column(10,
                                includeMarkdown("about.md")
                                
                                )
                                
                                )
                           
                         
                       )
                     )
                            )
                 )

server <- function(input, output, session) {

  output$table <- renderDataTable(
    DT::datatable(bank_cleaned)
  )
  
  output$plot <- renderPlot({
      if (input$Graph == "Senior staff in the investment industry"){
        x    <- bank_cleaned$age
        b <- seq(min(x), max(x), length.out = input$bins + 1)
        Gender = input$gender
        ggplot(bank_cleaned, aes(bank_cleaned$age)) + scale_fill_brewer(palette = "spectral") + geom_histogram(
          aes(
            fill = bank_cleaned$job,
            binwidth = .01,
            col = "blue",
            
          ),
          breaks = b,
          border = "white",
          xlab = "AGE of the Employees",
          ylab = "Poluation",
          main = "Number of employees in different sectors"
        )
      }else if (input$Graph == "Departments with seasoned personnel") {
          ggplot(bank_cleaned, aes(x= bank_cleaned$job, y= bank_cleaned$age)) + geom_point(aes(col= bank_cleaned$job, size=3))+
          geom_smooth(method = "lm", color = "firebrick")  +
          ggtitle("Job Department vs AGE") +
          xlab("") + ylab("Age") +
          theme(legend.position = "None", axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            vjust = 0.5),
            element_blank(),
            axis.ticks.x = element_blank())
      } else if (input$Graph == "Employees with suitable experience and training"){
        ggplot(bank_cleaned, aes(x = bank_cleaned$age, y = bank_cleaned$duration)) +
          geom_point(aes(col = bank_cleaned$education), size = 3) +
          geom_smooth(method = "lm", color = "firebrick") +
          xlab("AGE") + ylab("Durartion of the Designation") +
          scale_colour_brewer(palette = "Set1")+ labs(col= "Education" )
      } else if(input$Graph == "Marital status of employees"){

        ggplot(bank_cleaned, aes(bank_cleaned$marital, bank_cleaned$age))+
        geom_boxplot(aes(fill=factor(bank_cleaned$marital))) +
          theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
          labs(title="Marital status of employees working in Investment sector",
               caption="Source: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing ",
               x="  ",
               y="  "
               )
      
        }else if(input$Graph == "Employees who took out loans"){

          ggplot(bank_cleaned, aes(x = bank_cleaned$job, y = bank_cleaned$loan, fill = bank_cleaned$job)) +
            geom_bar(stat = "identity", width = .6)  +
           labs(x="", y="",title="Employees who took loan in the investment industry")+ theme_tufte() +
            theme(plot.title = element_text(hjust = .5),axis.ticks = element_blank()) +scale_fill_brewer(palette = "Dark2")
      
        }
  })
  
  output$job <- renderPlot({
    ggplot(bank_cleaned) +
      geom_density(aes(bank_cleaned$job, color = bank_cleaned$education), kernel = "gaussian") +
      ggtitle("Overall Market Analysis", subtitle = "From Bank Marketing Data Set" ) +
      xlab("Job titles") + ylab("Expertise") +
      theme(
        legend.position = "None",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      scale_colour_brewer(palette = "Set1")
    })
  


  

  
}

shinyApp(ui, server)

  

  output$DataTable <- renderDataTable({
    DT::datatable(bank_cleaned)
  })
  

  
}

shinyApp(ui, server)
