library(shiny)
library(tidyverse)
options(scipen=999)

df <- data.frame(pais = c("Corea", "Francia"), p_equil = c(40, 37), cf = c(2000000, 1750000),
                 cv = c(30, 30), vol_mer = c(500000, 1100000), par_prev = c(200000, 250000), 
                 ct = c(8000000, 9250000), p_actual = c(55, 50))


ui <- fluidPage(
  h1("Western Connector expansion project"),
  em("Gustavo Mayeregger"),
    sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select country",
                  choices = c("Corea", "Francia"),
                  selected = "Francia"),
      sliderInput("p_proy", "Elegir precio de venta en Corea",
                  min=30, max=70,
                  value = 55),
      sliderInput("cv_proy", "Elegir costo unitario en Corea",
                  min=20, max=40,
                  value = 30)
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("equi"),
      textOutput("uti")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    x <- c(0,250000)
    corea <- data.frame(x=x, costo = input$cv_proy*x+df$cf[1], ingreso=input$p_proy*x)
    francia <- data.frame(x=x, costo = input$cv_proy*x+df$cf[2], ingreso=input$p_proy*x)
    
    
    if(input$country=="Corea") {
      ggplot() +
        geom_line(aes(corea$x, corea$costo), color="red") +
        geom_line(aes(corea$x, corea$ingreso), color="red") + 
        geom_vline(xintercept = df$par_prev[1], color="green") +
        geom_hline(yintercept = df$p_equil[1]*df$par_prev[1], color="green")
    } else if(input$country == "Francia") {
      ggplot() +
        geom_line(aes(francia$x, francia$costo), color="blue") +
        geom_line(aes(francia$x, francia$ingreso), color="blue") + 
        geom_vline(xintercept = df$par_prev[2], color="green") +
        geom_hline(yintercept = df$p_equil[2]*df$par_prev[2], color="green")
    }
  })
  
  output$equi <- renderText({
    if(input$country=="Corea") {
      x_equi_corea <- df$cf[1] / (input$p_proy - input$cv_proy)
      ing_equi_corea <- x_equi_corea*input$p_proy
      paste("Corea:", "Ingreso y costo de equilibrio = ", as.integer(ing_equi_corea))
    } else if(input$country=="Francia") {
      x_equi_francia <- df$cf[2] / (input$p_proy - input$cv_proy) 
      ing_equi_francia <- x_equi_francia*input$p_proy
      paste("Francia:", "Ingreso y costo de equilibrio = ", as.integer(ing_equi_francia))
    }
  })
  output$uti <- renderText({
    if(input$country=="Corea") {
      uti_corea <- (input$p_proy - input$cv_proy)*df$par_prev[1] - df$cf[1]
      paste("Corea:", "Utilidad esperada =", uti_corea)
    } else if(input$country == "Francia") {
      uti_francia <- (input$p_proy - input$cv_proy)*df$par_prev[2] - df$cf[2]
      paste("Francia:", "Utilidad esperada =", uti_francia)
    }
  })  
  
}

shinyApp(ui = ui, server = server)

