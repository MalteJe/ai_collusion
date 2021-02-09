library(shiny)
library(shinythemes)
library(shinyWidgets)
library(rlist)
library(tidyverse)
library(tidyquant)
library(modelr)

load("simulation_results/first_intervention.RData")
str(meta_res)


ui <- fluidPage(
  sidebarLayout(
  	sidebarPanel(
  		selectInput("runid", "Run ID", choices = map_dbl(meta_res, ~.$run_id)),
  		sliderTextInput(
  			inputId = "group_size",
  			label = "# of aggregated periods", 
  			choices = c(10, 100, 500, 1000, 5000, 10000, 50000, 100000),
  			selected = 10000,
  			grid = TRUE
  		),
  		sliderInput("T_before_intervention", "periods before intervention",
  						min = 1,
  						max = 40,
  						value = 10,
  						step = 1)
  	),
  	mainPanel(
  		plotOutput("trajectories"),
  		plotOutput("intervention")
  	)
  )
)

server <- function(input, output, session) {
  selected_run <- reactive({
  	list.filter(meta_res, run_id == input$runid)[[1]]
  })
  
  selected_outcomes <- reactive({
  	selected_run()$outcomes
  })
  	
  metrics <- reactive({
  	dimnames(selected_outcomes())[[2]]
  })
  
  output$trajectories <- renderPlot({
  	
  	#browser()
  	selected_outcomes() %>%
  		as_tibble() %>%
  		mutate(t = row_number(),
  				 t_group = (t-1) %/% input$group_size + 1) %>%
  		group_by(t_group) %>%
  		summarize_at(metrics(), mean) %>%
  		pivot_longer(cols = metrics(), names_to = "type", values_to = "value") %>%
  		ggplot(aes(x = t_group, y = value)) +
  		geom_line() +
  		facet_wrap(~type, nrow = 3, scales = "free_y") +
  		theme_tq()
  })
  
  
  intervention_t <- reactive({
  	n_rows <- nrow(selected_outcomes())
  	n_rows - (n_rows %% 1000)
  })
  
  t_before_intervention <- reactive({
  	 intervention_t() - input$T_before_intervention
  })
  
  
  output$intervention <- renderPlot({
  	selected_outcomes() %>%
  		as_tibble() %>%
  		mutate(t = row_number()) %>%
  		filter(t > t_before_intervention()) %>%
  		pivot_longer(cols = metrics(), names_to = "type", values_to = "value") %>%
  		ggplot(aes(x = t, y = value)) +
  		geom_line() +
  		geom_point() +
  		geom_vline(xintercept = intervention_t() + 0.1, color = "red") +
  		facet_wrap(~type, nrow = 3, scales = "free_y") +
  		theme_tq()
  })
  
}

shinyApp(ui, server)
