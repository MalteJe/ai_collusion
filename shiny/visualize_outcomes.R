library(shiny)
library(shinythemes)
library(shinyWidgets)
library(rlist)
library(tidyverse)
library(tidyquant)
library(modelr)

getwd()
# load("simulation_results/8alphas_100000.RData")


benchmarks <- tibble(price_nash = 1.472928,
                        price_monopoly = 1.924981,
                        profit_nash = 0.23,
                        profit_monopoly = 0.34) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
  separate(type, into = c("metric", "conduct"), sep = "_")


ui <- fluidPage(
  sidebarLayout(
  	sidebarPanel(width = 3,
  		selectInput("runid", "Run ID", choices = map_dbl(meta_res, ~.$run_id)),
  		sliderTextInput(
  			inputId = "group_size",
  			label = "# of aggregated periods", 
  			choices = c(10, 100, 500, 1000, 5000, 10000, 50000, 100000),
  			selected = 1000,
  			grid = TRUE
  		),
  		sliderInput("T_before_intervention", "periods before intervention",
  						min = 1,
  						max = 40,
  						value = 10,
  						step = 1),
  		prettySwitch("tiles", "Show Best Responses and equilibria", value = FALSE),
  		prettySwitch("arrows", "Show Arrows?", value = FALSE),
  		br(),
  		dataTableOutput("run_info")
  	),
  	mainPanel(width = 9,
  		plotOutput("trajectories"),
  		plotOutput("intervention"),
  		plotOutput("tiles")
  	)
  )
)

server <- function(input, output, session) {
  selected_run <- reactive({
  	list.filter(meta_res, run_id == as.numeric(input$runid))[[1]]
  })
  
  run_outcomes <- reactive({
  	selected_run()$outcomes
  })
  
  metrics <- reactive({
  	dimnames(run_outcomes())[[2]]
  })
  
  available_prices <- reactive({
    selected_run()$available_prices
  })  
  
  run_info <- reactive({
    tribble(
      ~parameter, ~value,
      "Run ID"    , selected_run()$run_id,
      "m"         , selected_run()$specs$m,
      "Alpha"     , selected_run()$specs$Alpha,
      "Beta"      , selected_run()$specs$Beta,
      "Gamma"     , selected_run()$specs$Gamma,
      "Lambda"    , selected_run()$specs$Lambda,
     #  "Polynomial Degree", selected_run()$specs$specifications$degree,
      "Tiling"    , selected_run()$specs$Tiling,
      "Number of Tilings", selected_run()$specs$specifications$n_tilings,
      "Number of Tiles", selected_run()$specs$specifications$n_tiles,
      "differential"    , selected_run()$specs$differential,
      "dutch Traces"    , selected_run()$specs$dutch_traces,
      "Convergence achieved", selected_run()$convergence$converged,
      "Convergence after", selected_run()$convergence$convergence_t) %>%
      mutate(value = ifelse(is.symbol(value), NA, value))
  })
  
  
  output$run_info <- renderDataTable(run_info(),
                                     options = list(
                                       dom = "t"
                                       # searching = FALSE,
                                       # paging = FALSE,
                                     ))
  

# Long term trajectories --------------------------------------------------

  
  output$trajectories <- renderPlot({
  	
  	run_outcomes() %>%
      as_tibble() %>%
  		mutate(t = row_number(),
  				 t_group = (t-1) %/% input$group_size + 1) %>%
  		group_by(t_group) %>%
  		summarize_at(metrics(), mean) %>%
  		pivot_longer(cols = metrics(), names_to = "type", values_to = "value") %>%
      separate(col = type, into = c("metric", "player"), sep = "_") %>%
  		ggplot() +
      geom_hline(data = benchmarks, aes(yintercept = value), size = 0.6) +
  		geom_line(aes(x = t_group, y = value, color = player, linetype = player), size = 1.2) +
  		facet_wrap(~metric, nrow = 2, scales = "free_y") +
  		theme_tq()
  })
  
  

# Intervention ------------------------------------------------------------
  
  intervention_t <- reactive({
  	n_rows <- nrow(run_outcomes())
  	n_rows - (n_rows %% 1000)
  })
  
  t_before_intervention <- reactive({
  	 intervention_t() - input$T_before_intervention
  })
  
  
  output$intervention <- renderPlot({

    run_outcomes() %>%
  		as_tibble() %>%
  		mutate(t = row_number()) %>%
  		filter(t > t_before_intervention()) %>%
  		pivot_longer(cols = metrics(), names_to = "type", values_to = "value") %>%
      separate(col = type, into = c("metric", "player"), sep = "_") %>%
  		ggplot() +
      geom_hline(data = benchmarks, aes(yintercept = value), size = 0.6) +
  		geom_line(aes(x = t, y = value, col = player, linetype = player), size = 1.2) +
  		geom_point(aes(x = t, y = value, col = player, linetype = player), size = 1) +
  		geom_vline(xintercept = intervention_t() + 0.1, color = "red") +
  		facet_wrap(~metric, nrow = 2, scales = "free_y") +
  		theme_tq()
  })
  
  
  
  
  

# reaction tiles ----------------------------------------------------------

  run_w <- reactive({
    selected_run()$w
  })
  
  feature_specs <- reactive({
    selected_run()$feature_specs
  })

  
  price_grid <- reactive({
    cross2(available_prices(), available_prices()) %>%
      map(as.numeric)
  })
  
  
  plot_wide <- reactive({
    brs <- map_dfc(.x = run_w(),
            .f = optimize_grid,
            price_grid = price_grid(),
            available_prices = available_prices(),
            feature_specs = feature_specs())
    
    brs %>%
      rename(p1_response = V1, p2_response = V2) %>%
      bind_cols(expand_grid(p2 = available_prices(), p1 = available_prices())) %>%
      mutate(equilibrium = (p1 == p1_response & p2 == p2_response))
  })
  
  plot_long <- reactive({
    plot_wide() %>%
      pivot_longer(cols = c("p1_response", "p2_response"), names_to = "response", values_to = "value")
  })
  
  
  equilibriums <- reactive({
    filter(plot_wide(), equilibrium == TRUE)
  })
  
  
  output$tiles <- renderPlot({
    if(input$tiles) {
      out <- ggplot(mapping = aes(x = p1, y = p2)) +
        geom_raster(data = plot_long(), aes(fill = value)) +
        geom_point(data = equilibriums(), size = 3, color = "red") +
        {if(input$arrows) geom_segment(data = plot_wide(), aes(xend = p1_response, yend = p2_response),
                                       arrow = arrow(length = unit(0.5, "cm")))} +
        theme_tq() +
        scale_fill_viridis(option = "D", direction = -1) +
        facet_wrap(~response) +
        theme(legend.key.width = unit(130, "points"))
    } else {
      out <- ggplot() +
        geom_blank()
    }
    return(out)
  })
  
}

shinyApp(ui, server)

