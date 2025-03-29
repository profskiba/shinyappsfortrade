library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Effects of Import Tariff for a Small Open Economy"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("demand_slope", 
                  "Demand Curve Slope", 
                  min = -4,  # Updated bound
                  max = -0.8,  # Upper bound unchanged
                  value = -0.8,  # Starting value
                  step = 0.1),
      sliderInput("supply_slope", 
                  "Supply Curve Slope", 
                  min = 0.8,  # Lower bound unchanged
                  max = 4,  # Updated bound
                  value = 0.8,  # Starting value
                  step = 0.1),
      sliderInput("tariff", 
                  "Import Tariff", 
                  min = 0, 
                  max = 4,  # Tariff range unchanged
                  value = 0, 
                  step = 0.1)
    ),
    mainPanel(
      width = 9,
      plotOutput("supply_demand_plot"),
      textOutput("tariff_summary"),
      textOutput("surplus_summary"),
      textOutput("dwl_summary")
    )
  )
)

server <- function(input, output) {
  trade_data <- reactive({
    # Pivot points for demand and supply curves
    pivot_demand <- c(12, 2)  # Demand curve pivots around (12, 2)
    pivot_supply <- c(2, 2)   # Supply curve pivots around (2, 2)
    
    world_price <- 2  # Fixed world price
    tariff <- input$tariff  # Tariff value
    
    demand_slope <- input$demand_slope
    supply_slope <- input$supply_slope
    
    # Demand intercept: demand passes through pivot point (12, 2)
    demand_intercept <- pivot_demand[2] - demand_slope * pivot_demand[1]
    
    # Supply intercept: supply passes through pivot point (2, 2)
    supply_intercept <- pivot_supply[2] - supply_slope * pivot_supply[1]
    
    # Price with tariff
    tariff_price <- world_price + tariff
    
    # Quantities at world price and tariff price
    demand_qty_wp <- (world_price - demand_intercept) / demand_slope
    supply_qty_wp <- (world_price - supply_intercept) / supply_slope
    demand_qty_tp <- (tariff_price - demand_intercept) / demand_slope
    supply_qty_tp <- (tariff_price - supply_intercept) / supply_slope
    
    # Imports at world price and tariff price
    imports_wp <- demand_qty_wp - supply_qty_wp
    imports_tp <- demand_qty_tp - supply_qty_tp
    
    # Consumer surplus loss (trapezoid under demand curve)
    consumer_surplus_loss <- 0.5 * (demand_qty_wp + demand_qty_tp) * (tariff)
    
    # Producer surplus gain (trapezoid under supply curve)
    producer_surplus_gain <- 0.5 * (supply_qty_wp + supply_qty_tp) * (tariff)
    
    # Tariff revenue
    tariff_revenue <- imports_tp * tariff
    
    # Dead Weight Loss
    producer_dwl <- 0.5 * (supply_qty_tp - supply_qty_wp) * tariff
    consumer_dwl <- 0.5 * (demand_qty_wp - demand_qty_tp) * tariff
    
    list(world_price = world_price, tariff_price = tariff_price,
         demand_qty_wp = demand_qty_wp, supply_qty_wp = supply_qty_wp,
         demand_qty_tp = demand_qty_tp, supply_qty_tp = supply_qty_tp,
         imports_wp = imports_wp, imports_tp = imports_tp,
         consumer_surplus_loss = consumer_surplus_loss,
         producer_surplus_gain = producer_surplus_gain,
         tariff_revenue = tariff_revenue,
         producer_dwl = producer_dwl,
         consumer_dwl = consumer_dwl)
  })
  
  output$supply_demand_plot <- renderPlot({
    data <- trade_data()
    
    x <- seq(0, 16, length.out = 100)  # Updated horizontal range to 0-16
    demand_slope <- input$demand_slope
    supply_slope <- input$supply_slope
    demand_intercept <- 2 - demand_slope * 12
    supply_intercept <- 2 - supply_slope * 2
    demand_y <- demand_intercept + demand_slope * x
    supply_y <- supply_intercept + supply_slope * x
    
    ggplot() +
      geom_line(aes(x = x, y = demand_y), color = "navy", linewidth = 2, na.rm = TRUE) +
      geom_line(aes(x = x, y = supply_y), color = "maroon", linewidth = 2, na.rm = TRUE) +
      geom_hline(yintercept = data$tariff_price, color = "orangered", linetype = "solid", linewidth = 1) +
      geom_hline(yintercept = data$world_price, color = "purple", linetype = "solid", linewidth = 1) +
      geom_vline(xintercept = data$demand_qty_wp, color = "navy", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = data$supply_qty_wp, color = "maroon", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = data$demand_qty_tp, color = "navy", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = data$supply_qty_tp, color = "maroon", linetype = "dashed", linewidth = 1) +
      geom_point(aes(x = data$demand_qty_tp, y = data$tariff_price), color = "orangered", size = 4) +
      geom_point(aes(x = data$supply_qty_tp, y = data$tariff_price), color = "orangered", size = 4) +
      geom_point(aes(x = data$demand_qty_wp, y = data$world_price), color = "purple", size = 4) +
      geom_point(aes(x = data$supply_qty_wp, y = data$world_price), color = "purple", size = 4) +
      labs(title = "Import Tariff: Supply and Demand Curve",
           x = "Quantity", 
           y = "Price") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = rel(1.5)),
        legend.position = "none",  # Remove legend
        panel.grid.major = element_line(color = "lightgray", linetype = "solid", linewidth = 0.5)
      ) +
      scale_x_continuous(breaks = seq(0, 16, by = 2), limits = c(0, 16)) +  # Updated to 0-16
      scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10))  # Y-axis unchanged
  })
  
  output$tariff_summary <- renderText({
    data <- trade_data()
    paste("Imports at World Price:", round(data$imports_wp, 2), 
          "| Imports at Tariff Price:", round(data$imports_tp, 2))
  })
  
  output$surplus_summary <- renderText({
    data <- trade_data()
    paste("Consumer Surplus Loss:", round(data$consumer_surplus_loss, 2), 
          "| Producer Surplus Gain:", round(data$producer_surplus_gain, 2), 
          "| Tariff Revenue:", round(data$tariff_revenue, 2))
  })
  
  output$dwl_summary <- renderText({
    data <- trade_data()
    paste("Producer DWL:", round(data$producer_dwl, 2), 
          "| Consumer DWL:", round(data$consumer_dwl, 2))
  })
}

shinyApp(ui, server)
