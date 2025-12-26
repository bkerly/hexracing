library(shiny)
library(ggplot2)
library(dplyr)

# Define car types
cars <- list(
    rally = list(
        name = "Rally Car",
        icon = "🏎️",
        costs = list(road = 1, grass = 2, mud = 3, mountain = 5),
        color = "#ef4444"
    ),
    suv = list(
        name = "SUV",
        icon = "🚙",
        costs = list(road = 2, grass = 2, mud = 2, mountain = 3),
        color = "#3b82f6"
    ),
    truck = list(
        name = "Monster Truck",
        icon = "🚚",
        costs = list(road = 3, grass = 1, mud = 1, mountain = 2),
        color = "#22c55e"
    )
)

# Terrain colors
terrain_colors <- c(
    road = "#374151",
    grass = "#22c55e",
    mud = "#92400e",
    mountain = "#78716c"
)

# Function to generate hex coordinates
hex_to_pixel <- function(q, r, size = 1) {
    x <- size * (3/2 * q)
    y <- size * (sqrt(3)/2 * q + sqrt(3) * r)
    data.frame(x = x, y = y)
}

# Function to convert pixel coordinates back to hex
pixel_to_hex <- function(x, y, size = 1) {
    q <- (2/3 * x) / size
    r <- (-1/3 * x + sqrt(3)/3 * y) / size
    
    # Round to nearest hex using cube coordinates
    cube_round(q, r)
}

# Cube coordinate rounding
cube_round <- function(q, r) {
    x <- q
    z <- r
    y <- -x - z
    
    rx <- round(x)
    ry <- round(y)
    rz <- round(z)
    
    x_diff <- abs(rx - x)
    y_diff <- abs(ry - y)
    z_diff <- abs(rz - z)
    
    if (x_diff > y_diff && x_diff > z_diff) {
        rx <- -ry - rz
    } else if (y_diff > z_diff) {
        ry <- -rx - rz
    } else {
        rz <- -rx - ry
    }
    
    c(rx, rz)
}

# Function to create hexagon polygon
create_hex_polygon <- function(x, y, size = 1) {
    angles <- seq(0, 5) * pi / 3
    data.frame(
        x = x + size * cos(angles),
        y = y + size * sin(angles)
    )
}

# Function to get hex neighbors
get_neighbors <- function(q, r) {
    data.frame(
        q = c(q+1, q-1, q, q, q+1, q-1),
        r = c(r, r, r+1, r-1, r-1, r+1)
    )
}

# Function to generate level
generate_level <- function(level) {
    size <- min(8 + level, 15)
    
    grid <- expand.grid(q = 0:(size-1), r = 0:(size-1))
    grid$terrain <- sample(
        c("road", "grass", "mud", "mountain"),
        nrow(grid),
        replace = TRUE,
        prob = c(0.3, 0.3, 0.25, 0.15)
    )
    
    # Set start and finish
    start_r <- floor(size / 2)
    finish_r <- floor(size / 2)
    
    grid$terrain[grid$q == 0 & grid$r == start_r] <- "road"
    grid$terrain[grid$q == (size-1) & grid$r == finish_r] <- "road"
    
    list(
        grid = grid,
        start = c(0, start_r),
        finish = c(size-1, finish_r),
        size = size
    )
}

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      body { background-color: #0f172a; color: white; }
      .well { background-color: #1e293b; border: none; }
      .btn-car { 
        background-color: #1e293b; 
        color: white; 
        border: 2px solid #334155;
        margin: 5px;
        padding: 15px;
        width: 200px;
      }
      .btn-car:hover { 
        background-color: #334155; 
        border-color: #3b82f6;
      }
      .game-stats {
        background-color: #1e293b;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 10px;
      }
      .btn-action {
        background-color: #3b82f6;
        color: white;
        border: none;
        margin: 5px;
      }
      .btn-action:hover {
        background-color: #2563eb;
      }
      .victory-box, .defeat-box {
        background-color: #1e293b;
        padding: 30px;
        border-radius: 8px;
        text-align: center;
        margin-top: 50px;
      }
      .info-box {
        background-color: #1e293b;
        padding: 10px;
        border-radius: 8px;
        margin-bottom: 10px;
      }
    "))
    ),
    
    titlePanel(
        div(style = "text-align: center; margin-bottom: 30px;",
            h1("🏁 Hex Racer", style = "color: white; font-weight: bold;")
        )
    ),
    
    uiOutput("game_ui")
)

server <- function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
        game_state = "car_select",
        selected_car = NULL,
        level = 1,
        grid = NULL,
        player_pos = NULL,
        finish_pos = NULL,
        fuel = 100,
        path = list(),
        level_data = NULL,
        clicked_hex = NULL
    )
    
    # Generate level when car is selected or level changes
    observeEvent(c(rv$selected_car, rv$level), {
        if (!is.null(rv$selected_car) && rv$game_state == "playing") {
            level_data <- generate_level(rv$level)
            rv$level_data <- level_data
            rv$grid <- level_data$grid
            rv$player_pos <- level_data$start
            rv$finish_pos <- level_data$finish
            rv$fuel <- 100
            rv$path <- list(level_data$start)
        }
    })
    
    # Car selection
    observeEvent(input$select_rally, {
        rv$selected_car <- "rally"
        rv$game_state <- "playing"
    })
    
    observeEvent(input$select_suv, {
        rv$selected_car <- "suv"
        rv$game_state <- "playing"
    })
    
    observeEvent(input$select_truck, {
        rv$selected_car <- "truck"
        rv$game_state <- "playing"
    })
    
    # Reset level
    observeEvent(input$reset, {
        if (!is.null(rv$selected_car)) {
            level_data <- generate_level(rv$level)
            rv$level_data <- level_data
            rv$grid <- level_data$grid
            rv$player_pos <- level_data$start
            rv$finish_pos <- level_data$finish
            rv$fuel <- 100
            rv$path <- list(level_data$start)
            rv$game_state <- "playing"
        }
    })
    
    # Next level
    observeEvent(input$next_level, {
        rv$level <- rv$level + 1
        rv$game_state <- "playing"
    })
    
    # Retry level
    observeEvent(input$retry, {
        level_data <- generate_level(rv$level)
        rv$level_data <- level_data
        rv$grid <- level_data$grid
        rv$player_pos <- level_data$start
        rv$finish_pos <- level_data$finish
        rv$fuel <- 100
        rv$path <- list(level_data$start)
        rv$game_state <- "playing"
    })
    
    # Handle hex clicks
    observeEvent(input$hex_click, {
        if (rv$game_state != "playing") return()
        if (is.null(input$hex_click)) return()
        
        click <- input$hex_click
        
        # Convert pixel coordinates to hex coordinates
        hex_coords <- pixel_to_hex(click$x, click$y, size = 1)
        clicked_q <- hex_coords[1]
        clicked_r <- hex_coords[2]
        
        rv$clicked_hex <- c(clicked_q, clicked_r)
        
        # Check if hex exists in grid
        hex_idx <- which(rv$grid$q == clicked_q & rv$grid$r == clicked_r)
        if (length(hex_idx) == 0) return()
        
        # Check if adjacent
        neighbors <- get_neighbors(rv$player_pos[1], rv$player_pos[2])
        is_adjacent <- any(neighbors$q == clicked_q & neighbors$r == clicked_r)
        
        if (!is_adjacent) return()
        
        terrain <- rv$grid$terrain[hex_idx]
        fuel_cost <- cars[[rv$selected_car]]$costs[[terrain]]
        
        if (rv$fuel < fuel_cost) {
            rv$game_state <- "lost"
            return()
        }
        
        # Move player
        rv$player_pos <- c(clicked_q, clicked_r)
        rv$fuel <- rv$fuel - fuel_cost
        rv$path[[length(rv$path) + 1]] <- c(clicked_q, clicked_r)
        
        # Check if won
        if (clicked_q == rv$finish_pos[1] && clicked_r == rv$finish_pos[2]) {
            rv$game_state <- "won"
        }
    })
    
    # Main UI rendering
    output$game_ui <- renderUI({
        if (rv$game_state == "car_select") {
            fluidRow(
                column(12, align = "center",
                       h3("Choose Your Vehicle", style = "margin-bottom: 30px;"),
                       actionButton("select_rally", 
                                    HTML(paste0("<div style='font-size: 48px;'>", cars$rally$icon, "</div>",
                                                "<div style='font-size: 20px; font-weight: bold;'>", cars$rally$name, "</div>",
                                                "<div style='font-size: 14px; margin-top: 10px;'>",
                                                "Road: 1⛽ | Grass: 2⛽ | Mud: 3⛽ | Mountain: 5⛽</div>")),
                                    class = "btn-car"),
                       actionButton("select_suv",
                                    HTML(paste0("<div style='font-size: 48px;'>", cars$suv$icon, "</div>",
                                                "<div style='font-size: 20px; font-weight: bold;'>", cars$suv$name, "</div>",
                                                "<div style='font-size: 14px; margin-top: 10px;'>",
                                                "Road: 2⛽ | Grass: 2⛽ | Mud: 2⛽ | Mountain: 3⛽</div>")),
                                    class = "btn-car"),
                       actionButton("select_truck",
                                    HTML(paste0("<div style='font-size: 48px;'>", cars$truck$icon, "</div>",
                                                "<div style='font-size: 20px; font-weight: bold;'>", cars$truck$name, "</div>",
                                                "<div style='font-size: 14px; margin-top: 10px;'>",
                                                "Road: 3⛽ | Grass: 1⛽ | Mud: 1⛽ | Mountain: 2⛽</div>")),
                                    class = "btn-car")
                )
            )
        } else if (rv$game_state == "playing") {
            fluidRow(
                column(12,
                       div(class = "game-stats",
                           fluidRow(
                               column(3,
                                      h3(paste(cars[[rv$selected_car]]$icon, cars[[rv$selected_car]]$name))
                               ),
                               column(3,
                                      h4(paste("Level:", rv$level))
                               ),
                               column(3,
                                      h4(paste("Fuel:", rv$fuel, "⛽"))
                               ),
                               column(3,
                                      actionButton("reset", "Reset Level", class = "btn-action")
                               )
                           )
                       ),
                       div(class = "info-box",
                           p("Click on adjacent hexes (highlighted in yellow) to move. Yellow hexes indicate valid moves.")
                       ),
                       plotOutput("game_plot", click = "hex_click", height = "600px")
                )
            )
        } else if (rv$game_state == "won") {
            fluidRow(
                column(12, align = "center",
                       div(class = "victory-box",
                           h2("🏆 Level Complete!", style = "color: #fbbf24;"),
                           h4(paste("Fuel Remaining:", rv$fuel, "⛽")),
                           h4(paste("Level", rv$level)),
                           br(),
                           actionButton("next_level", "Next Level", class = "btn-action"),
                           actionButton("retry", "Retry Level", class = "btn-action")
                       )
                )
            )
        } else if (rv$game_state == "lost") {
            fluidRow(
                column(12, align = "center",
                       div(class = "defeat-box",
                           h2("⛽ Out of Fuel!", style = "color: #ef4444;"),
                           h4("You ran out of fuel before reaching the finish."),
                           br(),
                           actionButton("retry", "Try Again", class = "btn-action")
                       )
                )
            )
        }
    })
    
    # Game plot
    output$game_plot <- renderPlot({
        req(rv$grid, rv$player_pos, rv$finish_pos)
        
        # Create hex polygons
        hex_data <- rv$grid %>%
            rowwise() %>%
            mutate(
                coords = list(hex_to_pixel(q, r, size = 1))
            ) %>%
            ungroup()
        
        all_hexes <- data.frame()
        for (i in 1:nrow(hex_data)) {
            center <- hex_data$coords[[i]]
            hex_poly <- create_hex_polygon(center$x, center$y, size = 0.95)
            hex_poly$q <- hex_data$q[i]
            hex_poly$r <- hex_data$r[i]
            hex_poly$terrain <- hex_data$terrain[i]
            all_hexes <- rbind(all_hexes, hex_poly)
        }
        
        # Check which hexes are in path
        path_df <- do.call(rbind, rv$path)
        if (!is.null(path_df)) {
            path_df <- as.data.frame(path_df)
            colnames(path_df) <- c("q", "r")
            all_hexes <- all_hexes %>%
                left_join(path_df %>% mutate(in_path = TRUE), by = c("q", "r")) %>%
                mutate(in_path = ifelse(is.na(in_path), FALSE, in_path))
        } else {
            all_hexes$in_path <- FALSE
        }
        
        # Check which hexes are adjacent to player
        neighbors <- get_neighbors(rv$player_pos[1], rv$player_pos[2])
        all_hexes <- all_hexes %>%
            left_join(neighbors %>% mutate(is_adjacent = TRUE), by = c("q", "r")) %>%
            mutate(is_adjacent = ifelse(is.na(is_adjacent), FALSE, is_adjacent))
        
        # Player and finish positions
        player_coords <- hex_to_pixel(rv$player_pos[1], rv$player_pos[2], size = 1)
        finish_coords <- hex_to_pixel(rv$finish_pos[1], rv$finish_pos[2], size = 1)
        
        # Plot
        p <- ggplot() +
            geom_polygon(
                data = all_hexes,
                aes(x = x, y = y, group = interaction(q, r), fill = terrain),
                alpha = 0.7,
                color = "#1e293b",
                size = 0.5
            ) +
            geom_polygon(
                data = all_hexes %>% filter(in_path),
                aes(x = x, y = y, group = interaction(q, r)),
                fill = NA,
                color = "#60a5fa",
                size = 1.5
            ) +
            geom_polygon(
                data = all_hexes %>% filter(is_adjacent),
                aes(x = x, y = y, group = interaction(q, r)),
                fill = NA,
                color = "#fbbf24",
                size = 2
            ) +
            scale_fill_manual(values = terrain_colors) +
            geom_text(
                data = player_coords,
                aes(x = x, y = y),
                label = cars[[rv$selected_car]]$icon,
                size = 15
            ) +
            geom_text(
                data = finish_coords,
                aes(x = x, y = y),
                label = "🏁",
                size = 15
            ) +
            coord_equal() +
            theme_void() +
            theme(
                legend.position = "bottom",
                legend.text = element_text(color = "white", size = 12),
                legend.title = element_text(color = "white", size = 14),
                plot.background = element_rect(fill = "#1e293b", color = NA),
                panel.background = element_rect(fill = "#1e293b", color = NA)
            ) +
            labs(fill = "Terrain")
        
        p
    }, bg = "#1e293b")
}

shinyApp(ui = ui, server = server)