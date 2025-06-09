library(shiny)
library(visNetwork)
library(ggraph)
library(plotly)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(igraph)
library(vistime)
library(bslib)
library(scales)

# --------------------------------------------------
# 1. DIY bslib theme
# --------------------------------------------------
theme_covert <- bs_theme(
  bg           = "#585651",    # overall background
  fg           = "#090605",    # text color
  primary      = "#ba9b8d",    # primary color
  secondary    = "#814a38",    # secondary color
  base_font    = font_google("Rubik"),
  heading_font = font_google("Rubik")
)

# --------------------------------------------------
# UI
# --------------------------------------------------
ui <- fluidPage(
  theme = theme_covert,
  tags$head(
    tags$style(HTML("
      html, body { scroll-behavior: smooth; }
      .navbar-covert { background-color: #FFF; }
      .navbar-covert .nav-link { color: #c17e72 !important; font-weight: 500; }
      .navbar-covert .nav-link:hover { color: #DBCAC3 !important; }
      body { padding-top: 70px; }
      section { padding: 60px 20px; }
      /* web BG set for #DBCAC3 section */
#home,
#sketch,
#task2,
#task4 {
  background-color: #DBCAC3 !important;
}

/* web BG set for #ebe2de section */
#timeline,
#task1,
#task3,
#team {
  background-color: #ebe2de !important;
}


      /* TabsetPanel  */
      .nav-tabs .nav-link {
        color: #000 !important;
        background-color: #bda59f !important;
      }
      .nav-tabs .nav-item.show .nav-link,
      .nav-tabs .nav-link.active {
        color: #000 !important;
        background-color: #bda59f !important;
      }

      /* Dropdown box color matching */
      .form-control.shiny-bound-input,
      .selectize-control.single .selectize-input {
        background-color: #bda59f !important;
        color: #000 !important;
        border-color: #bda59f !important;
      }
      .selectize-dropdown {
        background-color: #bda59f !important;
        color: #000 !important;
      }

      /* dateRangeInput bg & text */
      #date_range .form-control {
        background-color: #bda59f !important;
        color: #000 !important;
        border-color: #bda59f !important;
      }
      #date_range .input-group-text {
        background-color: #bda59f !important;
        color: #000 !important;
        border-color: #bda59f !important;
      }
    "))
  ),
  
  # Navbar
  tags$nav(class = "navbar navbar-expand-lg navbar-covert fixed-top",
           tags$div(class = "container-fluid",
                    tags$a(class = "navbar-brand text-white", href = "#home",
                           tags$span("COVERT REEF "), tags$span("\U0001FAB8", style = "font-size:24px;")
                    ),
                    tags$button(class = "navbar-toggler", type = "button",
                                `data-bs-toggle` = "collapse", `data-bs-target` = "#menuCovert",
                                tags$span(class = "navbar-toggler-icon")
                    ),
                    tags$div(class = "collapse navbar-collapse", id = "menuCovert",
                             tags$ul(class = "navbar-nav ms-auto",
                                     tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#home",     "Home")),
                                     tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#timeline","Timeline")),
                                     tags$li(class=  "nav-item", tags$a(class="nav-link", href="#sketch", "UI Sketch")),
                                     tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#task1",   "Task 1")),
                                     tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#task2",   "Task 2")),
                                     tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#task3",   "Task 3")),
                                     tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#task4",   "Task 4")),
                                     tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#team",    "Team"))
                             )
                    )
           )
  ),
  
  # Home Section
  tags$section(id = "home",
               fluidRow(column(12,
                               h1("Project Introduction"),
                               p("Clepper, a lead investigator on Oceanus, has been closely monitoring the closure of Nemo Reef. Over the span of two weeks, he and his intern listened to and analyzed radio communications and utilized his investigative tools to uncover a complex web of expedited approvals, hidden communication, and secretive logistics. Their investigation revealed a story involving high-level Oceanus officials, Sailor Shift’s team, local influential families, and the Green Guardians, who are a local conservationist group, pointing towards possibilities of corruption and manipulation. Amidst this investigation, Nadia Conti, a known figure formerly entangled in illegal fishing operations, has resurfaced as a person of interest. The project aims to develop new and novel visual analytics techniques to support Clepper’s investigation in uncovering the full extent of the events on Oceanus."),
                               h3("Objectives"),
                               tags$ul(
                                 tags$li("Identifying daily temporal patterns in communication to detect recurring message timings."),
                                 tags$li("Analysing the shifts in communication patterns across the two weeks."),
                                 tags$li("Focusing on specific entities to determine influence within the network."),
                                 tags$li("Visualising the interactions between people and vessels to explore the relationships in the knowledge graph."),
                                 tags$li("Applying community detection to uncover groups that are closely associated and the predominant topic areas for each group."),
                                 tags$li("Detecting which person or vessel is using pseudonyms and unraveling them."),
                                 tags$li("Identifying common entities in the knowledge graph."),
                                 tags$li("Understanding activities by tracking interactions and unraveling pseudonyms."),
                                 tags$li("Providing evidence through visual analytics to determine whether Nadia Conti is engaging in illicit activity."),
                                 tags$li("Presenting a summary of Nadia’s actions and the visual reasoning behind any suspicion.")
                               )
               ))
  ),
  
  # Timeline Section
  tags$section(id = "timeline",
               fluidRow(column(12,
                               h2("Project Timeline"),
                               plotOutput("timeline_plot", height = "400px")
               ))
  ),
  
  
  # — UI Sketch —
  tags$section(id = "sketch",
               fluidRow(
                 column(12,
                        h2("UI Sketch"),
                        img(src = "sketch.jpg",
                            alt = "UI Sketch",
                            style = "max-width:100%; height:auto; border:1px solid #ccc;")
                 )
               )
  ),
  
  
  # Task 1 Section
  tags$section(id = "task1",
               fluidRow(column(12,
                               h2("Task 1: Interactive Visual Analytics"),
                               tags$ul(
                                 tags$li("1.1 Create visual analytics to identify daily patterns in communication timing."),
                                 tags$li("1.2 Analyze how these patterns changed over the two-week observation period."),
                                 tags$li("1.3 Focus on a specific entity and use this information to determine who has influence over them.")
                               ),
                               tabsetPanel(id = "task1_tabs", type = "tabs",
                                           
                                           # 1.1 Daily Patterns
                                           tabPanel("1.1 Daily Patterns",
                                                    fluidRow(
                                                      column(4, dateRangeInput("date_range", "Select date range:", start = NULL, end = NULL)),
                                                      column(4, sliderInput("hour_range", "Select hour range:", min = 0, max = 23, value = c(0,23), step = 1, ticks = FALSE))
                                                    ),
                                                    fluidRow(column(12, plotOutput("heatmap", height = "600px")))
                                           ),
                                           
                                           # 1.2 Week Comparison
                                           tabPanel("1.2 Week Comparison",
                                                    fluidRow(
                                                      column(4, checkboxGroupInput("weeks", "Select Comparison Week:", choices = c("Week 1","Week 2"), selected = c("Week 1","Week 2"))),
                                                      column(4, sliderInput("hour_range2", "Select Hourly Interval:", min = 0, max = 23, value = c(0,23), step = 1, ticks = FALSE))
                                                    ),
                                                    fluidRow(column(12, plotOutput("week_plot", height = "450px")))
                                           ),
                                           
                                           # 1.3 Influence Analysis
                                           tabPanel("1.3 Influence Analysis",
                                                    fluidRow(
                                                      column(6,
                                                             selectInput("subtype", "Select target_subtype:", choices = NULL),
                                                             plotlyOutput("inf_plot", height = "600px")
                                                      ),
                                                      column(6,
                                                             selectInput("target", "Select target entity:", choices = NULL),
                                                             plotOutput("net_plot", height = "600px")
                                                      )
                                                    )
                                           )
                                           
                               )
               ))
  ),
  
  # Task 2/3/4 & Team placeholders
  tags$section(id = "task2", fluidRow(column(12, h2("Task 2: Overview")))),
  tags$section(id = "task3", fluidRow(column(12, h2("Task 3: Overview")))),
  tags$section(id = "task4", fluidRow(column(12, h2("Task 4: Overview")))),
  tags$section(id = "team", fluidRow(column(12,
                                            h2("Team Members"),
                                            tags$ul(
                                              tags$li("Member 1 – Audrey"),
                                              tags$li("Member 2 – Li JianYi"),
                                              tags$li("Member 3 – Yang Lu")
                                            )
  )))
)

# --------------------------------------------------
# Server
# --------------------------------------------------
server <- function(input, output, session) {
  
  # --- Project Timeline Plot ---
  output$timeline_plot <- renderPlot({
    data <- read.csv(text = "
event,group,start,end,color
,Project Proposal,2025-06-02,2025-06-08,#a5d6a7
,Exploratory data analysis,2025-05-20,2025-06-08,#a5d6a7
,Exploratory data analysis,2025-06-08,2025-06-09,#DD4B39
,R Quarto/ Netlify,2025-05-20,2025-06-08,#a5d6a7
,R Quarto/ Netlify,2025-06-08,2025-06-30,#DD4B39
,R Shiny App,2025-06-15,2025-07-06,#DD4B39
,Poster,2025-06-23,2025-06-26,#DD4B39
,User Guide,2025-06-15,2025-06-26,#DD4B39
", stringsAsFactors = FALSE)
    data <- data %>%
      mutate(
        start = as.POSIXct(start),
        end   = as.POSIXct(end),
        color = recode(color,
                       `#a5d6a7` = "#8979BF",
                       `#DD4B39` = "#B0BF79"
        )
      )
    gg_vistime(data, title = NULL) +
      geom_vline(xintercept = as.numeric(as.POSIXct("2025-06-08")), color = "red", size = 1) +
      scale_x_datetime(date_breaks = "3 days", date_labels = "%d %b",
                       expand = expansion(mult = c(0.06,0.08))) +
      theme_minimal(base_family = "Rubik") +
      theme(
        panel.background   = element_rect(fill = NA, colour = NA),
        plot.background    = element_rect(fill = NA, colour = NA),
        panel.grid.major.x = element_line(color = "grey80", size = 0.3),
        axis.text.x        = element_text(angle = 45, hjust = 1)
      )
  }, bg = "transparent", res = 96)
  
  # --- Load & preprocess data for Task 1 ---
  MC3_graph <- fromJSON("data/MC3_graph.json")
  nodes_tbl  <- as_tibble(MC3_graph$nodes)
  edges_tbl  <- as_tibble(MC3_graph$edges)
  comm_nodes <- nodes_tbl %>% filter(type == "Event", sub_type == "Communication") %>%
    select(event_id = id, timestamp)
  sent_edges <- edges_tbl %>% filter(type == "sent") %>%
    select(sender = source, event_id = target)
  recv_edges <- edges_tbl %>% filter(type == "received") %>%
    select(event_id = source, receiver = target)
  msgs <- sent_edges %>%
    inner_join(recv_edges, by = "event_id") %>%
    inner_join(comm_nodes, by = "event_id") %>%
    mutate(
      ts   = ymd_hms(timestamp, tz = "UTC"),
      date = as_date(ts),
      hour = hour(ts)
    )
  heatmap_data <- msgs %>% count(date, hour) %>%
    complete(date = seq(min(date), max(date), by = "1 day"),
             hour = 0:23, fill = list(n = 0))
  updateDateRangeInput(session, "date_range",
                       start = min(heatmap_data$date),
                       end   = max(heatmap_data$date))
  week_patterns <- msgs %>% mutate(week = if_else(date <= min(date)+days(6),
                                                  "Week 1","Week 2")) %>%
    count(week, hour) %>% group_by(week) %>%
    mutate(proportion = n / sum(n)) %>% ungroup()
  entity_info <- nodes_tbl %>% filter(type == "Entity") %>%
    select(id, target_subtype = sub_type)
  msgs2 <- msgs %>% left_join(entity_info, by = c("receiver" = "id"))
  top_inf <- msgs2 %>% filter(!is.na(receiver)) %>%
    count(target_subtype, receiver, sender, sort = TRUE) %>%
    group_by(target_subtype, receiver) %>%
    slice_max(n, n = 10) %>% ungroup()
  subtypes <- unique(top_inf$target_subtype)
  updateSelectInput(session, "subtype",
                    choices = subtypes, selected = subtypes[1])
  
  # --- Build igraph ---
  ee_edges     <- msgs %>% select(from = sender, to = receiver)
  entity_nodes <- nodes_tbl %>% filter(type == "Entity") %>% select(id, name)
  g_ig <- graph_from_data_frame(d = ee_edges, vertices = entity_nodes, directed = TRUE)
  all_targets <- sort(V(g_ig)$name)
  updateSelectInput(session, "target",
                    choices = all_targets, selected = all_targets[1])
  
  # 1.1 Heatmap
  filtered_data <- reactive({
    req(input$date_range)
    heatmap_data %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        hour >= input$hour_range[1],
        hour <= input$hour_range[2]
      )
  })
  output$heatmap <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(hour, date, fill = n)) +
      geom_tile(color = "white") +
      scale_x_continuous(breaks = seq(input$hour_range[1], input$hour_range[2], by = 1),
                         expand = c(0,0)) +
      scale_y_date(date_labels = "%b %d", expand = c(0,0)) +
      scale_fill_distiller(name = "Messages", palette = "Spectral", direction = 1) +
      theme_minimal() + theme(panel.grid = element_blank())
  }, res = 96)
  
  # 1.2 Week Comparison
  filtered_patterns <- reactive({
    req(input$weeks, input$hour_range2)
    week_patterns %>%
      filter(
        week %in% input$weeks,
        hour >= input$hour_range2[1],
        hour <= input$hour_range2[2]
      )
  })
  output$week_plot <- renderPlot({
    df <- filtered_patterns()
    ggplot(df, aes(hour, proportion, color = week)) +
      geom_line(size = 1) + geom_point(size = 2) +
      scale_x_continuous(breaks = seq(input$hour_range2[1], input$hour_range2[2], by = 1)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "Hour", y = "Percent") +
      theme_light() + theme(legend.position = "top")
  }, res = 96)
  
  # 1.3 Influence Analysis - Bar
  filtered_inf <- reactive({
    req(input$subtype)
    top_inf %>% filter(target_subtype == input$subtype)
  })
  output$inf_plot <- renderPlotly({
    df <- filtered_inf()
    p <- ggplot(df, aes(reorder(sender, n), n, fill = receiver,
                        text = paste0("Sender: ", sender, "<br>Count: ", n))) +
      geom_col() + coord_flip() +
      scale_fill_viridis_d(option = "turbo", name = "Receiver") +
      labs(x = "Sender", y = "Count") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # 1.3 Influence Analysis - Network
  output$net_plot <- renderPlot({
    req(input$target)
    root_idx <- which(V(g_ig)$name == input$target)
    vids <- unique(c(
      root_idx,
      neighbors(g_ig, root_idx, mode = "in"),
      neighbors(g_ig, root_idx, mode = "out")
    ))
    subg <- induced_subgraph(g_ig, vids)
    ggraph(subg, layout = "kk") +
      geom_edge_link(arrow = arrow(length = unit(4, "mm")), end_cap = circle(3, "mm"), color = "grey70") +
      geom_node_point(aes(filter = (name == input$target)), color = "firebrick", size = 6) +
      geom_node_point(aes(filter = (name != input$target)), color = "steelblue", size = 4) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      labs(title = paste("Influence Network of", input$target)) + theme_void()
  }, res = 96)
}

# Launch app
shinyApp(ui, server)