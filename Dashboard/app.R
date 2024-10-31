library(shiny)
library(bslib)
library(dplyr)
library(heatmaply)
library(ggalt)

# Set primary color ####
PRIMARY <- "#267326"

# Variable& Label Naming ####
values <- c("Universalism",
            "Benevolence",
            "Self-Direction",
            "Stimulation",
            "Hedonism",
            "Achievement",
            "Power",
            "Security",
            "Tradition",
            "Conformity")

# Names & Values ###############################################################
## Value explanations ####
value_explanations <- 
  c("Thinking up new ideas and being creative is important to him/her. He/she likes to do things in his/her own original way.\nIt is important to him/her to make his/her own decisions about what he/she does. He/she likes to be free and not depend on others.",
    "He/she likes surprises and is always looking for new things to do. He/she thinks it is important to do lots of different things in life.\nHe/she looks for adventures and likes to take risks. He/she wants to have an exciting14 life",
    "Having a good time is important to him/her. He/she likes to “spoil” himself/herself.\nHe/she seeks every chance he/she can to have fun. It is important to him/her to do things that give him/her pleasure.",
    "It's important to him/her to show his/her abilities. He/she wants people to admire4 what he/she does.\nBeing very successful is important to him/her. He/she hopes people will recognise his/her achievements.",
    "It is important to him/her to be rich. He/she wants to have a lot of money and expensive things.\nIt is important to him/her to get respect from others. He/she wants people to do what he/she says.",
    "It is important to him/her to live in secure5 surroundings. He/she avoids anything that might endanger his/her safety.\nIt is important to him/her that the government ensures his/her safety against all threats. He/she wants the state to be strong so it can defend its citizens.",
    "It is important to him/her to be humble and modest. He/she tries not to draw attention to himself/herself.\nTradition is important to him/her. He/she tries to follow the customs handed down by his/her religion or his/her family.",
    "He/she believes that people should do what they're told. He/she thinks people should follow rules at all times, even when no-one is watching.\nIt is important to him/her always to behave properly. He/she wants to avoid doing anything people would say is wrong.",
    "It's very important to him/her to help the people around him/her. He/she wants to care for their well-being.\nIt is important to him/her to be loyal to his/her friends. He/she wants to devote himself/herself to people close to him/her.",
    "He/she thinks it is important that every person in the world should be treated equally. He/she believes everyone should have equal opportunities in life.\nHe/she strongly believes that people should care for17 nature. Looking after the environment is important to him/her.")

## Goal names (de) ####
goals_de <-
  c("Orientierung an der freiheitlich-demokratischen Grundordnung/Demokratieerziehung",
    "Einüben von Toleranz",
    "Erwerb sozialer Kompetenzen",
    "Förderung eigenverantwortliches Handeln",
    "Vorbereitung auf das zukünftige Leben",
    "Anerkennung von kultureller Vielfalt",
    "Einsatz für den Frieden",
    "Förderung der Persönlichkeitsentwicklung",
    "Schaffung von Verantwortungsbewusstsein für Natur und Umwelt",
    "Achtung der Menschenrechte",
    "Anerkennung gesellschaftlicher Grundwerte",
    "Gleichberechtigung zwischen den Geschlechtern",
    "Orientierung an Leistungsfähigkeit",
    "Förderung der Heimatverbundenheit",
    "Förderung des selbstständigen Lernens",
    "Erwerb von Konfliktfähigkeiten/friedlicher Umgang mit Konflikten")

## Goal names (en)
goals_en <-
  c("Democracy Education",
    "Practising tolerance",
    "Social compentencies",
    "Independent action",
    "Preparation for future life",
    "Recognition of cultural diversity",
    "Commitment to peace",
    "Personal development",
    "Responsibility for the environment",
    "Respecting human rights",
    "Recognition of basic social values",
    "Equality between the genders",
    "Orientation towards achievement",
    "Promotion of a sense of home",
    "Promotion of independent learning",
    "Peaceful handling of conflicts")


# UI Elements ##################################################################
## Sidebar Accordion ######################

sidebar_acc <- accordion(
  open = T,
  multiple = T,
  ### Goal Selection ####
  accordion_panel(
    "Educational Goal",
    icon = fontawesome::fa("bullseye"),
    uiOutput("flight_path_reset"),
    selectizeInput(
      "goal", "Goal",
      choices = goals_en,
      multiple = F,
      options = list(plugins = "remove_button", closeAfterSelect = TRUE)
    )
  ),
  
  ### Covariates #####
  accordion_panel(
    "Filter by Covariate",
    icon = fontawesome::fa("filter"),
    selectInput("sex", "Teachers' Sex",
                choices = c("male", "female"),
                multiple = T),
    selectInput("age", "Teachers' Age",
                choices = c("<35",
                            "35-44",
                            "45-50",
                            "50+"),
                multiple = T),
    selectInput("years_teaching", "Years Teaching",
                choices = c("less than 5 years",
                            "5-9 years",
                            "10-19 years",
                            "20-29 years",
                            "more than 30 years"),
                multiple = T),
    selectInput("federal_state", "Federal State",
                choices = c("Schleswig-Holstein",
                            "Hamburg",
                            "Lower Saxony",
                            "Bremen",
                            "North Rhine-Westphalia",
                            "Hessen",
                            "Rhineland-Palatinate",
                            "Baden-Württemberg",
                            "Bavaria",
                            "Saarland",
                            "Berlin",
                            "Brandenburg",
                            "Mecklenburg-Western Pomerania",
                            "Saxony",
                            "Saxony-Anhalt",
                            "Thuringia"),
                multiple = T),
    selectInput("school_type", "School Type",
                choices = c("Primary School", 
                            "Lower Track Secondary School", 
                            "Middle Track Secondary School", 
                            "Comprehensive School", 
                            "Academic Track Secondary School",
                            "Special Education", 
                            "Other"),
                multiple = T)
  )
)


# UI ###########################################################################

ui <- page_navbar(
  
  ## bs_theme ####
  theme = bs_theme(
    preset = "pulse",
    "primary" = "#267326"
  ),
  lang = "en",
  title = tags$span(
    tags$img(
      src = "ph-logo.svg",
      width = "46px",
      height = "auto",
      class = "me-3",
      alt = "PH Logo"
    ),
    "Teachers Eduactional Goals as a Function of Their Values"
  ),
  
  sidebar = sidebar(width = 275, sidebar_acc),
  nav_spacer(),
  
  ## Value Boxes ####
  nav_panel(
    "Teachers Goals & Values",
    class = "bslib-page-dashboard",
    uiOutput("value_boxes"),
    
    ## Product Plots ####
    layout_columns(
      card(
        full_screen = TRUE,
        card_header(
          "Association of Goal and Values",
          class = "d-flex justify-content-between align-items-center"
        ),
        plotOutput("product_plots")
      ),
      
    ## Lollipop Plot ####  
      card(
        full_screen = TRUE,
        card_header(
          "Importance of Goal",
          class = "d-flex justify-content-between align-items-center"
        ),
        #verbatimTextOutput("debug"),
        plotOutput("lollipop_plot")
      )
    )),
  
  nav_item(
    tags$a(
      tags$span(
        bsicons::bs_icon("code-slash"), "Source code"
      ),
      href = "https://github.com/rstudio/bslib/tree/main/inst/examples-shiny/flights",
      target = "_blank"
    )
  ),
  
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)


# server #######################################################################
server <- function(input, output, session) {
  
  ## Reactive Data ###########
  ### Filtering ###########
  data <- reactive({
    data <- read_csv("data/data.csv")
     if(!is.null(input$sex)) {
      data <- data %>% filter(Sex %in% input$sex)
     }
    
    if(!is.null(input$age)) {
      data <- data %>% filter(Age %in% input$age)
    }
    
    if(!is.null(input$federal_state)) {
      data <- data %>% filter(`Federal State` %in% input$federal_state)
    }
    
    if(!is.null(input$school_type)) {
      data <- data %>% filter(`School Type` %in% input$school_type)
    }
    
    if(!is.null(input$years_teaching)) {
      data <- data %>% filter(`Years Teaching` %in% input$years_teaching)
    }
    
    return(data)
  })
  
  ### Goal Rank Calculation ####
  data_goals_rank <- 
    reactive({
      data_goals_long <- 
        data() %>%
        select(`Democracy Education`:`Peaceful handling of conflicts`,
               PID, weight0) %>% 
        gather(goal, endorsement, -PID, -weight0) %>% 
        mutate(endorsement = as_numeric(endorsement))
      
      data_goals_rank <- 
        data_goals_long %>%
        group_by(goal) %>%
        mutate(mean_endorsement = weighted.mean(endorsement, weight0, na.rm = T)) %>%
        ungroup() %>% 
        select(goal, mean_endorsement) %>%
        distinct() %>%
        mutate(rank_goal = rank(mean_endorsement),
               rank_goal = max(rank_goal) + 1 - rank_goal,
               rank_goal = case_when(rank_goal > 3 ~ paste0(rank_goal, "th"),
                                     rank_goal == 3 ~ paste0(rank_goal, "rd"),
                                     rank_goal == 2 ~ paste0(rank_goal, "nd"),
                                     rank_goal == 1 ~ paste0(rank_goal, "st")))
      
     return(data_goals_rank)
    })
  
  ### Goal Value Association Calc ####
  data_goals_value_association <- reactive({
    data() %>% 
      select(`Self-Direction`: `Universalism`, 
             `Democracy Education`:`Peaceful handling of conflicts`) %>%
      correlate() %>% 
      select(input$goal, term) %>% 
      filter(term %in% values) %>% 
      arrange(!!rlang::sym(input$goal))
      })
  
  ## Value Box Rendering ####
  output$value_boxes <- renderUI({
  
    goal_rank <- value_box(
      "THIS GOAL RANKS",
      data_goals_rank() %>% 
        filter(goal == input$goal) %>% 
        pull(rank_goal),
      paste("across the 16 constitutional goals"),
      showcase = bsicons::bs_icon("award-fill"),
      theme = value_box_theme("primary")
    )

    closest_value <- value_box(
      "MOST RELATED VALUE IS",
      data_goals_value_association()[10,2],
      showcase = bsicons::bs_icon("link"),
      theme = value_box_theme("primary")
    )

    relation_strength <- value_box(
      "THE CORRELATION WITH THIS VALUE IS",
      data_goals_value_association()[10,1] %>% 
        round(2) %>% 
        sprintf("%.2f", .),
      "Theoretical Range: [-1,1]",
      showcase = bsicons::bs_icon("speedometer2"),
      theme = value_box_theme("primary")
    )
    
    layout_columns(class = "mb-0", goal_rank, closest_value, relation_strength)
  })

  
  ## Product Plots Rendering ####
  output$product_plots <- renderPlot({
    data() %>% 
      select(`Self-Direction`:`Universalism`, 
             input$goal, PID) %>%
      gather(variable, value, -PID, -input$goal) %>%
      mutate(variable = factor(variable, levels = values)) %>%
      ggplot(aes_string(x = "value", y = as.name(input$goal))) +
        geom_jitter(alpha = .4, shape = 1) +
        facet_wrap(~ variable,
                   ncol = 3) +
        stat_smooth(method = "lm",
                    se = F, color = PRIMARY) +
        coord_fixed() +
        theme_minimal(base_size = 16)
  })
    
    
    ## Lollipop-Plot Rendering ####
    output$lollipop_plot <- renderPlot({
      ggplot(data_goals_rank() %>% 
               mutate(selected = goal == input$goal),
              aes(mean_endorsement, 
                  fct_reorder(goal, mean_endorsement),
                  color = selected)) + 
        geom_lollipop(horizontal=TRUE, point.size = 5) +
        scale_color_manual(values = c("grey", PRIMARY)) +
        labs(x = "Average Goal Endorsement",
             y = "",
             title = "") +
        theme_minimal(base_size = 20) +
        #theme_light(base_size = 16) +
        theme(legend.position = "none",
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank())
    
  })
  
  ## Debug Rendering ####
  output$debug <- renderPrint({
    getCurrentTheme()
    
  })
  
  

}

shinyApp(ui, server)
