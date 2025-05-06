library(magrittr)
library(tidyverse)
library(htmltools)
library(shiny)
library(DT)
library(ggpubr)
library(plotly)

ds_file <- "https://www.soscisurvey.de/ki-melodie-24/?act=oR8RglXdWNLEwWLbzO10M6cH&vQuality="
options(encoding = "UTF-8")

questions <- c(str_c("SD0", 1:9, "_01"),
               str_c("OR0", 1:9, "_01"),
               str_c("IN0", 1:9, "_01"),
               str_c("FA0", 1:9),
               "SD10_01",
               "OR10_01",
               "IN10_01",
               "FA10",
               "QUESTNNR") %>%
  `names<-`(
    c(
      str_c("bsp", 1:9, "_response"),
      str_c("bsp", 1:9, "_originality"),
      str_c("bsp", 1:9, "_interest"),
      str_c("bsp", 1:9, "_familiarity"),
      str_c("bsp10",
            c("response", "originality", "interest", "familiarity"),
            sep = "_"),
      "session"
    )
  )
sessions <- str_c("sitzung_0", 1:9) %>% `names<-`(str_c("Sitzung ", 1:9))
stimuli <- read.csv("stimuli.csv") %>%
  mutate(stimulus = str_c(session, bsp, sep = "_"))

# colors for SDT plots
sdt_colors <- c("darkgreen", "brown", "green", "red")

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(paste0("Seminar: KI-Melodie - Wie künstliche Intelligenz die Musik verändert")),
  h1("Mensch oder Maschine?"),
  fluidRow(
    # column(width = 4,
    #        fileInput("upload", NULL, multiple = FALSE)),
    column(width = 4,
           actionButton("get_data", "Daten abrufen")),
    column(width = 4,
           checkboxInput("familiar_filter", "Filter: Stimulus ist bekannt",
                         value = TRUE))
  ),
  tabsetPanel(
    ## UI: frequencies corrects ----------------------------------------------
    tabPanel("Häufigkeiten: Korrekte Antworten",
             fluidRow(
               column(4,
                      tableOutput("frequency_table_corrects")),
               column(4,
                      plotlyOutput("frequency_plot_corrects")),
               column(4,
                      plotlyOutput("frequency_plot_corrects_grouped")))
    ),
    ## UI: frequencies SDT ---------------------------------------------------
    tabPanel("Häufigkeiten: SDT",
             fluidRow(
               column(4,
                      tableOutput("frequency_table_classes")),
               column(8,
                      plotlyOutput("frequency_plot_classes")))
    ),
    ## UI: time series -------------------------------------------------------
    tabPanel("Zeitverlauf",
             sidebarLayout(
               sidebarPanel(width = 2,
                            selectInput("filter_sessions", "Sitzungen",
                                        multiple = TRUE, choices = sessions,
                                        selected = sessions),
                            uiOutput("filter_names")
               ),
               mainPanel(plotlyOutput("time_series"))
             )),
    ## UI: per stimulus ----------------
    tabPanel("Korrekte Antworten pro Stimulus",
             selectInput("stimuli_session_1", "Sitzung",
                         choices = sessions, selected = "sitzung_01",
                         multiple = FALSE),
             htmlOutput("audio"),
             plotlyOutput("stimuli_plots")),
    ## UI: originality & intereset -------------------------------------------
    tabPanel("Originalität und Interesse",
             fluidRow(
               column(width = 4,
                      selectInput("stimuli_session_2", "Sitzung",
                                  choices = sessions, selected = "sitzung_01",
                                  multiple = FALSE)),
               column(width = 4,
                      selectInput("rating_select", "Rating-Skala",
                                  choices = list(`Originalität` = "originality",
                                                 `Interesse` = "interest"),
                                  selected = "originality", multiple = FALSE))
             ),
             plotlyOutput("rating_plots")),
    ## UI: raw data ----------------------------------------------------------
    tabPanel("Rohdaten",
             dataTableOutput("data_raw"))
  )
)

# Define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  ## data_raw as reactive expresssion ------------------------------------------
  data_raw <- reactive({
    # req(input$upload)
    ds <-
      read.delim(
        file=ds_file, encoding="UTF-8", fileEncoding="UTF-8",
        header = FALSE, sep = "\t", quote = "\"",
        dec = ".", row.names = NULL,
        col.names = c(
          "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","FA01","FA02","FA03","FA04",
          "FA05","FA06","FA07","FA08","FA09","FA10","IN01_01","IN02_01","IN03_01",
          "IN04_01","IN05_01","IN06_01","IN07_01","IN08_01","IN09_01","IN10_01","IV01_01",
          "KR01_01","KR01_02","KR01_03","KR01_04","N001_01","N002_01","OR01_01","OR02_01",
          "OR03_01","OR04_01","OR05_01","OR06_01","OR07_01","OR08_01","OR09_01","OR10_01",
          "SD01_01","SD02_01","SD03_01","SD04_01","SD05_01","SD06_01","SD07_01","SD08_01",
          "SD09_01","SD10_01","SO01","SO02","SO03_01","SO04_01","SO05","TIME001",
          "TIME002","TIME003","TIME004","TIME005","TIME006","TIME007","TIME008","TIME009",
          "TIME010","TIME011","TIME012","TIME_SUM","MAILSENT","LASTDATA","FINISHED",
          "Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI"
        ),
        as.is = TRUE,
        colClasses = c(
          CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
          MODE="factor", STARTED="POSIXct", FA01="numeric", FA02="numeric",
          FA03="numeric", FA04="numeric", FA05="numeric", FA06="numeric",
          FA07="numeric", FA08="numeric", FA09="numeric", FA10="numeric",
          IN01_01="numeric", IN02_01="numeric", IN03_01="numeric", IN04_01="numeric",
          IN05_01="numeric", IN06_01="numeric", IN07_01="numeric", IN08_01="numeric",
          IN09_01="numeric", IN10_01="numeric", IV01_01="character",
          KR01_01="numeric", KR01_02="numeric", KR01_03="numeric", KR01_04="numeric",
          N001_01="character", N002_01="character", OR01_01="numeric",
          OR02_01="numeric", OR03_01="numeric", OR04_01="numeric", OR05_01="numeric",
          OR06_01="numeric", OR07_01="numeric", OR08_01="numeric", OR09_01="numeric",
          OR10_01="numeric", SD01_01="numeric", SD02_01="numeric", SD03_01="numeric",
          SD04_01="numeric", SD05_01="numeric", SD06_01="numeric", SD07_01="numeric",
          SD08_01="numeric", SD09_01="numeric", SD10_01="numeric", SO01="numeric",
          SO02="numeric", SO03_01="numeric", SO04_01="numeric", SO05="numeric",
          TIME001="integer", TIME002="integer", TIME003="integer", TIME004="integer",
          TIME005="integer", TIME006="integer", TIME007="integer", TIME008="integer",
          TIME009="integer", TIME010="integer", TIME011="integer", TIME012="integer",
          TIME_SUM="integer", MAILSENT="POSIXct", LASTDATA="POSIXct",
          FINISHED="logical", Q_VIEWER="logical", LASTPAGE="numeric",
          MAXPAGE="numeric", MISSING="numeric", MISSREL="numeric", TIME_RSI="numeric"
        ),
        skip = 1,
        check.names = TRUE, fill = TRUE,
        strip.white = FALSE, blank.lines.skip = TRUE,
        comment.char = "",
        na.strings = ""
      )
    data <- #read.csv(input$upload$datapath) %>%
      ds %>%
      set_attr("server", NULL) %>%
      set_attr("description", NULL) %>%
      set_attr("date", NULL) %>%
      set_attr("project", NULL) %>%
      ### !!!FILTER 2024 DATA!!! ---------
      filter(
        STARTED >= lubridate::ymd("2025-04-15"),
        stringr::str_detect(N001_01, "test", negate = TRUE),
        !(CASE %in% c(83,85))
      ) %>%
      select(QUESTNNR, starts_with(c("N00", "SD", "OR", "IN", "FA"))) %>%
      # nick name
      #mutate(name = pmap_chr(list(init = N001_01, foll = N002_01),
      #                       function(init, foll) if_else(init != "", init, foll))) %>%
      mutate(name = N001_01 %>% gsub(" ", "", .) %>%
               gsub("@stud.hmtm-hannover.de", "", .)) %>%
      filter(!is.na(name), name != "", !is.na(SD01_01)) %>%
      select(!starts_with("N00")) %>%
      # rename questions
      rename(!!!questions) %>%
      # long format
      pivot_longer(cols = starts_with("bsp"),
                   names_sep = "_",
                   names_to = c("bsp", ".value")) %>%
      mutate(stimulus = str_c(session, bsp, sep = "_"),
             interest = interest %>%
               factor(levels = 1:5,
                      labels = c("gewöhnlich", "ein wenig interessant",
                                 "interessant", "sehr interessant",
                                 "innovativ")),
             originality = originality %>%
               factor(levels = 1:5),
             response_binary = !(response < 5)) %>%
      left_join(stimuli %>% select(stimulus, ki, matched_pair), by = "stimulus") %>%
      mutate(response_correct = (response_binary == ki),
             response_class = pmap_chr(list(resp = response_binary, ts = ki),
                                       function(resp, ts) {
                                         case_when(
                                           resp + ts == 0 ~ "correct rejection",
                                           resp + ts == 2 ~ "hit",
                                           resp - ts < 0 ~ "miss",
                                           resp - ts > 0 ~ "false alarm"
                                         )
                                       }),
             response_binary = if_else(response_binary, "Maschine", "Mensch")) %>%
      relocate(name, session, bsp, stimulus) %>%
      #select(-ki) %>%
      # recode to stimulus name
      mutate(stimulus = stimulus %>%
               recode(!!!(stimuli$stimulus_name %>%
                            `names<-`(stimuli$stimulus)))) %>%
      filter(!is.na(response_correct))

    data
  }) %>%
    bindEvent(input$get_data)

  ## data as reactive expression -----------------------------------------------
  data <- reactive({
    req(data_raw())
    data <- data_raw()

    if(input$familiar_filter) {
      data <- data %>% familiarity_filter()
    }

    data
  })

  ## frequency data & plots ----------------------------------------------------
  freq_classes <- reactive({
    req(data_raw())
    data() %>% group_by(response_class) %>% summarise(n = n(),
                                                      `%` = n / nrow(.) * 100)
  })
  output$frequency_table_classes <- renderTable({
    freq_classes() %>% rename(Antwortklasse = response_class)
  })

  output$frequency_plot_classes <- renderPlotly({
    freq_classes <- data() %>%
      ggplot(aes(x = response_class, fill = response_class)) +
      geom_bar() +
      theme_pubr() +
      scale_fill_manual(name = "Antwortklasse",
                        values = sdt_colors) +
      ylab("Anzahl")
    freq_classes %>% ggplotly()
  })

  output$frequency_table_corrects <- renderTable({
    data() %>% group_by(response_binary) %>%
      summarise(n = n(),
                korrekt = sum(response_correct),
                falsch = sum(!response_correct)) %>%
      add_case(response_binary = "alle",
               n = sum(.$n),
               korrekt = sum(.$korrekt),
               falsch = sum(.$falsch)) %>%
      mutate(`korrekt rel.` = round(korrekt / n * 100, digits = 1) %>%
               str_c(" %"),
             `falsch rel.` = round(falsch / n * 100, digits = 1) %>%
               str_c(" %")) %>%
      relocate(falsch, .after = `korrekt rel.`) %>%
      rename(`gegebene Antwort` = response_binary)
  })

  output$frequency_plot_corrects <- renderPlotly({
    freq_plot <- data() %>%
      ggplot(aes(x = response_correct, fill = response_correct)) +
      geom_bar() +
      theme_pubr() +
      xlab("Antwort") +
      ylab("Anzahl")
    freq_plot %>% ggplotly()
  })

  output$frequency_plot_corrects_grouped <- renderPlotly({
    freq_plot_grouped <- data() %>%
      ggplot(aes(x = response_binary, fill = response_correct)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      theme_pubr() +
      scale_x_discrete(name = "gegebene Antwort",drop = FALSE) +
      ylab("Anzahl")
    freq_plot_grouped %>% ggplotly()
  })

  ## time series ---------------------------------------------------------------
  output$time_series <- renderPlotly({
    req(plot_data())
    plot <- plot_data() %>%
      rename(Antwortklasse = response_class) %>%
      ggplot(aes(x = session, fill = Antwortklasse)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      theme_pubr() +
      scale_x_discrete(drop = FALSE, name = "Sitzung") +
      scale_fill_manual(values = sdt_colors) +
      ylab("Anzahl")
    plot %>% ggplotly()
  })

  output$data_raw <- renderDataTable({
    data_raw()
  })

  output$filter_names <- renderUI({
    all_names <- data()$name %>% unique()
    selectInput("filter_names", "Nick-Names", multiple = TRUE,
                choices = all_names, selected = all_names)
  })

  plot_data <- reactive({
    req(data())
    data() %>%
      filter_names(input) %>%
      filter_stimuli_session(input, filter = "filter_sessions")
  })

  output$stimuli_plots <- renderPlotly({
    stim_plots <- data() %>%
      filter_stimuli_session(input) %>%
      rename(Antwort = response_correct) %>%
      ggplot(aes(x = Antwort, fill = Antwort)) +
      geom_bar(position = position_dodge(preserve = "single")) +
      theme_pubr() +
      scale_x_discrete(drop = FALSE) +
      #scale_fill_manual(values = ) +
      facet_wrap(facets = vars(stimulus))
  })

  observe({
    selected_session <- input$stimuli_session_1
    updateSelectInput(session,
                      "stimuli_session_2",
                      selected = selected_session)
  })

  observe({
    selected_session <- input$stimuli_session_2
    updateSelectInput(session,
                      "stimuli_session_1",
                      selected = selected_session)
  })

  rating_plot <- reactive({
    req(data_raw())
    data() %>%
      filter_stimuli_session(input) %>%
      mutate(Urheber = if_else(ki, "Maschine", "Mensch")) %>%
      ggplot(aes(x = !!sym(input$rating_select), fill = Urheber)) +
      geom_bar() +
      theme_pubr() +
      scale_y_continuous(breaks = scales::breaks_pretty(), name = "Anzahl") +
      scale_x_discrete(drop = FALSE, name = "Antwort") +
      facet_wrap(facets = vars(stimulus), ncol = 2)
  })

  output$rating_plots <- renderPlotly({
    rating_plot() %>%
      ggplotly()
  })

  output$audio <- renderUI({
    audio_players(stimuli = stimuli, input = input)
  })
}

# function to filter the data depending on inputs
filter_stimuli_session <- function(data, input, filter = "stimuli_session_1") {
  data %>% filter(session %in% input[[filter]])
}
filter_names <- function(data, input) {
  data %>% filter(name %in% input$filter_names)
}
audio_players <- function(stimuli = stimuli, input) {
  audios <- stimuli %>% as_tibble() %>%
    filter(session %in% input$stimuli_session_1) %>%
    select(stimulus_name, link) %>%
    mutate(stimulus_name = stimulus_name %>%
             map(function(name) {
               tags$td(name)
             }),
           link = link %>%
             map(function(link) {
               tags$td(tags$audio(src = link, autoplay = NULL, controls = NA))
             }))
  div(
    tags$table(
      tags$tr(audios$stimulus_name),
      tags$tr(audios$link)
    )
  )
}

# familiarity filter
familiarity_filter <- function(data) {
  famils <- data %>%
    # participant thinks they know the composer of a stimulus
    filter(familiarity == 1) %>%
    # participant was right
    filter(response_correct)

  data$filter_matched_pair <- FALSE

  for (i in seq_along(famils$name)) {
    n <- famils$name[i]
    p <- famils$matched_pair[i]
    data$filter_matched_pair[data$name == n & data$matched_pair == p] <- TRUE
  }

  data %>% filter(!filter_matched_pair)
}

# Run the application
shinyApp(ui = ui, server = server)
