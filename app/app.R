#!/usr/bin/env Rscript

source('functions.R')
source('dashboard.R')
options(shiny.reactlog = FALSE)
options(shiny.fullstacktrace = TRUE)

# Define UI for application
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = 'Peakfit Explorer'),
  dashboardSidebar(sidebarMenu(
    menuItem(
      'Visualize Data',
      tabName = 'explore',
      icon = icon('think-peaks')
    ),
    menuItem(
      'Gaussian Mixture Model',
      tabName = 'pkfit',
      icon = icon('mortar-pestle')
    )
  )),
  body
)

server <- function(input, output, session) {
  # Initialize reactive values (reactive values store values that need to change)
  vals <- reactiveValues()
  # Locations of peak positions (output from isoplotR function 'peakfit')
  vals$pks <- NULL
  vals$x1 <- NULL
  vals$y1 <- NULL
  vals$x2 <- NULL
  vals$y2 <- NULL
  data.file <- 'data/demo-data-trayler-et-al-2020.csv'
  if(file.exists(data.file)) {
    vals$data <-
      readr::read_csv(data.file, show_col_types = F) %>%
      group_by(sample) %>%
      mutate(suffix = row_number()) %>%
      ungroup() %>%
      mutate(suffix = ifelse(!is.na(suffix), str_c('-', suffix), '')) %>%
      mutate(sample = str_c(sample, suffix)) %>%
      select(-suffix)
  } else {
    vals$data <- tibble(
      sample = rep(NA_real_, 17),
      value = rep(NA_real_, 17),
      stdError = rep(NA_real_, 17),
      toggle = rep(FALSE, 17)
    )
  }
  # Generates interactive table populated with data from vals$data
  output$table <- renderRHandsontable({
    d <- vals$data
    rhandsontable(d, stretchH = 'all') %>% hot_cols(columnSorting = TRUE)
  })
  observe({
    if (!is.null(input$table)) {vals$data <- hot_to_r(input$table)} 
  })
  # This observer finds peaks when the find peaks button is pressed
  observeEvent(input$calc, {
    req(input$sigma)
    req(input$k)
    d <- vals$data %>% drop_na() %>% filter(toggle == TRUE)
    vals$pks <-
      pkfit(
        data = d,
        k = input$k,
        sigerror = input$sigma,
        sigdig = input$sigdig,
        sigrange = input$sigrange,
        alpha = input$alpha
    )
  })
  observeEvent(input$toggle, {
    req(vals$data)
    if(input$toggle == FALSE) {
      vals$data$toggle <- FALSE
    } else {
      vals$data$toggle <- TRUE
    }
  })
  # Reactive plot function updates the data plot
  p1 <- reactive({
    req(!is.null(vals$data))
    d <- vals$data %>% drop_na() %>% filter(toggle == TRUE)
    if (nrow(d) != 0) {
      p <- p.data(
        data = d,
        bins = input$bins1,
        sigrange = input$sigrange,
        sigerror = input$sigma,
        xlim = vals$x1,
        ylim = vals$y1,
        type = input$type1,
        arrange = input$arrange1,
        curves = input$curves1
      )
    } else {
      p <- NULL
    }
    return(p)
  })
  # Reactive plot function updates the peakfit plot
  p2 <- reactive({
    req(!is.na(vals$pks))
    req(input$sigrange)
    req(input$scalepdf)
    p <- try(
      p.pks(
        pks = vals$pks,
        bins = input$bins1,
        sigrange = input$sigrange,
        xlim = vals$x2,
        ylim = vals$y2,
        curves = input$curves2,
        scalepdf = input$scalepdf
      )
    )
    if (is(p.pks, "try-error")) {
      p <- NULL
    }
    return(p)
  })
  # Render data plot
  output$p1 <- renderPlot({
    p <- p1 %>% debounce(200)
    p()
  })
  # Render peakfit plot
  output$p2 <- renderPlot({
    p <- p2 %>% debounce(0)
    p()
  })
  # Reactive function that updates the verbatim text
  # output only when the peakfit plot is updated
  vtext <- eventReactive(p2(), {
    req(vals$pks)
    info <- try(paste0('peak,value,stdError\n',paste0(vals$pks$pks$legend, collapse = '\n')))
    if (is(info, "try-error")) {
      info <- 'Algorithm went singular. Please select another value for k'
    } else {
      info <- info
    }
    return(info)
  })
  # Render verbatim text output
  output$info <- renderText({
    t <- vtext %>% debounce(0)
    t()
  })
  # Download text output
  output$dlcsv <- renderUI({
    req(vtext)
    downloadButton('dlcsv', 'Download Peaks (csv)')
  })
  # Render acknowledgements verbatim text output
  output$acknowledgements <- renderText({
    paste(
      'This app uses functions from the IsoplotR package:\nVermeesch, P. (2018). https://doi.org/10.1016/j.gsf.2018.04.001.\n\nDemo data from Trayler et al. (2020). https://doi.org/10.1130/B35203.1.\n\nAll data, code, and relevant information for reproducing this work can be found at:\nhttps://github.com/buchanankerswell/pkft, and at https://doi.org/10.17605/OSF.IO/2BU3C, the official Open Science Framework data repository.\nAll code is MIT Licensed and free for use and distribution (see license details).\n\nPlease cite as:\nKerswell, B. (2023). Peakfit Explorer. https://doi.org/10.17605/OSF.IO/2BU3C.'
    )
  })
  # Reset data plot zoom when plot type changes
  observeEvent(input$type1, {
    vals$x1 <- NULL
    vals$y1 <- NULL
  })
  # Data plot zoom
  observeEvent(input$p1.dbl, {
    #======== ZOOM =======================
    # Zoom to brush window on double click
    brush <- input$p1.brush
    if (!is.null(brush)) {
      vals$x1 <- c(brush$xmin, brush$xmax)
      vals$y1 <- c(brush$ymin, brush$ymax)
    }
    else {
      vals$x1 <- NULL
      vals$y1 <- NULL
    }
  })
  # Peakfit plot zoom
  observeEvent(input$p2.dbl, {
    #======== ZOOM =======================
    # Zoom to brush window on double click
    brush <- input$p2.brush
    if (!is.null(brush)) {
      vals$x2 <- c(brush$xmin, brush$xmax)
      vals$y2 <- c(brush$ymin, brush$ymax)
    }
    else {
      vals$x2 <- NULL
      vals$y2 <- NULL
    }
  })
  # Output different user options depending on the plot type
  observe({
    type <- input$type1
    if (type == 'histogram') {
      output$curves1 <- renderUI({
        checkboxGroupInput(
          'curves1',
          'Choose Density Curve(s)',
          choices = c('norm', 'kernel', 'sumpdf', 'stackpdf', 'indv'),
          inline = TRUE
        )
      })
      output$bins1 <- renderUI({
        sliderInput(
          'bins1',
          'Number of Histogram Bins',
          min = 1,
          max = 50,
          value = 30,
          ticks = FALSE
        )
      })
      output$arrange1 <- NULL
    } else {
      output$curves1 <- NULL
      output$bins1 <- NULL
      output$arrange1 <- renderUI({
        selectInput(
          'arrange1',
          label = 'Arrange Data',
          choices = c('value', 'sample', 'stdError'),
          selected = 'value'
        )
      })
    }
  })
  # User options gaussian mixture modeling visualization
  output$curves2 <- renderUI({
    checkboxGroupInput(
      'curves2',
      'Choose Density Curve(s)',
      choices = c('norm', 'kernel', 'sumpdf'), inline = TRUE
    )
  })
  # User options gaussian mixture modeling visualization
  output$scalepdf <- renderUI({
    sliderInput(
      'scalepdf',
      'Scale PDF',
      min = 1,
      max = 10,
      value = 3,
      ticks = FALSE
    )
  })
  # This observer handles the plot download
  observe({
    # Needs a filename input by the user
    req(input$filename)
    # Download button
    output$dlplot <-
      # Download the current plot
      downloadHandler(
        filename <- input$filename,
        content <- function(filename) {
          if(input$p.select == 'Explore Data') {
            p <- p1()
          } else {
            p <- p2()
          }
          ggsave(
            filename,
            plot = p,
            width = input$plot_width,
            height = input$plot_height,
            device = input$plot_device
          )
        }
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
