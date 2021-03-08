source('functions.R')
options(shiny.reactlog = FALSE)
options(shiny.fullstacktrace = TRUE)

# Define UI for application
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = 'Peakfit Explorer'),
  dashboardSidebar(sidebarMenu(
    menuItem(
      'Explore Data',
      tabName = 'explore',
      icon = icon('think-peaks')
    ),
    menuItem(
      'Gaussian Mixture Model',
      tabName = 'pkfit',
      icon = icon('mortar-pestle')
    ),
    menuItem('Options',
             tabName = 'options',
             icon = icon('sliders-h')),
    menuItem('Acknowledgements',
             tabName = 'ack',
             icon = icon('book-open'))
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
  vals$data <- tibble(meas = rep(NA_real_, 15), sig = rep(NA_real_, 15), toggle = rep(FALSE, 15))
  # Generates interactive table populated with data from vals$data
  output$table <- renderRHandsontable({
    d <- vals$data
    rhandsontable(d, stretchH = 'all') %>%
      hot_cols(columnSorting = TRUE)
  })
  observe({
    if (!is.null(input$table)) {vals$data <- hot_to_r(input$table)} 
  })
  # This observer finds peaks when the find peaks button is pressed
  observeEvent(input$calc, {
    req(input$sigma)
    req(input$k)
    d <- vals$data %>% drop_na() %>% filter(toggle == TRUE)
      vals$pks <- pkfit(
        d,
        k = input$k,
        sigerror = input$sigma,
        sigdig = input$sigdig,
        sigrange = input$sigrange,
        log = input$log,
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
        d,
        type = input$type1,
        bins = input$bins1,
        xlim = vals$x1,
        ylim = vals$y1,
        sigrange = input$sigrange,
        sigerror = input$sigma,
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
    req(input$bins2)
    req(input$sigrange)
    req(input$type2)
    p <- try(
      p.pks(
        vals$pks,
        bins = input$bins2,
        sigrange = input$sigrange,
        xlim = vals$x2,
        ylim = vals$y2,
        type = input$type2,
        curves = input$curves2
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
    info <- try(paste0(
      ' Peak   |  Pos. | StdE  |  Proportion',
      '\n ',
      paste(vals$pks$pks$legend, collapse = '\n '),
      '\n ',
      'Bayesian Information Criterion: ',
      round(vals$pks$pks$BIC[[1]], 2)
    ))
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
  # Render acknoledgments verbatim text output
  output$acknowledgements <- renderText({
    paste('This app was built using the IsoplotR package:\nVermeesch, P. (2018). IsoplotR: a free and open toolbox for geochronology. Geoscience Frontiers, 9, 1479-1493. doi: 10.1016/j.gsf.2018.04.001.\nInitial dataset from:\nTrayler, R.B., Schmitz, M.D., Cuitiño, J.I., Kohn, M.J., Bargo, M.S., Kay, R.F., Strömberg, C.A. and Vizcaíno, S.F., 2020. An improved approach to age-modeling in deep time: Implications for the Santa Cruz Formation, Argentina. GSA Bulletin, 132(1-2)')
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
    if (type == 'hist') {
      output$curves1 <- renderUI({
        checkboxGroupInput(
          'curves1',
          'Choose Curves',
          choices = c('norm', 'kernel', 'sumpdf', 'stackpdf', 'indv'),
          inline = TRUE
        )
      })
      output$bins1 <- renderUI({
        sliderInput(
          'bins1',
          'Bins',
          min = 1,
          max = 50,
          value = 10,
          ticks = FALSE
        )
      })
    } else {
      output$curves1 <- NULL
      output$bins1 <- NULL
    }
  })
  # Output different user options depending on the plot type
  observeEvent(input$type2, {
    type <- input$type2
    if(type == 'hist') {
      output$bins2 <- renderUI({
        sliderInput(
          'bins2',
          'Bins',
          min = 1,
          max = 50,
          value = 10,
          ticks = FALSE
        )
      })
      output$curves2 <- renderUI({
        checkboxGroupInput('curves2', 'Choose Curves',
                           choices = c('norm', 'sumpdf', 'kernel'), inline = TRUE)
      })
    } else {
      output$curves2 <- renderUI({
        checkboxGroupInput('curves2', 'Choose Curves',
                           choices = c('norm', 'sumpdf', 'kernel'), inline = TRUE)
      })
      output$bins2 <- NULL
    }
  })
  # This observer handles the plot download
  observe({
    # Needs a filename input by the user
    req(input$filename)
    # Download button
    output$dlplot <-
      # Download the current plot as a pdf, with a filename, width, and height defined
      # by user inputs
      downloadHandler(filename <- paste0(input$filename, '.pdf'),
                      content <-
                        function(filename) {
                          if(input$p.select == 'Explore Data') {
                            p <- p1()
                          } else {
                            p <- p2()
                          }
                          ggsave(
                            filename,
                            plot = p,
                            device = pdf(
                              width = input$pdf_width,
                              height = input$pdf_height
                            )
                          )
                        })
  })
}
# Run the application
shinyApp(ui = ui, server = server)

