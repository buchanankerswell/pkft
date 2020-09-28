source('functions.R')
# options(shiny.reactlog = FALSE)
options(shiny.fullstacktrace = TRUE)

# Define UI for application
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
  vals$pks <- NA
  vals$x1 <- NULL
  vals$y1 <- NULL
  vals$x2 <- NULL
  vals$y2 <- NULL
  # Raw data
  vals$data <-
    tibble(
      measurement = c(
        17.118,
        17.040,
        17.032,
        17.024,
        17.020,
        17.009,
        16.989,
        16.972,
        16.969,
        16.942,
        16.929,
        16.922
      ),
      sde = c(
        0.155,
        0.022,
        0.039,
        0.224,
        0.015,
        0.031,
        0.089,
        0.099,
        0.041,
        0.041,
        0.041,
        0.087
      )
    ) %>%
    mutate(
      toggle = rep(TRUE, length(measurement))
    )
  # Generates interactive table populated with data from vals$data
  output$table <- renderRHandsontable({
    rhandsontable(vals$data, stretchH = 'all') %>%
      hot_cols(columnSorting = TRUE)
  })
  observe({
    req(input$sigma)
    req(input$k)
    if (!is.null(input$table)) {
      vals$data <- hot_to_r(input$table)
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
    }
  })
  output$p1 <- renderPlot({
    req(!is.null(vals$data))
    d <- vals$data %>% drop_na() %>% filter(toggle == TRUE)
    p1 <<- p.data(
      d,
      type = input$type1,
      bins = input$bins,
      xlim = vals$x1,
      ylim = vals$y1,
      sigrange = input$sigrange,
      sigerror = input$sigma
    )
    p1
  })
  output$p2 <- renderPlot({
    req(!is.na(vals$pks))
    req(input$bins)
    req(input$sigrange)
    req(input$type2)
    p2 <<- try(
      p.pks(
        vals$pks,
        bins = input$bins2,
        sigrange = input$sigrange,
        xlim = vals$x2,
        ylim = vals$y2,
        type = input$type2
      )
    )
    if (is(p.pks, "try-error")) {
     p2 <<- NULL
    } else {
      p2
    }
  })
  output$info <- renderText({
    req(vals$pks)
    paste0(
      ' Peak   |  Pos. | StdE  |  Proportion',
      '\n ',
      paste(vals$pks$pks$legend, collapse = '\n ')
    )
  })
  output$acknowledgements <- renderText({
    paste('This app was built using the IsoplotR package:\nLudwig, K.R., 1998. On the treatment of concordant uranium-lead ages. Geochimica et Cosmochimica Acta, 62(4), pp.665-676.')
  })
  observeEvent(input$type1, {
    vals$x1 <- NULL
    vals$y1 <- NULL
  })
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
                            p <- p1
                          } else {
                            p <- p2
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
shinyApp(ui = ui, server = server, options = 'quiet')

