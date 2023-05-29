#!/usr/bin/env Rscript

# Shiny dashboard
body <-
  dashboardBody(
    tabItems(
      tabItem('explore',
        fluidRow(
          box(
            width = 5,
            status = 'primary',
            solidHeader = FALSE,
            rHandsontableOutput(outputId = 'table', height = '41em'),
            'Copy & paste data, right-click to add and delete rows',
            checkboxInput('toggle', 'Toggle All', value = FALSE)
          ),
          # Second column on first row of layout (Plot)
          box(
            width = 7,
            status = 'success',
            solidHeader = FALSE,
            div(
              #HTML style
              style = 'position:relative',
              plotOutput(
                # Plot ID
                outputId = 'p1',
                # Add interactive brushing (window drag for zoom)
                brush = brushOpts(id = 'p1.brush', resetOnNew = FALSE),
                # Add interactive double click to reset zoom on dbl click
                dblclick = 'p1.dbl',
                # Add interactive hover to show x-axis position when hovering over raw data
                hover = hoverOpts('p1.hover', delay = 50, delayType = 'throttle')
              ),
              # HTML output to display box with x-axis position during cursor hovering
              uiOutput('p1.info')
            ),
            'Drag window and double-click to zoom, double-click inside plot to reset',
            selectInput(
              'type1',
              label = 'Plot Type',
              choices = c('point', 'histogram'),
              selected = 'histogram'
            ),
            uiOutput('arrange1'),
            uiOutput('bins1'),
            uiOutput('curves1')
          )
        )
      ),
      tabItem('pkfit',
        fluidRow(
          tabBox(
            width = 5,
            # isoplotR peakfit function parameters
            tabPanel(
              'Peakfit Parameters',
              numericInput(
                'k',
                'Number of Peaks (0 = "auto")',
                value = 0,
                min = 0,
                max = 25,
                step = 1
              ),
              numericInput(
                'sigdig',
                'Significant Digits',
                value = 2,
                min = 1,
                max = 5,
                step = 1
              ),
              selectInput(
                'alpha',
                'Confidence',
                choices = c('90%', '95%', '98%', '99%'),
                selected = '95%'
              ),
              numericInput(
                'sigrange',
                'Sigma Range',
                min = 1,
                max = 5,
                step = 1,
                value = 5
              ),
              numericInput(
                'sigma',
                'Standard Error (# of sigma for input data)',
                min = 1,
                max = 3,
                step = 1,
                value = 1
              ),
              # Check boxes
              actionButton('calc', 'Find Peaks')
            ),
            # isoplotR peakfit function parameters
            tabPanel(
              'Download Plot',
              selectInput(
                'p.select',
                'Select Plot',
                choices = c('Explore Data', 'Gaussian Mixture Model')
              ),
              textInput('filename', 'Filename (no extension)'),
              selectInput(
                'plot_device',
                'Plot Type',
                choices = c('png', 'pdf')
              ),
              numericInput(
                'plot_width',
                'Plot width (inches)',
                min = 5.5,
                max = 17,
                step = 0.25,
                value = 5.5
              ),
              numericInput(
                'plot_height',
                'Plot height (inches)',
                min = 4.25,
                max = 11,
                step = 0.25,
                value = 4.25
              ),
              downloadButton('dlplot', 'Download Plot')
            ),
            tabPanel(
              'Acknowledgements',
              verbatimTextOutput('acknowledgements')
            )
          ),
          # Second column on first row of layout (Plot)
          box(
            width = 7,
            status = 'success',
            solidHeader = FALSE,
            div(
              #HTML style
              style = 'position:relative',
              plotOutput(
                # Plot ID
                outputId = 'p2',
                # Add interactive brushing (window drag for zoom)
                brush = brushOpts(id = 'p2.brush', resetOnNew = FALSE),
                # Add interactive double click to reset zoom on dbl click
                dblclick = 'p2.dbl',
                # Add interactive hover to show x-axis position when hovering over raw data
                hover = hoverOpts('p2.hover', delay = 50, delayType = 'throttle')
              ),
              # HTML output to display box with x-axis position during cursor hovering
              uiOutput('p2.info')
            ),
            'Drag window and double-click to zoom, double-click inside plot to reset',
            uiOutput('curves2'),
            uiOutput('scalepdf'),
            verbatimTextOutput('info', placeholder = FALSE)
          )
        )
      )
    )
  )
