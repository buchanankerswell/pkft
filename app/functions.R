# Functions for raman_hist app
library(tidyverse)
library(shinydashboard)
library(IsoplotR)
library(rhandsontable)
body <-
  dashboardBody(tabItems(
    tabItem('explore',
            fluidRow(
              box(
                width = 5,
                title = 'User Input',
                status = 'primary',
                'Copy & paste data',
                br(),
                'Right-click to add and delete rows',
                solidHeader = TRUE,
                rHandsontableOutput(outputId = 'table', height = "400px"),
                selectInput(
                  'type1',
                  label = 'Plot Type',
                  choices = c('point', 'hist'),
                  selected = 'point'
                ),
                uiOutput('bins1'),
                uiOutput('curves1'),
                checkboxInput('toggle', 'Toggle All', value = FALSE)
              ),
              # Second column on first row of layout (Plot)
              box(
                width = 7,
                title = 'Visualization',
                status = 'success',
                'Drag window and dbl click to zoom',
                br(),
                'Dbl click inside plot to reset',
                solidHeader = TRUE,
                div(
                  #HTML style
                  style = "position:relative",
                  plotOutput(
                    # Plot ID
                    outputId = "p1",
                    # Add interactive brushing (window drag for zoom)
                    brush = brushOpts(id = "p1.brush", resetOnNew = FALSE),
                    # Add interactive double click to reset zoom on dbl click
                    dblclick = "p1.dbl",
                    # Add interactive hover to show x-axis position when hovering over raw data (black curve)
                    hover = hoverOpts("p1.hover", delay = 50, delayType = "throttle")
                  ),
                  # HTML output to display box with x-axis position during cursor hovering
                  uiOutput("p1.info")
                )
              )
            )),
    tabItem('pkfit',
            fluidRow(
              box(
                width = 5,
                title = 'User Input',
                status = 'primary',
                solidHeader = TRUE,
                'Num peaks = 0 = "auto"',
                numericInput(
                  'k',
                  'Number of Peaks',
                  value = 0,
                  min = 0,
                  max = 25,
                  step = 1
                ),
                selectInput(
                  'type2',
                  label = 'Plot Type',
                  choices = c('hist', 'peaks'),
                  selected = 'hist'
                ),
                uiOutput('bins2'),
                uiOutput('curves2'),
                actionButton('calc', 'Find Peaks')
              ),
              # Second column on first row of layout (Plot)
              box(
                width = 7,
                title = 'Visualization',
                status = 'success',
                solidHeader = TRUE,
                'Drag window and dbl click to zoom',
                br(),
                'Dbl click inside plot to reset',
                div(
                  #HTML style
                  style = "position:relative",
                  plotOutput(
                    # Plot ID
                    outputId = "p2",
                    # Add interactive brushing (window drag for zoom)
                    brush = brushOpts(id = "p2.brush", resetOnNew = FALSE),
                    # Add interactive double click to reset zoom on dbl click
                    dblclick = "p2.dbl",
                    # Add interactive hover to show x-axis position when hovering over raw data (black curve)
                    hover = hoverOpts("p2.hover", delay = 50, delayType = "throttle")
                  ),
                  # HTML output to display box with x-axis position during cursor hovering
                  uiOutput("p2.info")
                ),
                verbatimTextOutput('info', placeholder = TRUE)
              )
            )),
    tabItem('options',
            # Second row for float layout (includes columns for all user inputs)
            fluidRow(
              # column of inputs (isoplotR peakfit function parameters)
              box(
                title = 'Peakfit Parameters',
                status = 'primary',
                solidHeader = TRUE,
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
                  value = 3
                ),
                numericInput(
                  'sigma',
                  'meas Errors (# of sigma)',
                  min = 1,
                  max = 3,
                  step = 1,
                  value = 1
                ),
                # Check boxes
                checkboxInput('log', 'Log-Transform Data', value = FALSE)
              ),
              # Fourth column of inputs (Download plot configurations)
              box(
                title = 'Configure Download',
                status = 'primary',
                solidHeader = TRUE,
                selectInput(
                  'p.select',
                  'Select Plot',
                  choices = c('Explore Data', 'Gaussian Mixture Model')
                ),
                textInput('filename',
                          'Filename (no extension)'),
                numericInput(
                  'pdf_width',
                  'Plot width (inches)',
                  min = 5.5,
                  max = 17,
                  step = 0.25,
                  value = 5.5
                ),
                numericInput(
                  'pdf_height',
                  'Plot height (inches)',
                  min = 4.25,
                  max = 11,
                  step = 0.25,
                  value = 4.25
                ),
                downloadButton('dlplot',
                               'Get Plot')
              )
            )),
    tabItem('ack',
            fluidRow(
              box(
                width = 12,
                status = 'primary',
                solidHeader = TRUE,
                verbatimTextOutput('acknowledgements')
              )
            ))
  ))
pkfit <-
  function(data,
           k = 0,
           sigerror = 1,
           sigdig = 3,
           sigrange = 3,
           log = FALSE,
           alpha = '95%') {
    # If the two-sigma box is checked, divide the errors input into the second column of the
    # input table by two. This is because 'peakfit' accepts 1sigma errors as an argument
    if (k == 0) {
      k <- 'auto'
    } else {
      k <- k
    }
    # Set alpha (confidence interval) based on user input
    if (alpha == '90%') {
      alpha = 0.10
    } else if (alpha == '95%') {
      alpha = 0.05
    } else if (alpha == '98%') {
      alpha = 0.02
    } else if (alpha == '99%') {
      alpha = 0.01
    }
    d <- data %>% drop_na()
    d <- d %>%
      filter(toggle == TRUE) %>%
      select(-toggle) %>%
      mutate('sig' = sig / sigerror)
    # Find peaks
    peaks <-
      # Try peakfit function with the current arguments
      try(peakfit(
        as.matrix(d),
        k = k,
        sigdig = sigdig,
        log = log,
        alpha = alpha
      ))
    # If peakfit throws a try-error, then peaks = NULL
    if (is(peaks, "try-error")) {
      pks <- NULL
    } else {
      BIC <- (ncol(peaks$peaks)*log(nrow(d)))-(2*peaks$L)
      legend <- peaks$legend %>%
        as_tibble(rownames = 'peak') %>%
        rename(legend = value) %>%
        group_by(peak)
      props <- peaks$props %>%
        as_tibble(rownames = 'stat') %>%
        pivot_longer(-stat, names_to = 'peak', values_to = 'val') %>%
        group_by(peak) %>%
        replace(is.na(.), 0) %>%
        nest() %>%
        rename(props = data)
      pks <- peaks$peaks %>%
        as_tibble(rownames = 'stat') %>%
        pivot_longer(-stat, names_to = 'peak', values_to = 'val') %>%
        group_by(peak) %>%
        replace(is.na(.), 0) %>%
        nest() %>%
        rename(est = data) %>%
        left_join(legend) %>%
        left_join(props) %>%
        add_column(BIC = BIC, .after = 'legend') %>%
        mutate(d.norm = est %>% purrr::map(
          ~ .x %>%
            pivot_wider(names_from = stat, values_from = val) %>%
            purrr::pmap_df( ~ tibble(
              x = seq(.x - sigrange * .y, .x + sigrange * .y, .y / 10),
              d = dnorm(
                seq(.x - sigrange * .y, .x + sigrange * .y, .y / 10),
                mean = .x,
                sd = .y
              )
            ))
        )) %>%
        mutate('d.norm' = purrr::map2(
          d.norm,
          props %>% purrr::map( ~ .x %>% pivot_wider(
            names_from = stat, values_from = val
          )),
          ~ .x %>% mutate('d' = d * .y$p)
        ))
    }
    return(list(data = d, pks = pks))
  }
p.data <-
  function(data,
           pks = NULL,
           bins = 10,
           sigrange = 3,
           sigerror = 1,
           xlim = NULL,
           ylim = NULL,
           type = c('hist',
                    'point',
                    'peaks',
                    'histpeaks',
                    'histsum',
                    'histsumstack'),
           curves = c('norm', 'sumpdf')) {
    if(nrow(data) == 0){stop('No data')}
    d <- data %>%
      drop_na() %>%
      mutate(
        d.norm = purrr::map2(
          data$meas,
          data$sig,
          ~ tibble(
            x = seq(
              mean(.x, na.rm = TRUE) - sigrange * .y,
              mean(.x, na.rm = TRUE) + sigrange * .y,
              .y / 10
            ),
            d = dnorm(
              seq(
                mean(.x, na.rm = TRUE) - sigrange * .y,
                mean(.x, na.rm = TRUE) + sigrange * .y,
                .y / 10
              ),
              mean = .x,
              sd = .y
            ) / length(data$meas)
          )
        ),
        d.sum = purrr::map2(data$meas, data$sig, ~ tibble(
          x = seq(
            min(data$meas, na.rm = TRUE) - sigrange * mean(data$sig, na.rm = TRUE),
            max(data$meas, na.rm = TRUE) + sigrange * mean(data$sig, na.rm = TRUE),
            mean(data$sig, na.rm = TRUE) / 15
          ),
          d = dnorm(
            seq(
              min(data$meas, na.rm = TRUE) - sigrange * mean(data$sig, na.rm = TRUE),
              max(data$meas, na.rm = TRUE) + sigrange * mean(data$sig, na.rm = TRUE),
              mean(data$sig, na.rm = TRUE) / 15
            ),
            mean = .x,
            sd = .y
          ) / length(data$meas)
        ))
      )
    if(nrow(d) == 1){
      d.n <- NULL
    } else {
      d.n <- tibble(
        x = seq(
          min(d$meas, na.rm = TRUE) - sigrange * mean(d$sig, na.rm = TRUE),
          max(d$meas, na.rm = TRUE) + sigrange * mean(d$sig, na.rm = TRUE),
          sd(d$meas, na.rm = TRUE) / 10
        ),
        d = dnorm(
          seq(
            min(d$meas, na.rm = TRUE) - sigrange * mean(d$sig, na.rm = TRUE),
            max(d$meas, na.rm = TRUE) + sigrange * mean(d$sig, na.rm = TRUE),
            sd(d$meas, na.rm = TRUE) / 10
          ),
          mean = mean(d$meas, na.rm = TRUE),
          sd = sd(d$meas, na.rm = TRUE)
        )
      )
    }
    d.s <- d$d.sum %>%
      bind_rows(.id = 'm') %>%
      group_by(x) %>%
      summarise(sumd = sum(d, na.rm = TRUE))
    d.pks <- d$d.norm %>%
      bind_rows(.id = 'm') %>%
      group_by(m)
    if (type == 'hist') {
      p <- d %>%
        ggplot() +
        geom_histogram(
          aes(x = meas, y = ..density..),
          bins = bins,
          color = 'white',
          alpha = 0.25
        ) +
        geom_rug(aes(x = meas)) +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        xlab('Measured Value') +
        ylab('Density') +
        ggtitle('Histogram with density functions') +
        scale_color_gradient(
          name = 'meas',
          low = 'black',
          high = 'palegreen2',
          guide = 'legend'
        ) +
        scale_fill_gradient(
          name = 'meas',
          low = 'black',
          high = 'palegreen2',
          guide = 'legend'
        ) +
        theme_grey(base_size = 14)
      if('indv' %in% curves) {
        p <- p + geom_area(
          data = d.pks,
          aes(
            x = x,
            y = d,
            fill = as.numeric(m),
            group = m
          ),
          color = 'black',
          alpha = 0.5,
          position = 'identity',
          show.legend = FALSE
        )
      }
      if('norm' %in% curves) {
        p <- p + geom_area(data = d.n, aes(x = x, y = d), fill = 'black', color = 'black', alpha = 0.1, position = 'identity')
      }
      if('sumpdf' %in% curves) {
        p <- p + geom_area(data = d.s, aes(x = x, y = sumd), fill = 'thistle4', color = 'black', alpha = 0.1, position = 'identity')
      }
      if('stackpdf' %in% curves) {
        p <- p + geom_area(
          data = d$d.sum %>% bind_rows(.id = 'm') %>% group_by(m),
          aes(
            x = x,
            y = d,
            fill = as.numeric(m),
            group = m
          ),
          color = 'black',
          alpha = 0.5,
          position = 'stack',
          show.legend = FALSE
        )
      }
      if('kernel' %in% curves) {
        p <- p + geom_density(data = d, aes(x = meas, y = ..density..), color = 'black', fill = 'thistle4', alpha = 0.1)
      }
    } else if (type == 'point') {
      p <- d %>%
        ggplot() +
        geom_point(
          data = data,
          aes(
            x = 1:length(meas),
            y = meas
          ),
          size = 0.75,
          show.legend = FALSE,
          color = 'black'
        ) +
        geom_errorbar(aes(
          x = 1:length(meas),
          ymin = meas - sig,
          ymax = meas + sig
        )) +
        ggtitle(bquote(paste(
          'Data with \U00B1 ', .(sigerror), ' sigma uncertainty'
        ))) +
        xlab('') +
        ylab('Measured Value') +
        scale_fill_gradient(
          name = 'Peak',
          low = 'black',
          high = 'palegreen2',
          guide = 'legend',
          breaks = 1:length(pks)
        ) +
        guides(fill = guide_legend(override.aes = list(alpha = 1))) +
        coord_cartesian(xlim = xlim,
                        ylim = ylim) +
        scale_x_continuous(breaks = seq_along(d$meas)) +
        theme_grey(base_size = 14) +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())
      if(!is.null(pks)) {
        stp <- pks$est %>% 
          bind_rows(.id = 'pk') %>%
          group_by(pk) %>%
          pivot_wider(names_from = stat, values_from = val) %>%
          select(t, `s[t]`) %>%
          nest()
        p.stp <- stp$data %>%
          bind_rows(.id = 'pk') %>%
          mutate('pk' = as.numeric(pk)) %>%
          purrr::pmap( ~ geom_rect(
            aes(
              ymin = ..2 - ..3 * sigrange,
              ymax = ..2 + ..3 * sigrange,
              xmin = 1,
              xmax = length(d$meas),
              fill = ..1
            ),
            alpha = 0.005
          ))
        p <- p + p.stp
      }
    }
    return(p)
  }
p.pks <-
  function(pks,
           bins = 10,
           sigrange = 3,
           xlim = NULL,
           ylim = NULL,
           type = c('hist', 'peaks'),
           curves = c('norm', 'sumpdf', 'kernel')) {
    d <- pks$data %>%
      drop_na() %>%
      mutate(
        d.norm = purrr::map2(
          pks$data$meas,
          pks$data$sig,
          ~ tibble(
            x = seq(
              mean(.x, na.rm = TRUE) - sigrange * .y,
              mean(.x, na.rm = TRUE) + sigrange * .y,
              .y / 10
            ),
            d = dnorm(
              seq(
                mean(.x, na.rm = TRUE) - sigrange * .y,
                mean(.x, na.rm = TRUE) + sigrange * .y,
                .y / 10
              ),
              mean = .x,
              sd = .y
            ) / length(pks$data$meas)
          )
        ),
        d.sum = purrr::map2(pks$data$meas, pks$data$sig, ~ tibble(
          x = seq(
            min(pks$data$meas, na.rm = TRUE) - sigrange * mean(pks$data$sig, na.rm = TRUE),
            max(pks$data$meas, na.rm = TRUE) + sigrange * mean(pks$data$sig, na.rm = TRUE),
            mean(pks$data$sig, na.rm = TRUE) / 15
          ),
          d = dnorm(
            seq(
              min(pks$data$meas, na.rm = TRUE) - sigrange * mean(pks$data$sig, na.rm = TRUE),
              max(pks$data$meas, na.rm = TRUE) + sigrange * mean(pks$data$sig, na.rm = TRUE),
              mean(pks$data$sig, na.rm = TRUE) / 15
            ),
            mean = .x,
            sd = .y
          ) / length(pks$data$meas)
        ))
      )
    d.n <- tibble(
      x = seq(
        min(d$meas, na.rm = TRUE) - sigrange * mean(d$sig, na.rm = TRUE),
        max(d$meas, na.rm = TRUE) + sigrange * mean(d$sig, na.rm = TRUE),
        sd(d$meas, na.rm = TRUE) / 10
      ),
      d = dnorm(
        seq(
          min(d$meas, na.rm = TRUE) - sigrange * mean(d$sig, na.rm = TRUE),
          max(d$meas, na.rm = TRUE) + sigrange * mean(d$sig, na.rm = TRUE),
          sd(d$meas, na.rm = TRUE) / 10
        ),
        mean = mean(d$meas, na.rm = TRUE),
        sd = sd(d$meas, na.rm = TRUE)
      )
    )
    d.s <- d$d.sum %>%
      bind_rows(.id = 'm') %>%
      group_by(x) %>%
      summarise(sumd = sum(d, na.rm = TRUE))
    d.pks <- pks$pks$d.norm %>%
      bind_rows(.id = 'pk') %>%
      group_by(pk)
    if (type == 'hist') {
      p <- d %>%
        ggplot() +
        geom_histogram(
          aes(x = meas, y = ..density..),
          bins = bins,
          color = 'white',
          alpha = 0.25
        ) +
        geom_rug(aes(x = meas)) +
        geom_area(
          data = d.pks,
          aes(
            x = x,
            y = d,
            fill = as.numeric(pk),
            group = pk
          ),
          color = 'black',
          alpha = 0.5,
          position = 'identity'
        ) +
        xlab('Measured Value') +
        ylab('Density') +
        ggtitle('Finite Gaussian Mixture') +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        scale_fill_gradient(
          name = 'Peak',
          low = 'black',
          high = 'palegreen2',
          guide = 'legend',
          breaks = seq_along(d$meas)
        ) +
        guides(fill = guide_legend(override.aes = list(alpha = 1))) +
        theme_grey(base_size = 14)
      if('kernel' %in% curves) {
        p <- p + geom_density(data = d, aes(x = meas, y = ..density..), color = 'black', fill = 'thistle4', alpha = 0.1)
      }
      if('sumpdf' %in% curves) {
        p <- p + geom_area(data = d.s, aes(x = x, y = sumd), fill = 'thistle4', color = 'black', alpha = 0.1, position = 'identity')
      }
      if('norm' %in% curves) {
        p <- p + geom_area(data = d.n, aes(x = x, y = d), fill = 'black', color = 'black', alpha = 0.1, position = 'identity')
      }
    } else if (type == 'peaks') {
      p <- d.pks %>%
        ggplot() +
        geom_area(
          aes(
            x = x,
            y = d,
            fill = as.numeric(pk),
            group = pk
          ),
          color = 'black',
          alpha = 0.5,
          position = 'identity'
        ) +
        xlab('Measured Value') +
        ylab('Density') +
        ggtitle('Finite Gaussian Mixture') +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        scale_fill_gradient(
          name = 'Peak',
          low = 'black',
          high = 'palegreen2',
          guide = 'legend',
          breaks = seq_along(d$meas)
        ) +
        guides(fill = guide_legend(override.aes = list(alpha = 1))) +
        theme_grey(base_size = 14)
      if('kernel' %in% curves) {
        p <- p + geom_density(data = d, aes(x = meas, y = ..density..), color = 'black', fill = 'thistle4', alpha = 0.1)
      }
      if('sumpdf' %in% curves) {
        p <- p + geom_area(data = d.s, aes(x = x, y = sumd), fill = 'thistle4', color = 'black', alpha = 0.1, position = 'identity')
      }
      if('norm' %in% curves) {
        p <- p + geom_area(data = d.n, aes(x = x, y = d), fill = 'black', color = 'black', alpha = 0.1, position = 'identity')
      }
    }
    return(p)
  }
