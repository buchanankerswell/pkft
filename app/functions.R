#!/usr/bin/env Rscript

# Load packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(IsoplotR)
library(rhandsontable)

# Extract peakfit legend info
extract_pkfit_legend_info <- function(input_string) {
  # Extract peak number
  peak_number <- str_extract(input_string, '\\d+')
  # Extract value and uncertainty
  value_uncertainty <- str_extract_all(input_string, '\\d+\\.\\d+')
  value <- as.numeric(value_uncertainty[[1]][1])
  uncertainty <- as.numeric(value_uncertainty[[1]][2])
  # Extract proportion
  proportion <- str_extract(input_string, '\\d+\\.\\d+')
  # Return the extracted values as a vector
  paste0(as.integer(peak_number), ',', value, ',', uncertainty)
}

# Wrapper for IsoplotR::peakfit
pkfit <-
  function(
    data,
    k = 0,
    sigerror = 1,
    sigdig = 3,
    sigrange = 3,
    alpha = '95%'
  ) {
    # Let k be chosen by BIC if not set
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
    # Drop missing data
    d <- data %>% drop_na()
    # Drop untoggled data and make sure sig is 1sigma for peakfit
    d <-
      d %>%
      filter(toggle == TRUE) %>%
      select(-toggle) %>%
      mutate('stdError' = stdError / sigerror)
    # Find peaks
    # Try peakfit function with the current arguments
    peaks <-
      try(
        peakfit(
          as.matrix(d[c('value', 'stdError')]),
          k = k,
          sigdig = sigdig,
          log = FALSE,
          alpha = alpha
        )
      )
    # If peakfit throws a try-error, then peaks = NULL
    if (is(peaks, 'try-error')) {
      pks <- NULL
    } else {
      # Extract legend info
      legend <-
        as.character(peaks$legend) %>%
        as_tibble(rownames = 'peak') %>%
        mutate(value = purrr::map_chr(value, ~extract_pkfit_legend_info(.x))) %>%
        rename(legend = value) %>%
        group_by(peak)
      # Get peak proportions
      props <-
        peaks$props %>%
        as_tibble(rownames = 'stat') %>%
        pivot_longer(-stat, names_to = 'peak', values_to = 'val') %>%
        group_by(peak) %>%
        replace(is.na(.), 0) %>%
        nest() %>%
        rename(props = data)
      # Combine IsoplotR::peakfit output into a single tibble
      pks <-
        peaks$peaks %>%
        as_tibble(rownames = 'stat') %>%
        pivot_longer(-stat, names_to = 'peak', values_to = 'val') %>%
        group_by(peak) %>%
        replace(is.na(.), 0) %>%
        nest() %>%
        rename(est = data) %>%
        left_join(legend, by = 'peak') %>%
        left_join(props, by = 'peak') %>%
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

# Plot data
p.data <-
  function(
    data,
    pks = NULL,
    bins = 30,
    sigrange = 3,
    sigerror = 1,
    xlim = NULL,
    ylim = NULL,
    type = c('histogram', 'point'),
    arrange = c('value', 'sample', 'stdError'),
    curves = c('norm', 'kernel', 'sumpdf', 'stackpdf', 'indv')
  ) {
    if(nrow(data) == 0){stop('No data')}
    d <-
      as_tibble(data) %>%
      drop_na() %>%
      mutate(
        d.norm = purrr::map2(
          data$value,
          data$stdError,
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
            ) / length(data$value)
          )
        ),
        d.sum = purrr::map2(data$value, data$stdError, ~ tibble(
          x = seq(
            min(data$value, na.rm = TRUE) - sigrange * mean(data$stdError, na.rm = TRUE),
            max(data$value, na.rm = TRUE) + sigrange * mean(data$stdError, na.rm = TRUE),
            mean(data$stdError, na.rm = TRUE) / 15
          ),
          d = dnorm(
            seq(
              min(data$value, na.rm = TRUE) - sigrange * mean(data$stdError, na.rm = TRUE),
              max(data$value, na.rm = TRUE) + sigrange * mean(data$stdError, na.rm = TRUE),
              mean(data$stdError, na.rm = TRUE) / 15
            ),
            mean = .x,
            sd = .y
          ) / length(data$value)
        ))
      )
    if(nrow(d) == 1){
      d.n <- NULL
    } else {
      d.n <- tibble(
        x = seq(
          min(d$value, na.rm = TRUE) - sigrange * mean(d$stdError, na.rm = TRUE),
          max(d$value, na.rm = TRUE) + sigrange * mean(d$stdError, na.rm = TRUE),
          sd(d$value, na.rm = TRUE) / 10
        ),
        d = dnorm(
          seq(
            min(d$value, na.rm = TRUE) - sigrange * mean(d$stdError, na.rm = TRUE),
            max(d$value, na.rm = TRUE) + sigrange * mean(d$stdError, na.rm = TRUE),
            sd(d$value, na.rm = TRUE) / 10
          ),
          mean = mean(d$value, na.rm = TRUE),
          sd = sd(d$value, na.rm = TRUE)
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
    if (type == 'histogram') {
      p <- d %>%
        ggplot() +
        geom_histogram(
          aes(x = value, y = after_stat(density)),
          bins = bins,
          fill = NA,
          color = 'grey10'
        ) +
        geom_rug(aes(x = value)) +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        labs(x = 'Value', y = 'Count/Density', fill = 'Peak') +
        scale_fill_gradient(low = 'black', high = 'palegreen2') +
        theme_bw(base_size = 14) +
        theme(
          plot.title = element_text(size = 32, face = 'bold'),
          plot.margin = margin(10, 10, 10, 10),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      if('norm' %in% curves) {
        p <- p + geom_area(
          data = d.n,
          aes(x = x, y = d),
          fill = 'black',
          color = 'black',
          alpha = 0.1,
          position = 'identity'
        )
      }
      if('kernel' %in% curves) {
        p <- p + geom_density(
          data = d,
          aes(x = value, y = after_stat(density)),
          fill = 'black',
          color = 'black',
          alpha = 0.1,
          position = 'identity'
        )
      }
      if('sumpdf' %in% curves) {
        p <- p + geom_area(
          data = d.s,
          aes(x = x, y = sumd),
          fill = 'black',
          color = 'black',
          alpha = 0.1,
          position = 'identity'
        )
      }
      if('stackpdf' %in% curves) {
        p <- p + geom_area(
          data = d$d.sum %>% bind_rows(.id = 'm') %>% group_by(m),
          aes(x = x, y = d, fill = as.numeric(m), group = m),
          color = 'black',
          alpha = 0.1,
          position = 'stack',
          show.legend = FALSE
        )
      }
      if('indv' %in% curves) {
        p <- p + geom_area(
          data = d.pks,
          aes(x = x, y = d, fill = as.numeric(m), group = m),
          color = 'black',
          alpha = 0.1,
          position = 'identity',
          show.legend = FALSE
        )
      }
    } else if (type == 'point') {
      p <-
        d %>%
        ggplot() +
        geom_point(
          aes(
            y = reorder(sample, .data[[arrange]]),
            x = value
          ),
          show.legend = FALSE,
          color = 'black'
        ) +
        geom_errorbar(
          aes(
            y = reorder(sample, .data[[arrange]]),
            xmin = value - stdError,
            xmax = value + stdError
          )
        ) +
        labs(y = 'Sample', x = 'Value') +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        theme_bw(base_size = 14) +
        theme(
          plot.margin = margin(10, 10, 10, 10),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1)
        )
    }
    return(p)
  }

# Plot GMM
p.pks <-
  function(
    pks,
    bins = 30,
    sigrange = 3,
    xlim = NULL,
    ylim = NULL,
    curves = c('norm', 'kernel', 'sumpdf'),
    scalepdf = 3
  ) {
    d <- pks$data %>%
      drop_na() %>%
      mutate(
        d.norm = purrr::map2(
          pks$data$value,
          pks$data$stdError,
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
            ) / length(pks$data$value)
          )
        ),
        d.sum = purrr::map2(pks$data$value, pks$data$stdError, ~ tibble(
          x = seq(
            min(pks$data$value, na.rm = TRUE) - sigrange * mean(pks$data$stdError, na.rm = TRUE),
            max(pks$data$value, na.rm = TRUE) + sigrange * mean(pks$data$stdError, na.rm = TRUE),
            mean(pks$data$stdError, na.rm = TRUE) / 15
          ),
          d = dnorm(
            seq(
              min(pks$data$value, na.rm = TRUE) - sigrange * mean(pks$data$stdError, na.rm = TRUE),
              max(pks$data$value, na.rm = TRUE) + sigrange * mean(pks$data$stdError, na.rm = TRUE),
              mean(pks$data$stdError, na.rm = TRUE) / 15
            ),
            mean = .x,
            sd = .y
          ) / length(pks$data$value)
        ))
      )
    d.n <- tibble(
      x = seq(
        min(d$value, na.rm = TRUE) - sigrange * mean(d$stdError, na.rm = TRUE),
        max(d$value, na.rm = TRUE) + sigrange * mean(d$stdError, na.rm = TRUE),
        sd(d$value, na.rm = TRUE) / 10
      ),
      d = dnorm(
        seq(
          min(d$value, na.rm = TRUE) - sigrange * mean(d$stdError, na.rm = TRUE),
          max(d$value, na.rm = TRUE) + sigrange * mean(d$stdError, na.rm = TRUE),
          sd(d$value, na.rm = TRUE) / 10
        ),
        mean = mean(d$value, na.rm = TRUE),
        sd = sd(d$value, na.rm = TRUE)
      )
    )
    d.s <- d$d.sum %>%
      bind_rows(.id = 'm') %>%
      group_by(x) %>%
      summarise(sumd = sum(d, na.rm = TRUE))
    d.pks <- pks$pks$d.norm %>%
      bind_rows(.id = 'pk') %>%
      group_by(pk)
    p <- d %>%
      ggplot() +
      geom_histogram(
        aes(x = value, y = after_stat(density)),
        bins = bins,
        fill = NA,
        color = 'grey10'
      ) +
      geom_rug(aes(x = value)) +
      geom_area(
        data = d.pks,
        aes(
          x = x,
          y = d/scalepdf,
          fill = as.numeric(pk),
          group = pk
        ),
        color = 'black',
        alpha = 0.5,
        position = 'identity'
      ) +
      scale_fill_gradient(
        name = 'Peak',
        low = 'black',
        high = 'palegreen2',
        guide = 'legend',
        breaks = seq_along(d$value)
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 1))) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      labs(x = 'Value', y = 'Count/Density', fill = 'Peak') +
      theme_bw(base_size = 14) +
      theme(
        plot.title = element_text(size = 32, face = 'bold'),
        plot.margin = margin(10, 10, 10, 10),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'bottom'
      )
    if('norm' %in% curves) {
      p <- p + geom_area(
        data = d.n,
        aes(x = x, y = d),
        fill = 'black',
        color = 'black',
        alpha = 0.1,
        position = 'identity'
      )
    }
    if('kernel' %in% curves) {
      p <- p + geom_density(
        data = d,
        aes(x = value, y = after_stat(density)),
        color = 'black',
        fill = 'black',
        alpha = 0.1
      )
    }
    if('sumpdf' %in% curves) {
      p <- p + geom_area(
        data = d.s,
        aes(x = x, y = sumd),
        fill = 'black',
        color = 'black',
        alpha = 0.1,
        position = 'identity'
      )
    }
    return(p)
  }
