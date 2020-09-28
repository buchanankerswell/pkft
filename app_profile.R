library(profvis)
library(htmlwidgets)

# Profvis is a neat tool to visualize the memory and run time for every function call
# Run this script, then play a little with the app, then exit the app. A Profvis HTML
# file will be created, which gives an interactive look at the function call stack

# Read more here: https://rstudio.github.io/profvis/examples.html?_ga=2.30410495.1928094711.1593192414-999296703.1588223349#example-3---profiling-a-shiny-application
p <- profvis({runApp()})
# Save profile to html widget
saveWidget(p, "profile.html")
# Open html widget
browseURL("profile.html")
