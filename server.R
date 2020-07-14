# Server ====
server <- function(input, output, session){
  output$ggpairs_plot <- renderPlot({
    # ggpairs(df[-c(1,2,3,4)], mapping = ggplot2::aes(color = L_3_FACIES))
    chart.Correlation(df[-c(1,2,3,4,5)], histogram = TRUE, pch = 21)
  })
  
  output$plot_hist <- renderPlot({
    if (startsWith(input$prop, "L_") == TRUE){
      barplot(table(df[,input$prop]), main = paste("Histogram of", input$prop),
              xlab = input$prop)
    } else {
      chart.Histogram(df[,input$prop], main = paste("Histogram of", input$prop),
                      xlab = input$prop)
    }
  })
  
  output$plot_raw <- renderPlot({
    if (startsWith(input$prop, "L_") == TRUE){
      plot.new()
    } else {
      boxplot(df[,input$prop], outcol = "red", names = input$prop,
              main=paste("Boxplot of", input$prop))
    }
  })
  
  output$plot_log10 <- renderPlot({
    if (startsWith(input$prop, "L_") == TRUE){
      plot.new()
    } else {
      boxplot(log10(df[,input$prop]), outcol = "red", names = input$prop,
              main=paste("Boxplot of", input$prop, "- log transformed"))
    }
  })
  
  output$variogram <- renderPlot({
    newdf <- log_transformation(df, input$property, input$log_transform)
    newdf <- outlier_filter(newdf, input$property, input$outlier_filter)
    bounds <- rounding(newdf, input$xlon, input$ylat)
    variograms <- create_vario(newdf, input$geo.surface, input$xlon, input$ylat, input$property)
    
    print(typeof(input$outlier_filter))
    print(input$outlier_filter)
    print(typeof(input$log_transform))
    print(input$log_transform)
    
    switch(input$dir,
           "omni" = plot(variograms[[1]], type = "p", pch = 19, npairpt= T, npairdw= F,
                         title = paste(input$property, "Experimental Omnidirectional Variogram"),
                         pos.legend = 1, cex = 0.7,
                         xlab = "Log distance",
                         ylab = expression(paste("Variance (", gamma, "(h))", sep=""))),
           "2dir" = plot(variograms[[2]], type = "p", pch = 19, npairpt= T, npairdw= F,
                         title = paste(input$property, "Experimental 2 Directional Variogram"),
                         pos.legend = 1, cex = 0.7,
                         xlab = "Log distance",
                         ylab = expression(paste("Variance (", gamma, "(h))", sep=""))),
           "3dir" = plot(variograms[[3]], type = "p", pch = 19, npairpt= T, npairdw= F,
                         title = paste(input$property, "Experimental 3 Directional Variogram"),
                         pos.legend = 1, cex = 0.7,
                         xlab = "Log distance",
                         ylab = expression(paste("Variance (", gamma, "(h))", sep=""))),
           "4dir" = plot(variograms[[4]], type = "p", pch = 19, npairpt= T, npairdw= F,
                         title = paste(input$property, "Experimental 4 Directional Variogram"),
                         pos.legend = 1, cex = 0.7,
                         xlab = "Log distance",
                         ylab = expression(paste("Variance (", gamma, "(h))", sep="")))
    )
  })
  
  output$fitted_variogram <- renderPlot({
    newdf <- log_transformation(df, input$property, input$log_transform)
    newdf <- outlier_filter(newdf, input$property, input$outlier_filter)
    bounds <- rounding(newdf, input$xlon, input$ylat)
    variograms <- create_vario(newdf, input$geo.surface, input$xlon, input$ylat, input$property)
    
    switch(input$dir,
           "omni" = model.auto(variograms[[1]], struct=input$structure,
                               title = paste(input$property, "Omnidirectional model with", 
                                             input$structure, "fitting"),
                               pos.legend = 1,
                               xlab = "Log distance",
                               ylab = expression(paste("Variance (", gamma, "(h))", sep=""))),
           "2dir" = model.auto(variograms[[2]], struct=input$structure,
                               title = paste(input$property, "2 Directional model with", 
                                             input$structure, "fitting"),
                               pos.legend = 1,
                               xlab = "Log distance",
                               ylab = expression(paste("Variance (", gamma, "(h))", sep=""))),
           "3dir" = model.auto(variograms[[3]], struct=input$structure,
                               title = paste(input$property, "3 Directional model with", 
                                             input$structure, "fitting"),
                               pos.legend = 1,
                               xlab = "Log distance",
                               ylab = expression(paste("Variance (", gamma, "(h))", sep=""))),
           "4dir" = model.auto(variograms[[4]], struct=input$structure,
                               title = paste(input$property, "4 Directional model with", 
                                             input$structure, "fitting"),
                               pos.legend = 1,
                               xlab = "Log distance",
                               ylab = expression(paste("Variance (", gamma, "(h))", sep="")))
    )
  })
}

