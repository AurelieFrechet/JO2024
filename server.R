function(input, output, session) {
  
  output$plot <- renderPlotly({
    plot_ly(trace_points, 
            x = ~tweet_date, 
            y = ~coord, 
            color = ~segment2,
            # text = ~tweet_text, 
            type = 'scattergl', 
            mode = 'markers',
            marker = list(size = ~size_point, 
                          opacity = 0.5)) %>%
      layout(title = 'Tweets émis sur les JO2024',
             xaxis = list(title = "",
                          showgrid = FALSE),
             yaxis = list(title = "",
                          showgrid = FALSE,
                          zeroline = FALSE,
                          showline = FALSE,
                          ticks = '',
                          showticklabels = FALSE))
  })
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
  })
  
  output$zoom <- renderPrint({
    d <- event_data("plotly_relayout")
    if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
  })
  
  
  # output$nuage <- renderPlot({
  #   v <- terms()# tableau term Terdocument matrice  https://shiny.rstudio.com/gallery/word-cloud.html
  #   wordcloud_rep(names(v), v, scale=c(4,0.5),
  #                 min.freq = input$freq, max.words=input$max,
  #                 colors=brewer.pal(8, "Dark2"))
  # })
}