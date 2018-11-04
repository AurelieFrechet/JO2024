function(input, output, session) {
  current_selection <- reactive({
    result_checkbox <- c(
      input$checkbox_organisateurs,
      input$checkbox_lambdas,
      input$checkbox_politiques,
      input$checkbox_influenceurs,
      input$checkbox_medias
    )
    modalites_segments <- c("organisateurs",
                            "lambdas",
                            "politiques",
                            "influenceurs",
                            "medias")
    modalites_segments[result_checkbox]
  })
  
  current_data <- reactive({
    trace_points[which(trace_points$segment %in% current_selection()), ]
  })
  
  output$plot <- renderPlotly({
    plot_ly(
      current_data(),
      x = ~ tweet_date,
      y = ~ coord,
      text = ~ tweet_text,
      key = ~ tweet_id,
      type = 'scattergl',
      mode = 'markers',
      marker = list(
        size = ~ size_point,
        opacity = 0.6,
        color = 'transparent',
        line = list(color = ~ segment_col,
                    width = 2)
      )
    ) %>%
      layout(
        title = '',
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          ticks = '',
          showticklabels = FALSE,
          rangesilder = list(type = "date")
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          ticks = '',
          showticklabels = FALSE
        ),
        annotations = list(
          x = x,
          y = y,
          text = text,
          showarrow = TRUE,
          arrowhead = 6,
          xanchor = xanchor,
          ax = y,
          ay = ay
        )
      )
  })
  
  current_words <- reactive({
    d <- event_data("plotly_selected")
    if (is.null(d)) {
      txt <- current_data()$lemme
    } else {
      txt <-
        current_data()[which(current_data()$tweet_id %in% d$key), ]$lemme
    }
    
    txt <- gsub("Ãªtre", "être", txt)
    unwanted_array = list(
      'Š' = 'S',
      'š' = 's',
      'Ž' = 'Z',
      'ž' = 'z',
      'À' = 'A',
      'Á' = 'A',
      ' ' = 'A',
      'Ã' = 'A',
      'Ä' = 'A',
      'Å' = 'A',
      'Æ' = 'A',
      'Ç' = 'C',
      'È' = 'E',
      'É' = 'E',
      'Ê' = 'E',
      'Ë' = 'E',
      'Ì' = 'I',
      'Í' = 'I',
      'Î' = 'I',
      'Ï' = 'I',
      'Ñ' = 'N',
      'Ò' = 'O',
      'Ó' = 'O',
      'Ô' = 'O',
      'Õ' = 'O',
      'Ö' = 'O',
      'Ø' = 'O',
      'Ù' = 'U',
      'Ú' = 'U',
      'Û' = 'U',
      'Ü' = 'U',
      'Ý' = 'Y',
      'Þ' = 'B',
      'ß' = 'S',
      'à' = 'a',
      'á' = 'a',
      'â' = 'a',
      'ã' = 'a',
      'ä' = 'a',
      'å' = 'a',
      'æ' = 'a',
      'ç' = 'c',
      'è' = 'e',
      'é' = 'e',
      'ê' = 'e',
      'ë' = 'e',
      'ì' = 'i',
      'í' = 'i',
      'î' = 'i',
      'ï' = 'i',
      'ð' = 'o',
      'ñ' = 'n',
      'ò' = 'o',
      'ó' = 'o',
      'ô' = 'o',
      'õ' = 'o',
      'ö' = 'o',
      'ø' = 'o',
      'ù' = 'u',
      'ú' = 'u',
      'û' = 'u',
      'ý' = 'y',
      'ý' = 'y',
      'þ' = 'b',
      'ÿ' = 'y'
    )
    
    lettres_avec_accents = paste(names(unwanted_array), collapse = ' ')
    lettres_sans_accents = paste(unwanted_array, collapse = ' ')
    txt <- chartr(lettres_avec_accents, lettres_sans_accents, txt)
    corpus <- Corpus(x = VectorSource(x = txt))
    
    corpus <- tm_map(
      x = corpus,
      FUN = removeWords,
      words = c(
        stopwords("fr"),
        "etre",
        "tre",
        "for",
        "the",
        "and",
        "avoir",
        "deja",
        "paris2024",
        "jo2024"
      )
    )
    
    dtm_tf <- DocumentTermMatrix(x = corpus,
                                 control = list(weighting = weightTf))
    inspect(dtm_tf) #26761 termes
    
    # Nettoyage matrice
    dtm_tf <- removeSparseTerms(dtm_tf, sparse = 0.99)
    words_tf<- sort(apply(dtm_tf, 2, sum), decreasing = T)
    words_tf
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$nuage <- renderPlot({
    wordcloud_rep(names(current_words()), 
                  current_words(),
                  colors=unlist(colors_JO))

  })
  
  current_hashtags <- reactive({
    d <- event_data("plotly_selected")
    if (is.null(d)) {
      txt <- current_data()$tweet_used_hashtags_list
    } else {
      txt <-
        current_data()[which(current_data()$tweet_id %in% d$key), ]$tweet_used_hashtags_list
    }
    mots <- unlist(strsplit(txt, split = " "))
    mots <- paste0("#", mots)
    mots <- as.data.frame(table(mots))
    mots <- mots[order(mots$Freq, decreasing = TRUE),]
    head(mots,10)
  })
  
  output$top_hashtags <- renderTable({
    current_hashtags()
  })
  
  current_mentions <- reactive({
    d <- event_data("plotly_selected")
    if (is.null(d)) {
      txt <- current_data()$tweet_user_mentions_list
    } else {
      txt <-
        current_data()[which(current_data()$tweet_id %in% d$key), ]$tweet_user_mentions_list
    }
    mots <- unlist(strsplit(txt, split = " "))
    mots <- paste0("@", mots)
    mots <- as.data.frame(table(mots))
    mots <- mots[order(mots$Freq, decreasing = TRUE),]
    head(mots,10)
  })
  
  output$top_mentions <- renderTable({
    current_mentions()
  })
}