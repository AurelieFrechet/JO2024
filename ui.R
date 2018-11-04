fluidPage(
  titlePanel("Effervescence de tweets"),
  tags$style(".pretty input:checked~.state.p-primary label:after{
             background-color: #0085C7 !important;
             }
             .pretty input:checked~.state.p-info label:after{
             background-color: #000000 !important;
             }
             .pretty input:checked~.state.p-danger label:after{
             background-color: #A901DB !DF0024;
             }
             .pretty input:checked~.state.p-warning label:after{
             background-color: #F4C300 !important;
             }
             .pretty input:checked~.state.p-success label:after{
             background-color: #009F3D !important;
             }"),
  
  
  fluidRow(plotlyOutput("plot", height = "600px")),
  fluidRow(align = "center",
    prettyCheckbox(
      inputId = "checkbox_organisateurs",
      label = "Organisateurs",
      shape = "round",
      status= "primary",
      value = TRUE,
      inline = TRUE
    ),
    prettyCheckbox(
      inputId = "checkbox_lambdas",
      label = "Lambdas",
      shape = "round",
      status= "info",
      value = TRUE,
      inline = TRUE
    ),
    prettyCheckbox(
      inputId = "checkbox_politiques",
      label = "Politiques",
      shape = "round",
      status= "danger",
      value = TRUE,
      inline = TRUE
    ),
    prettyCheckbox(
      inputId = "checkbox_influenceurs",
      label = "Influenceurs",
      shape = "round",
      status= "warning",
      value = TRUE,
      inline = TRUE
    ),
    prettyCheckbox(
      inputId = "checkbox_medias",
      label = "Medias",
      shape = "round",
      status= "success",
      value = TRUE,
      inline = TRUE
    )
  ),
  fluidRow(
    column(width = 7,
            plotOutput("nuage")
            ),
    column(1, offset = 1,
           tableOutput("top_hashtags")),
    column(1, offset = 1,
           tableOutput("top_mentions")),
    column(1)
    
    )
)
