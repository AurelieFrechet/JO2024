fluidPage(
  titlePanel("Hello Shiny!"),
  checkboxGroupInput("groupe_twittos", "Catégorie :",
                     c("Lambdas" = "lambdas",
                       "Organisateurs" = "organisateurs",
                       "Politiques" = "politiques")),
  
  plotlyOutput("plot"),
  verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush"),
  verbatimTextOutput("zoom")
)