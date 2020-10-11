
library(shiny)

dados = read.csv("slr12.csv",sep=";")
modelo = lm(CusInic ~ FrqAnual, data = dados)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Previsão de Custo Inicial para montar uma Franquia"),
    
    fluidRow(
        column(
            4,
            h2("Dados"),
            tableOutput("Dados")
        ),
        column(
            8,
            plotOutput("Graf")
        )
    ),
    
    fluidRow(
        column(
            6,
            h3("Valor Anual da Franquia:"),
            numericInput(inputId = "novoValor",label = "Insira Novo Valor", value = 1500, min = 1, max = 99999999),
            actionButton(inputId = "processar", label = "Processar")
        ),
        column(
            6,
            h1(textOutput(outputId = "resultado"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Graf <- renderPlot({
        plot(CusInic ~ FrqAnual, data = dados)
        abline(modelo)
    })
    
    output$Dados = renderTable({head(dados, 10)})
    
    observeEvent(input$processar, {
        valr = input$novoValor
        prev = predict(modelo, data.frame(FrqAnual = eval(parse(text = valr))))
        prev = paste0("Previsão de Custo Inicial R$: ", round(prev,2))
        output$resultado = renderText({prev})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
