library(shiny)
library(knitr)
library(readr)
library("crayon")
library("shinyjs")

source("doman_functions.r")

ui <- fluidPage(

    sidebarLayout(
        
        sidebarPanel(
            
            fileInput(inputId = "word_list",
                      label = "Загрузить словарь"),
            numericInput(inputId = "word_num",
                         label = "Сколько слов",
                         value =25),
            numericInput(inputId = "start_word",
                         label = "Первое слово",
                         value = 1),
            sliderInput(inputId = "speed",
                        label = "Скорость",
                        min = 0.5, max = 1.5, 
                        value = 1, ticks = F, round = -1),
            sliderInput(inputId = "size",
                        label = "Размер",
                        min = 0.5, max = 1.5, 
                        value = 1, ticks = F, round = -1),
            
            actionButton(inputId = "start", label = "Start!"),
            
            width = 2
        ),
        
        mainPanel(
            useShinyjs(),
            span(textOutput(outputId = "error"), align = "center", style="font-size:200%"),
            span(textOutput(outputId = "word"), align = "center"),

            width = 11
             
        )
    )
)

server <- function(input, output, session) {
    
    pick_word <- function(input, output) {
        num_words = input$word_num
        
        speed = input$speed
        size = input$size
        vocabulary_file = unlist(input$word_list[4])
        
        word_list = read_vocabulary(vocabulary_file, num_words = num_words, start_word = input$start_word)

        if (word_list[1] == 1) {
            shinyjs::html(id = 'error', "Загрузите текстовый файл")
            return()
        } else if (word_list[1] == 2) {
            shinyjs::html(id = 'error', "'Первое слово' за пределами словаря")
            return()
        }
        
        
        word_list = c(word_list, "")
        
        for (i in 1:length(word_list)) {
            sleep_time = (0.9 + round(nchar(word_list[i])/10, 1))/speed
            print(paste0(sleep_time, " ", word_list[i]))
            word_react$n <- paste0("<p style=\"color:red; font-size:", size*1500, 
                                   "%\"><b>", word_list[i], "</p></b>")
            #print(word_react$n)
            shinyjs::html(id = 'word', word_react$n)
            
            Sys.sleep(sleep_time)
        }
    }
    
    word_react = reactiveValues(n = "")
    
    observeEvent(input$start, {
        shinyjs::html(id = 'error', "")
        showWord <- pick_word(input, output)
        
        #session$onSessionEnded(function() { unlink(word_list, recursive = TRUE) } )
        
    }, once = F) 
}

shinyApp(ui=ui, server = server)
