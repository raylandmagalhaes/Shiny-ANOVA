function(input, output,session) {
  
  
  
  df<-reactive({
    
    req(input$file1)
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    )
    
    
    return(df)
    
    
  })
  
  
  observe({
    req(input$file1)
    dsnames <- names(df())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    updateSelectInput(session, "response",
                      choices = cb_options,
                      selected = "")
    
  })  
  observe({
    req(input$file1)
    dcnames <- names(df()[names(df()) != input$response])
    cc_options <- list()
    cc_options[dcnames] <- dcnames
    updateSelectInput(session, "categorical",
                      choices = cc_options,
                      selected ="")
  })  
  output$contents <- renderDataTable({
    df()
  })
  
  resposta<-reactive({
    df()%>%
      select(y=input$response)
  })
  
  dados<-reactive({
    df()%>%
      select(resposta=input$response,input$categorical)
  })
  
  observe({
    req(input$file1)
    req(input$categorical)
    
    n <- reactive({
      length( input$categorical)
    })
    
    updateSelectInput(session, "interacoes",
                      choices = 1:min(3,n()),
                      selected =1)
  })  
  
  interacao3<-reactive({
    df()%>%
      select(resp=input$response,c1=input$categorical[1],c2=input$categorical[2],c3=input$categorical[3]) %>% 
      group_by(c1,c2,c3) %>%
      summarise(Media = mean(resp, na.rm=TRUE))
  })
  
  interacao2<-reactive({
    df()%>%
      select(resp=input$response,c1=input$categorical[1],c2=input$categorical[2]) %>% 
      group_by(c1,c2) %>%
      summarise(Media = mean(resp, na.rm=TRUE))
  })
  
  output$interacao2<-renderPlot({
    
    ggplot(interacao2(), aes(x=c2, y=Media)) +
      geom_line(aes(colour=c1, group=c1)) +
      geom_point(aes(colour=c1)) 
    
    
  })
  
  output$interacao3<-renderPlot({
    
    interaction.ABC.plot(Media, x.factor=c2,
                         groups.factor=c1, trace.factor=c3,
                         data=interacao3())
    
  })

    anova<- reactive({
      if(input$interacoes==1){
      aov(resposta~.,data=dados())
      }else if(input$interacoes==2){
        aov(resposta~.^2,data=dados())
      }else if(input$interacoes==3){
        aov(resposta~.^3,data=dados())
      }
    })

  

  output$anova<- renderPrint({
    xtable(summary(anova()))%>%
    kable(caption = "Anova",digits=4,col.names=c("Gl","Soma_Quadrados","Quadrado_Médio","Estatística_F","Pr(>F)"))%>%
    kable_styling(bootstrap_options = "striped", full_width = T,latex_options = "hold_position")
  })
  
  
  output$autoplot<- renderPlot({
    autoplot(anova())
  })
  
}

# cor para menos níveis facets para o segudo menr...

#ajustar o autoplot

#melhorar a estética dos gráficos theme bw()

# organizar em abas 

# comçar a organizar o relatório automatizado.

