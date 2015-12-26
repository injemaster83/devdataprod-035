shinyServer(
  function(input, output) {
    
    data(mtcars)
    library(car)
    mtcars$cyl <- as.factor(mtcars$cyl)
    mtcars$am <- as.factor(mtcars$am)
    mtcars$vs <- as.factor(mtcars$vs)
    mtcars$gear <- as.factor(mtcars$gear)
    
    output$predictors <- renderUI ({
      colnames <- names(subset(mtcars,select=-c(mpg)))
      names(colnames) <- c("Cylinders (cyl)","Displacement (disp)","Gross horsepower (hp)","Rear axle ratio (drat)","Weight (wt)",
                           "Quarter-mile time (qsec)","V/S (vs)","Transmission (am)","# of Forward gears (gear)","# of Carburetors (carb)")
      
      checkboxGroupInput("selection", 
                         label = h3("Predictors"), 
                         choices = colnames,
                         selected= c("am","hp"),
                         width='200px')
      
    })
    
    output$text <- renderText({
      calculateModel()
      showText()
    })

    output$table <- renderDataTable ({
      calculateModel()
      showTable()  
    }, options = list(dom = 't', searching = FALSE, paging= FALSE, aoColumnDefs = list(list(sWidth=c("80px"), aTargets=list(0)))
    ))
        
    output$model <- renderPlot({
      calculateModel()
      graph()
    })
    
    output$r2 <- renderPrint({
      calculateModel()
      showR2()
    })    
    
    output$tablevif <- renderDataTable ({
      calculateModel()
      showTableVIF()  
    }, options = list(dom = 't', searching = FALSE, paging= FALSE, aoColumnDefs = list(list(sWidth=c("80px"), aTargets=list(0)))
    ))

    showTable <- eventReactive(input$calculate, {       
      as.data.frame(cbind(variables=rownames(summary(fit)$coefficients),summary(fit)$coefficients))
    })  
    
    showTableVIF <- eventReactive(input$calculate, {       
      as.data.frame(cbind(variables=rownames(as.data.frame(vif(fit))),vif=vif(fit)))
    })
    
    showText <- eventReactive(input$calculate, {       
      paste("Selected predictors: ", paste(input$selection, sep = "", collapse = ", "),sep="")           
      })        
    
    showR2 <- eventReactive(input$calculate, { 
      cat(paste("R^2= ",summary(fit)$r.squared,sep=""))
    })
    
    calculateModel <- eventReactive(input$calculate, {       
      fit <<- lm(as.formula(paste("mpg", paste(input$selection, sep = "", collapse = " + "), sep = " ~ ")), mtcars)
    }) 
    
    graph <- eventReactive(input$calculate, {       
      par( mfrow = c( 2, 3 ) )
      plot(fit,1)
      plot(fit,2)
      plot(fit,3)
      plot(fit,4)
      plot(fit,5)
      plot(fit,6)
    })
    
  }
)
