library(shiny)
library(lpSolveAPI)

ui <- fluidPage(
  navbarPage("Table Production Cost Optimization",
             tabPanel("Problem Statement",
                      mainPanel(
                        h4('A company produces 2 types of tables, T1 and T2. 
                           For T1, it takes 2 hours to produce the parts of one unit, 
                           1 hour to assemble and 2 hours to polish.
                           For T2, it takes 4 hours to produce the parts of one unit, 
                           2.5 hour to assemble and 1.5 hours to polish.
                           Per month, 7000 hours are available for producing the parts, 
                           4000 hours for assembling the parts and 5500 hours for polishing the tables.
                           The profit per unit of T1 is $90 and per unit of T2 is $110.'),  
                        
                        h4('We are supposed to maximize the total monthly profit by figuring out the number of tables to be produced for each type.')
                        )),       
             
             tabPanel("LP Model",
                      mainPanel(
                        h3('The LP formulation below is created in R based on the user inputs and is solved using the 
                           lpSolveAPI package'),
                        verbatimTextOutput("model"))),
             
             tabPanel("Optimization Results",
                      sidebarPanel(
                        h4("Enter the number of tables to produce, and hours spent on producing it"),
                        
                        numericInput('Bound1','Number of hours to produce the parts',7000,min=0,step=1),
                        numericInput('Bound2','Number of hours to assemble the parts',4000,min=0,step=1),
                        numericInput('Bound3','Number of hours for polishing the parts',5500,min=0,step=1),
                        
                        numericInput('Variable1','Type1 Tables',90,min=0,step=1),
                        numericInput('Variable2','Type2 Tables',110,min=0,step=1),
                        
                        numericInput('Row11','T1: hours to produce',2,step=1),
                        numericInput('Row21','T1: hours to assemble',1,step=1),
                        numericInput('Row31','T1: hours to polish',2,step=1),
                        
                        numericInput('Row12','T2: hours to produce',4,step=1),
                        numericInput('Row22','T2: hours to assemble',2.5,step=1),
                        numericInput('Row32','T2: hours to polish',1.5,step=1),
                        
                        submitButton('Optimize')),
                      
                      
                      mainPanel(
                        h2('Results on optimizing'),
                        h4('Constraints:'),verbatimTextOutput('constraints'),
                        h4('Variables:'),verbatimTextOutput("variables"),
                        h4('Maximized Profit:'),verbatimTextOutput("objective"))),
             
             tabPanel("Understanding the results",
                      mainPanel(
                        h4('Looking at the optimized values, we can say that the companys maximization profit is of
                  $273000 producing 2300 Type 1 tables and 600 Type 2 tables.'),
                        h4('Here the problem statement is defined with specific values, 
                but the user can even enter the updated number of hours or tables to be produced
                to check for other maximized profits.')
                      ))
))


server <- function(input, output) {
  lprec <- make.lp(3, 2)
  invisible(lp.control(lprec, sense = "max"))
  
  set.objfn(lprec, c(90, 110))
  set.constr.value(lprec, rhs = c(7000,4000,5500), constraints=seq(1:3))
  set.constr.type(lprec, c(rep("<=", 3)))
  set.row(lprec, 1, c(2,4))
  set.row(lprec, 2, c(1,2.5))
  set.row(lprec, 3, c(2,1.5))
  name.lp(lprec, "Maximizing the profit to produce the two types of Tables")
  dimnames(lprec) <- list(c("Hours to produce","Hours to assemble","Hours to polish"), c("T1 Tables","T2 Tables"))
  
  output$outvariable1 <- renderPrint({input$Variable1})
  output$outvariable2 <- renderPrint({input$Variable2})
  
  output$outrow11 <- renderPrint({input$Row11})
  output$outrow12 <- renderPrint({input$Row12})
  
  output$outrow21 <- renderPrint({input$Row21})
  output$outrow22 <- renderPrint({input$Row22})
  
  output$outrow31 <- renderPrint({input$Row31})
  output$outrow32 <- renderPrint({input$Row32})
  
  output$outBound1 <- renderPrint({input$Bound1})
  output$outBound2 <- renderPrint({input$Bound2})
  output$outBound3 <- renderPrint({input$Bound3})
  
  output$objective <- renderText({
    set.objfn(lprec,c(input$Variable1,input$Variable2))
    set.row(lprec, 1, c(input$Row11,input$Row12))
    set.row(lprec, 2, c(input$Row21,input$Row22))
    set.row(lprec, 3, c(input$Row31,input$Row32))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.objective(lprec)
  })
  
  output$constraints <- renderText({
    set.objfn(lprec,c(input$Variable1,input$Variable2))
    set.row(lprec, 1, c(input$Row11,input$Row12))
    set.row(lprec, 2, c(input$Row21,input$Row22))
    set.row(lprec, 3, c(input$Row31,input$Row32))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.constraints(lprec)
  })
  
  output$variables <- renderText({
    set.objfn(lprec,c(input$Variable1,input$Variable2))
    set.row(lprec, 1, c(input$Row11,input$Row12))
    set.row(lprec, 2, c(input$Row21,input$Row22))
    set.row(lprec, 3, c(input$Row31,input$Row32))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.variables(lprec)
  })  
  
  output$model <- renderPrint({
    set.objfn(lprec,c(input$Variable1,input$Variable2))
    set.row(lprec, 1, c(input$Row11,input$Row12))
    set.row(lprec, 2, c(input$Row21,input$Row22))
    set.row(lprec, 3, c(input$Row31,input$Row32))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.objective(lprec)
    solve(lprec)
    print(lprec)
  })
  
}  
shinyApp(ui, server)
