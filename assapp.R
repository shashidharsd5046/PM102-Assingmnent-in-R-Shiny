ui <-shinyUI(fluidPage(
  
  titlePanel("PMIM102 Scientific Computing and Healthcare"),
  
          uiOutput("Box1"),
                 
                 
                   fluidRow("main panel",
                            splitLayout( cellWidths = c(400,400), 
                                         
                                         tableOutput('tablesp'),
                                         
                                         tableOutput('tablep'),
                                         
                            )),
                   
                   
                   fluidRow("plot",
                            plotOutput('figur')
                   ),
  fluidRow("plot1",
           plotOutput('figu')
  )
  
  
))          














server <-shinyServer(function(input, output) {
  
  
  output$Box1<-renderUI({query<-sqlInterpolate(ANSI(),"select distinct practiceid from address ")
  outp <- dbGetQuery(pool, query)
  selectInput("name", "Choose Name",choices = outp)
  })
  
  
  
  output$tablesp <- renderTable({
    query <- sqlInterpolate(ANSI(),"select bnfname as Drug_name, sum(actcost * quantity) as Cost 
    from gp_data_up_to_2015   where practiceid like ?id 
group by bnfname order by Cost desc limit 5",id=input$name)
    outp <- dbGetQuery(pool, query)
    return(outp)})
  
  output$tablep <- renderTable({
    query <- sqlInterpolate(ANSI(),"select bnfname as Drug_name, sum(quantity) as Quantity 
    from gp_data_up_to_2015   where practiceid like ?id 
group by bnfname order by Quantity desc limit 5",id=input$name)
    outp <- dbGetQuery(pool, query)
    return(outp)})
  
  
  output$figur<-renderPlot({
    query <-sqlInterpolate(ANSI(),"select orgcode,ratio * 100 as Asthma_rate,postcode from qof_achievement
               q full join qof_indicator i using(indicator)  full join address a
               on q.orgcode = a.practiceid
          
               where i.indicator like 'AST001'")
    outp <- dbGetQuery(pool, query)
    outp<-outp%>%mutate(area=substr(postcode, start = 1, stop = 2))
    postcod<-outp%>%filter(orgcode==input$name)
    
    postcode1<-(postcod[1,4])[[1]]
    
    ad1<-outp%>%filter(area==postcode1)
    
    highlight_df <- outp %>% 
      filter(orgcode==input$name)
    hight_wales<-outp %>% 
      filter(orgcode=='WAL')
    
    x<- ggplot(data=ad1,aes(x=orgcode,y=asthma_rate))+geom_col()+geom_col(data=highlight_df,aes(x=orgcode,y=asthma_rate,color='red'))+
      geom_col(data=hight_wales,aes(x=orgcode,y=asthma_rate,color='blue'))
    return(x)
  })
  
  output$figu<-renderPlot({
    query <-sqlInterpolate(ANSI(),"select orgcode, max(field4) as num_of_patients,postcode from qof_achievement
               q full join qof_indicator i using(indicator)  full join address a
               on q.orgcode = a.practiceid
          
               where orgcode is not null and orgcode not like 'WAL' group by orgcode,postcode")
    outp <- dbGetQuery(pool, query)
    outp<-outp%>%mutate(area=substr(postcode, start = 1, stop = 2))
    postcod<-outp%>%filter(orgcode==input$name)
    
    postcode1<-(postcod[1,4])[[1]]
    
    ad1<-outp%>%filter(area==postcode1)
    
    highlight_df <- outp %>% 
      filter(orgcode==input$name)
    
    
    x<- ggplot(data=ad1,aes(x=orgcode,y=num_of_patients))+geom_col()+
      geom_col(data=highlight_df,aes(x=orgcode,y=num_of_patients,color='red'))
    return(x)
  })
  
  
  
  
})


shinyApp(ui, server)
