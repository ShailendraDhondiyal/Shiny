function(input, output) {

# reactive

  df_index1 <- reactive({
    df %>% filter(., Country.Name == input$country1, !is.na(X.Score)) %>%
      ggplot(., aes(x=Year, y = X.Score)) +
      geom_col(fill = '#83b51e') +
      ggtitle('EFI Index Score Over Years - Country 1') +
      xlab('Year') +
      ylab('Index Score') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none') +
      ylim(0, 100)
  })
  
  df_index2 <- reactive({
    df %>% filter(., Country.Name == input$country2, !is.na(X.Score)) %>%
      ggplot(., aes(x=Year, y = X.Score)) +
      geom_col(fill = '#83b51e') +
      ggtitle('EFI Index Score Over Years - Country 2') +
      xlab('Year') +
      ylab('Index Score') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none') +
      ylim(0, 100)
  })
  
# UPPER TAB (Comparison)

# TAB 1

  output$rank1 <- renderPlot(
    df %>% filter(., Country.Name == input$country1, !is.na(World.Rank)) %>%
      ggplot(., aes(x=Year)) +
      geom_col(aes(y = World.Rank), fill = '#4682b4') +
      ggtitle('World-Wide Rank Over Years - Country1') +
      xlab('Year') +
      ylab('World Rank') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none') +
      ylim(0, 200)
  )

  output$rank2 <- renderPlot(
    df %>% filter(., Country.Name == input$country2, !is.na(World.Rank)) %>%
      ggplot(., aes(x=Year)) +
      geom_col(aes(y = World.Rank), fill = '#4682b4') +
      ggtitle('World-Wide Rank Over Years - Country2') +
      xlab('Year') +
      ylab('World Rank') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none') +
      ylim(0, 200)
  )  
   
  output$index11 <- renderPlot(
    df_index1()
  )

  output$index12 <- renderPlot(
    df_index2()
  ) 

  
# TAB 2
  output$components1 <- renderPlot(
    df %>%
      filter(., Country.Name == input$country1, Year == 2019) %>%
      gather(., key = 'component', value = 'value', Property.Rights:Financial.Freedom) %>%
      ggplot(., aes(x = ordered(component, levels = c("Property.Rights", "Judical.Effectiveness", "Government.Integrity", "Tax.Burden", "Govt.Spending", "Fiscal.Health", "Business.Freedom", "Labor.Freedom", "Monetary.Freedom", "Trade.Freedom", "Investment.Freedom", "Financial.Freedom")), y = value, fill = component)) +
      geom_col() +
      ggtitle('2019 Performance on Different Paramaters - Country 1') +
      xlab('Year') +
      ylab('Index Score') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none',axis.text.x = element_text(angle = 90)) 
  )
  
  output$categories1 <- renderPlot(
    df %>%
      filter(., Country.Name == input$country1, Year == 2019) %>%
      gather(., key = 'category', value = 'value', rule_law:mkt_open, X.Score) %>%
      ggplot(., aes(x = ordered(category, levels = c("rule_law", "govt_size", "reg_eff", "mkt_open", "X.Score")), y = value, fill = category)) +
      geom_col() +
      ggtitle('2019 Performance in Different Categories - Country 1') +
      xlab('Year') +
      ylab('Index Score') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none',axis.text.x = element_text(angle = 90))     
  )

  output$components2 <- renderPlot(
    df %>%
      filter(., Country.Name == input$country2, Year == 2019) %>%
      gather(., key = 'component', value = 'value', Property.Rights:Financial.Freedom) %>%
      ggplot(., aes(x = ordered(component, levels = c("Property.Rights", "Judical.Effectiveness", "Government.Integrity", "Tax.Burden", "Govt.Spending", "Fiscal.Health", "Business.Freedom", "Labor.Freedom", "Monetary.Freedom", "Trade.Freedom", "Investment.Freedom", "Financial.Freedom")), y = value, fill = component)) +
      geom_col() +
      ggtitle('2019 Performance on Different Paramaters - Country 2') +
      xlab('Year') +
      ylab('Index Score') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none',axis.text.x = element_text(angle = 90)) 
  )
  
  output$categories2 <- renderPlot(
    df %>%
      filter(., Country.Name == input$country2, Year == 2019) %>%
      gather(., key = 'category', value = 'value', rule_law:mkt_open, X.Score) %>%
      ggplot(., aes(x = ordered(category, levels = c("rule_law", "govt_size", "reg_eff", "mkt_open", "X.Score")), y = value, fill = category)) +
      geom_col() +
      ggtitle('2019 Performance in Different Categories - Country 2') +
      xlab('Year') +
      ylab('Index Score') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none',axis.text.x = element_text(angle = 90))     
  )


# TAB 3
  output$index31 <- renderPlot(
    df_index1()
  )
    
  output$index32 <- renderPlot(
    df_index2()
  ) 

  output$pcgdp1 <- renderPlot(
    df %>% filter(., Country.Name == input$country1, !is.na(GDP.per.Capita.PPP)) %>%
      ggplot(., aes(x = Year)) +
      geom_col(aes(y = GDP.per.Capita.PPP), fill = "#848484") +
      ggtitle('Per Capita GDP Over Years - Country 1') +
      xlab('Year') +
      ylab('Per Capita GDP (adjusted for PPP)') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
  )
  
  output$pcgdp2 <- renderPlot(
    df %>% filter(., Country.Name == input$country2, !is.na(GDP.per.Capita.PPP)) %>%
      ggplot(., aes(x = Year)) +
      geom_col(aes(y = GDP.per.Capita.PPP), fill = "#848484") +
      ggtitle('Per Capita GDP Over Years - Country 2') +
      xlab('Year') +
      ylab('Per Capita GDP (adjusted for PPP)') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
  ) 

# MIDDLE TAB (Income Q) 

  output$r1c1 <- renderPlot(
    if(input$radio == "box-plot"){ 
             ggplot(df2019, aes(x = income.quartile, y = rule_law, fill = income.quartile)) +
             geom_boxplot() +
        ggtitle('Rule of Law ~ Per Capita Income') +
        xlab('Income Quartile') +
        ylab('Score on Rule of Law') +
        theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
    } else{
       mosaicplot(table2019_law, main="Rule of Law ~ Per Capita Income",
                  xlab="Income Quartile",
                  ylab="Rule of Law Quartile",
                  color = TRUE)
        }
  )
  
  output$r1c2 <- renderPlot(
    if(input$radio == "box-plot"){ 
      ggplot(df2019, aes(x = income.quartile, y = govt_size, fill = income.quartile)) +
        geom_boxplot() +
        ggtitle('Govt Size ~ Per Capita Income') +
        xlab('Income Quartile') +
        ylab('Score on Govt Size') +
        theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
      
    } else{
      mosaicplot(table2019_govt, main="Govt Size ~ Per Capita Income",
                 xlab="Income Quartile",
                 ylab="Govt Size Quartile",
                 color = TRUE)
    }
  )  
  
  output$r2c1 <- renderPlot(
    if(input$radio == "box-plot"){ 
      ggplot(df2019, aes(x = income.quartile, y = reg_eff, fill = income.quartile)) +
        geom_boxplot() +
        ggtitle('Regulatory Efficiency ~ Per Capita Income') +
        xlab('Income Quartile') +
        ylab('Score on Regulatory Efficiency') +
        theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
    } else{
      mosaicplot(table2019_reg, main="Regulatory Efficiency ~ Per Capita Income",
                 xlab="Income Quartile",
                 ylab="Regulatory Efficiency Quartile",
                 color = TRUE)
    }
  )    
  
  output$r2c2 <- renderPlot(
    if(input$radio == "box-plot"){ 
      ggplot(df2019, aes(x = income.quartile, y = mkt_open, fill = income.quartile)) +
        geom_boxplot() +
        ggtitle('Market Openness ~ Per Capita Income') +
        xlab('Income Quartile') +
        ylab('Score on Market Openness') +
        theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
    } else{
      mosaicplot(table2019_mkt, main="Market Openness ~ Per Capita Income",
                 xlab="Income Quartile",
                 ylab="Market Openness Quartile",
                 color = TRUE)
    }
  )  
  
     
# LAST TAB (cor)
  output$cormap <- renderPlot(
    df2019 %>% select(., x = input$xvar, y = input$yvar) %>%
      ggplot(., aes(x = x, y = y)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = 'lm')+
      ggtitle('Economic Indicator ~ Economic Freedom Parameter (2019)') +
      xlab('Score on Economic Freedom Parameter') +
      ylab('Value of Economic Indicator') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
  )
  
  output$lmsummary <- renderPrint({
    print("******************Summary*********************")
    summary(lm(reformulate(input$xvar, input$yvar), data = df))

  })
  
}
