# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  
# Define page titles ----
  output$page0title <- renderUI({
    paste0(input$lep0a, ": overview of local landscape")
  })
  
  output$page1title <- renderUI({
    paste0(input$lep1, ": employment trends")
  })
  
  output$page2title <- renderUI({
    paste0(input$lep3, ": skill supply trends")
  })

  output$page3title <- renderUI({
    paste0(input$lep5, ": Overview of Skill Demand")
  })
  
  output$page4title <- renderUI({
    paste0(input$lep7, ": Overview of earnings")
  })
  
  output$page5title <- renderUI({
    paste0(input$lep9, ": Overview of HE")
  })
  
  ###Conditional functions---- 
  # Returns arrow-up icon on true (if true_direction is 'up')
  cond_icon <- function(condition, true_direction = "up") {
    if (true_direction == "up") {
      return(icon(ifelse(condition, "arrow-up", "arrow-down"), "fa-2x fa-beat"))
    }
    return(icon(ifelse(condition, "arrow-down", "arrow-up"), "fa-2x fa-beat"))
  }
  
  # Conditional color for widget
  # Returns 'green' on true, 'red' on false, e.g. api usage % change > 0
  #                                               load time % change < 0
  cond_color <- function(condition, true_color = "green") {
    if(is.na(condition)){
      return("black")
    }
    colours <- c("green","red")
    return(ifelse(condition, true_color, colours[!colours == true_color]))
  }
  
  # OVERVIEW ----
  
###Downloads---- 
  #download all indicators
  list_of_datasets0 <- list("2.Emp by occupation" = C_EmpOcc_APS1721,
                           "5.Emp rate" = C_EmpRate_APS1721,
                           "12a.FE achievements SSA"=C_Achieve_ILR21,
                           "12b.FE achievements"=C_Achieve_ILR1621,
                           "22.Vacancies"=C_Vacancy_ONS1722)
  output$download_btn0a <- downloadHandler(
    filename = function() { "CoreIndicators.xlsx"},
    content = function(file) {write_xlsx(list_of_datasets0, path = file)}
  )
  
  #Download current LEP indicators
  filtered_data0  <- reactive({
    list("2.Emp by occupation" = filter(C_EmpOcc_APS1721,geographic_level == "lep",area == input$lep0a),
         "5.Emp rate" = filter(C_EmpRate_APS1721, geographic_level == "lep",area==input$lep0a),
         "12a.FE achievements SSA"=filter(C_Achieve_ILR21,LEP == input$lep0a),
         "12b.FE achievements"=filter(C_Achieve_ILR1621,LEP == input$lep0a),
         "22.Vacancies"=filter(C_Vacancy_ONS1722,LEP == input$lep0a)
    )
    })
  output$download_btn0b <- downloadHandler(
    filename = function() {"CurrentIndicators.xlsx"},
    content = function(file) {write_xlsx(filtered_data0(), path = file)}
  )

  ## KPIs ----

  ### Employment count ----
  output$locland.emplcnt0 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      format((C_EmpRate_APS1721 %>%
                filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                       area==input$lep0a,
                       year=="2021")
      )$"28  in employment ",
      scientific=FALSE,big.mark=","),
      "people employed"
      ,width=12
      ,color="blue"
    )
  })
  
  ### Employment change ----
  output$locland.emplcntchange0 <- renderValueBox({
#get value
    x<-((C_EmpRate_APS1721 %>%
          filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                 area==input$lep0a,
                 year=="2021")
    )$"28  in employment "
    -(C_EmpRate_APS1721 %>%
        filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
               area==input$lep0a,
               year=="2020")
    )$"28  in employment ")
#build box   
    valueBox(
      sprintf("%+.0f",x),
      subtitle = NULL
      ,width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)
    )
  })
    
  ### Employment rate -----
  output$locland.emplrate0 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      paste(
        format(100.*(C_EmpRate_APS1721 %>%
                       filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                              area==input$lep0a,
                              year=="2021")
        )$empRate,digits=2),
        "%"),
      paste("employment rate (compared with ",
            
              format(100.*(C_EmpRate_APS1721 %>%
                             filter(geographic_level == "country", # cleans up for London which is included as lep and gor
                                    year=="2021")
              )$empRate,digits=2),
              "% for England)")
            , width=12
      ,color="blue"
    )
  })
  
  ### Employment rate change -----
  output$locland.emplchange0 <- renderValueBox({
x<-(100.*((C_EmpRate_APS1721 %>%
            filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                   area==input$lep0a,
                   year=="2021"))$empRate-
           (C_EmpRate_APS1721 %>%
              filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                     area==input$lep0a,
                     year=="2020"))$empRate))
    valueBox(
      # take input number
      paste(
        sprintf("%+.0f",x
        ),
        "ppts"),subtitle = NULL,
      width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)
    )
  })
 
  #Add button to link to employment data 
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Employment")
  })
  
  ### ONS job advert units  ----
  output$jobad.units <- renderValueBox({
    valueBox(C_Vacancy_England %>%
                       filter(year == "2022",
                              LEP == input$lep0a)%>%
                       summarise(job.unit=sum(vacancy_unit)),
      "job vacancy units",
      width=12
      ,color="blue"
    )
  })
  
  ### ONS job advert units change  ----
  output$jobad.change <- renderValueBox({
    x<-((C_Vacancy_England %>%
           filter(year == "2022",
                  LEP == input$lep0a)%>%
           summarise(job.unit=sum(vacancy_unit)))
        -(C_Vacancy_England %>%
            filter(year == "2021",
                   LEP == input$lep0a)%>%
            summarise(job.unit=sum(vacancy_unit))))
    valueBox(
      sprintf("%+.0f",x),
      subtitle=NULL,
      width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)     
    )
  })
  
  #Add button to link to vacancy data 
  observeEvent(input$link_to_tabpanel_skill_demand, {
    updateTabsetPanel(session, "navbar", "Skill Demand")
  })
  
  ### Average salary  ----
  output$earn.avg <- renderValueBox({
    valueBox("In V2",
             "average salary (compared with x% for England)",
             width=12
             ,color="blue"
    )
  })
  
  ### Average salary change  ----
  output$earn.change <- renderValueBox({
    x<-(0)
    valueBox(
      sprintf("%+.0f",x),
      subtitle=NULL,
      width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)     
    )
  })
  
  #Add button to link to salary data 
  observeEvent(input$link_to_tabpanel_earnings, {
    updateTabsetPanel(session, "navbar", "Earnings")
  })
  
  ### L4+  ----
  output$skills.l4 <- renderValueBox({
    valueBox("In V2",
             "qualified at level 4+",
             width=12
             ,color="blue"
    )
  })
  
  ### L4+ change  ----
  output$skills.l4change <- renderValueBox({
    x<-(0)
    valueBox(
      sprintf("%+.0f",x),
      subtitle=NULL,
      width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)     
    )
  })
 
  #Add button to link to skills data 
  observeEvent(input$link_to_tabpanel_skills, {
    updateTabsetPanel(session, "navbar", "Skill Supply")
  }) 
  
  
  ### E&T achievements -----
  output$skisup.ETach <- renderValueBox({
    valueBox(
      format((C_Achieve_ILR1621 %>%
                filter(time_period=="202021",
                       LEP == input$lep0a, 
                       level_or_type == "Education and training: Total")%>%
                        summarise(App_ach=sum(achievements))), scientific=FALSE,big.mark=","),
      "Education and training acheivements",
      width=12
      ,color="blue"
    )
  })
  
  ### E&T achievements change -----
  output$skisup.ETachChange <- renderValueBox({
    x<-((C_Achieve_ILR1621 %>%
          filter(time_period=="202021",
                 LEP == input$lep0a, 
                 level_or_type == "Education and training: Total")%>%
          summarise(App_ach=sum(achievements)))
    -(C_Achieve_ILR1621 %>%
        filter(time_period=="201920",
               LEP == input$lep0a, 
               level_or_type == "Education and training: Total")%>%
        summarise(App_ach=sum(achievements))))
    valueBox(
      sprintf("%+.0f",x
             ),
      subtitle=NULL,
      width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)    
    )
  })
  
  ### App achievements ----
  output$skisup.APPach <- renderValueBox({
    valueBox(
      format((C_Achieve_ILR1621 %>%
                filter(time_period=="202021",
                       LEP == input$lep0a, 
                       level_or_type == "Apprenticeships: Total")%>%
                dplyr::summarise(App_ach=sum(achievements))), scientific=FALSE,big.mark=","),
      "Apprenticeship achievements",
      width=12
      ,color="blue"
    )
  })
  
  ### App achievements change ----
  output$skisup.APPachChange <- renderValueBox({
    x<-((C_Achieve_ILR1621 %>%
          filter(time_period=="202021",
                 LEP == input$lep0a, 
                 level_or_type == "Apprenticeships: Total")%>%
          summarise(App_ach=sum(achievements)))
    -(C_Achieve_ILR1621 %>%
        filter(time_period=="201920",
               LEP == input$lep0a, 
               level_or_type == "Apprenticeships: Total")%>%
        summarise(App_ach=sum(achievements))))
    valueBox(
      sprintf("%+.0f",x
             ),
      subtitle=NULL,
      width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)  
    )
  }) 

  ### HE entrants  ----
  output$he.entrants <- renderValueBox({
    valueBox("In V2",
             "HE entrants",
             width=12
             ,color="blue"
    )
  })
  
  ### HE+ change  ----
  output$he.entrantschange <- renderValueBox({
    x<-(0)
    valueBox(
      sprintf("%+.0f",x),
      subtitle=NULL,
      width=12
      ,icon = cond_icon(x > 0)
      ,color = cond_color(x > 0)     
    )
  })
  
  #Add button to link to skills data 
  observeEvent(input$link_to_tabpanel_HE, {
    updateTabsetPanel(session, "navbar", "HE")
  }) 
  
# EMPLOYMENT ----
  ###Downloads---- 
  #download employment indicators
  list_of_datasets1 <- list("2.Emp by occupation" = C_EmpOcc_APS1721,
                            "5.Emp rate" = C_EmpRate_APS1721)
  output$download_btn1a <- downloadHandler(
    filename = function() { "EmploymentIndicators.xlsx"},
    content = function(file) {write_xlsx(list_of_datasets1, path = file)}
  )
  
  #Download current LEP indicators
  filtered_data1  <- reactive({
    list("2.Emp by occupation" = filter(C_EmpOcc_APS1721,geographic_level == "lep",area == input$lep1),
         "5.Emp rate" = filter(C_EmpRate_APS1721, geographic_level == "lep",area==input$lep1)
    )
  })
  output$download_btn1b <- downloadHandler(
    filename = function() {"CurrentEmploymentIndicators.xlsx"},
    content = function(file) {write_xlsx(filtered_data1(), path = file)}
  )
  
 ## KPIs ----

  ### Employment rate -----
  output$locland.emplrate <- renderValueBox({
    valueBox(
      paste(
        format(100.*(C_EmpRate_APS1721 %>%
                       filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                              area==input$lep1,
                              year=="2021")
        )$empRate,digits=2),
        "%"),
      paste0("Employment rate in ",input$lep1),
      color="blue"
    )
  })

  output$locland.emplrate.2 <- renderValueBox({
   # if(input$lep2=="\nNone")
   #   return(NULL)
    valueBox(
      paste(
        format(100.*(C_EmpRate_APS1721 %>%
                       filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                              area==input$lep2,
                              year=="2021")
        )$empRate,digits=2),
        "%"),
      paste0("Employment rate in ",input$lep2),
      color="blue"
    )
  })
  ### Employment count ----
  output$locland.emplcnt <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      format((C_EmpRate_APS1721 %>%
                filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                       area==input$lep1,
                       year=="2021")
      )$"28  in employment ",
      scientific=FALSE,big.mark=","),
      # add subtitle to explain what it's showing
      paste0("In employment in ",input$lep1),
      color="blue"
    )
  })
  
  output$locland.emplcnt.2 <- renderValueBox({
    valueBox(
      format((C_EmpRate_APS1721 %>%
                filter(geographic_level == "lep", # cleans up for London which is included as lep and gor
                       area==input$lep2,
                       year=="2021")
      )$"28  in employment ",
      scientific=FALSE,big.mark=","),
      paste0("In employment in ",input$lep2),
      color="blue"
    )
  })

  ## Employment rate over time line graph ----
  EmpRate_time <- reactive({
    C_EmpRate_APS1721 %>%
      select(year, area, geographic_level, empRate)%>%
      filter(geographic_level == "lep"|
             geographic_level=="country",# cleans up for London and South East which is included as lep and gor
             area == "England" |
             area == input$lep1 |
             area == input$lep2) %>%
      ggplot(aes(x=year, y=empRate, group = area, colour = area))+
      geom_line()+
      theme_minimal()+
      expand_limits(y = 0.6)+
      ggtitle("Employment rate")+
      theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position = "bottom", legend.title = element_blank())+
      scale_y_continuous(labels = scales::percent_format(accuracy=1))+
      labs(colour = "")
  })

  output$EmpRate_time <- renderPlotly({
    ggplotly(EmpRate_time()) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1))
  })

  ## Employment by occupation data table ----
  EmpOcc <- reactive({
    EmpOcc <- C_EmpOcc_APS1721 %>%
      filter(year == "2021") %>%
      filter(geographic_level == "lep"|
             geographic_level=="country",# cleans up for London and South East which is included as lep and gor
             area == "England" |
             area == input$lep1 |
             area == input$lep2)%>%
      select(-year, -geographic_level)%>%
      mutate(across(where(is.numeric), format, big.mark=","))%>% 
      as.data.frame()%>%
      t()%>%
      row_to_names(row_number=1)
    #%>%
      #mutate_if(is.character, as.numeric)
  })

  output$EmpOcc <- renderDataTable({
    datatable(EmpOcc()) 
    })

# SKILLS ----
  ###Downloads---- 
  #download skills indicators
  list_of_datasets2 <- list("12a.FE achievements SSA"=C_Achieve_ILR21,
                            "12b.FE achievements"=C_Achieve_ILR1621)
  output$download_btn2a <- downloadHandler(
    filename = function() { "SkillIndicators.xlsx"},
    content = function(file) {write_xlsx(list_of_datasets2, path = file)}
  )
  
  #Download current LEP indicators
  filtered_data2  <- reactive({
    list("12a.FE achievements SSA"=filter(C_Achieve_ILR21,LEP == input$lep3),
         "12b.FE achievements"=filter(C_Achieve_ILR1621,LEP == input$lep3)
    )
  })
  output$download_btn2b <- downloadHandler(
    filename = function() {"CurrentSkillIndicators.xlsx"},
    content = function(file) {write_xlsx(filtered_data2(), path = file)}
  )
  
  ## KPIs ----
  ### FE achievements -----
  output$skisup.FEach <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
                filter(time_period=="202122",
                       LEP == input$lep3, 
                       level_or_type == "Further education and skills: Total")%>%
                summarise(App_ach=sum(achievements))), scientific=FALSE,big.mark=","),
      paste0("FE Achievements in ", input$lep3),
      color="blue"
    )
  })

  output$skisup.FEach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
                filter(time_period=="202122",
                       LEP == input$lep4, 
                       level_or_type == "Further education and skills: Total")%>%
                      summarise(App_ach=sum(achievements))), scientific=FALSE,big.mark=","),
      paste0("FE achievements in ", input$lep4),
      color="blue"
    )
  }) 

  ### Apprentichesip achievements ----
  output$skisup.APach <- renderValueBox({
    # Put value into box to plug into app
      valueBox(
        format((C_Achieve_ILR1621 %>%
                 filter(time_period=="202122",
                        LEP == input$lep3, 
                        level_or_type == "Apprenticeships: Total")%>%
                        summarise(App_ach=sum(achievements))), scientific=FALSE,big.mark=","),
        paste0("Apprenticeship achievements in ", input$lep3),
        color="blue"
      )
  })
  
  output$skisup.APach.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      format((C_Achieve_ILR1621 %>%
                filter(time_period=="202122",
                       LEP == input$lep4, 
                       level_or_type == "Apprenticeships: Total")%>%
                      summarise(App_ach=sum(achievements))), scientific=FALSE,big.mark=","),
      paste0("Apprenticeship achievements in ", input$lep4),
      color="blue"
    )
  })

  ## Achievements over time line chart ----
  Ach_time <- reactive({
    C_Achieve_ILR1621 %>%
    select(time_period, area, LEP, level_or_type, achievements)%>%
    filter(
      #area == "England" |
      LEP == input$lep3 |
        LEP == input$lep4,
      level_or_type == input$skill_line)%>% # |
      mutate(time_period=paste(substr(time_period,3,4),"/",substr(time_period,5,6),sep = ""))%>%
    group_by(time_period, LEP, level_or_type)%>%
    summarise(Achievements=sum(achievements))%>%
    ggplot(aes(x=time_period, y=Achievements, colour = LEP, #shape = level_or_type,
                       group=interaction(level_or_type, LEP)))+
   # geom_point() + 
    geom_line()+
    theme_minimal()+
    theme(legend.position = "bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
    labs(shape = "", colour = "")+
    scale_y_continuous(label=comma)+
    ggtitle("FE and apprenticeship achievements")+
    xlab("Year")
  })
  
  output$Ach_time <- renderPlotly({
    ggplotly(Ach_time())%>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1))
  })
  
  ## Achievements pc bar chart ----
  Ach_SSA_pc <- reactive ({
    AchSSA_21 <- C_Achieve_ILR21 %>%
    filter(time_period== "202122" ,
           LEP == input$lep3 |
             LEP == input$lep4)%>%
    #  area == "England")%>%
    select(LEP, SSA=ssa_t1_desc, Achievements = achievements)%>%
    group_by(LEP, SSA)%>%
    summarise(Achievements=sum(Achievements))%>%
    ungroup()

    Ach_pc <- AchSSA_21 %>%
      filter(SSA == "Total")%>%
      select(LEP, Total=SSA, Total_ach=Achievements)
  
    Ach_SSA_pc <- AchSSA_21 %>%
      left_join(Ach_pc, by = "LEP")%>%
      group_by(LEP)%>%
      mutate(pc = Achievements/Total_ach)%>%
      filter(SSA != "Total")%>%
      ggplot(aes(x=SSA, y=pc, fill=LEP))+
      geom_bar(stat="identity",position = "dodge")+
      scale_y_continuous(labels = scales::percent)+
      scale_x_discrete(label = function(SSA) stringr::str_trunc(SSA, 12)) +#truncate labels becaus ethey can be very long
      coord_flip()+
      theme_minimal()+
      labs(fill = "",
           title ="All achievements by SSA tier 1. AY20/21")+
      theme(legend.position = "bottom",
            axis.title.x=element_blank(),axis.title.y=element_blank())
  })

  output$Ach_SSA_pc <- renderPlotly({
    ggplotly(Ach_SSA_pc())%>%
      layout(legend=list(orientation = "h", x = 0, y = -0.1))
  })
# VACANCIES ----
  
  ###Downloads---- 
  #download skills indicators
  list_of_datasets3 <- list("22.Vacancies"=C_Vacancy_ONS1722)
  output$download_btn3a <- downloadHandler(
    filename = function() { "VacancyIndicators.xlsx"},
    content = function(file) {write_xlsx(list_of_datasets3, path = file)}
  )
  
  #Download current LEP indicators
  filtered_data3  <- reactive({
    list("22.Vacancies"=filter(C_Vacancy_ONS1722,LEP == input$lep5))
  })
  output$download_btn3b <- downloadHandler(
    filename = function() {"CurrentVacancyIndicators.xlsx"},
    content = function(file) {write_xlsx(filtered_data3(), path = file)}
  )
  ## KPIs ----
   ### ONS job advert unit percent of total  ----
  output$jobad.pc <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100.*(C_Vacancy_England %>%
                filter(year == "2022",
                       LEP == input$lep5)%>%
                summarise(job.pc=sum(pc_total))), digits = 3),
        "%"),
      paste0("of total online total vacancies in England in January 2022 were in ", input$lep5),
      color="blue"
    )
  })
  
  output$jobad.pc.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100.*(C_Vacancy_England %>%
                       filter(year == "2022",
                              LEP == input$lep6)%>%
                       summarise(job.pc=sum(pc_total))), digits = 3),
        "%"),
      paste0("of total online total vacancies in England in January 2022 were in ", input$lep6),
      color="blue"
    )
  })

  ### Skill Demand KPI 2 ----
  output$jobad.ch <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100.*(C_Vacancy_England %>%
                         filter(year == "2022"|
                                  year == "2021",
                                LEP == input$lep5)%>%
                         group_by(year)%>%
                         dplyr::summarise(job.cnt=sum(vacancy_unit))%>%
                         dplyr::mutate(Row = 1:n()) %>%
                         mutate(Percentage_Change = job.cnt/lag(job.cnt)) %>%
                         ungroup%>%
                         filter(year == "2022")%>%
                         select(Percentage_Change)), digits = 3),
        "%"),
      paste0("change in online job vacancies in ", input$lep5, " January 2021 to January 2022"),
      color="blue"
    )
  })
  
  output$jobad.ch.2 <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      paste0(
        format(100.*(C_Vacancy_England %>%
                       filter(year == "2022"|
                                year == "2021",
                              LEP == input$lep6)%>%
                       group_by(year)%>%
                       dplyr::summarise(job.cnt=sum(vacancy_unit))%>%
                       dplyr::mutate(Row = 1:n()) %>%
                       mutate(Percentage_Change = job.cnt/lag(job.cnt)) %>%
                       ungroup%>%
                       filter(year == "2022")%>%
                       select(Percentage_Change)), digits = 3),
        "%"),
      paste0("change in online job vacancies in ", input$lep6, " January 2021 to January 2022"),
      color="blue"
    )
  })
  # jobad.ch <- C_Vacancy_England %>%
  #   filter(year == "2022"|
  #            year == "2021",
  #          LEP == "Black Country")%>%
  #   group_by(year)%>%
  #   dplyr::summarise(job.cnt=sum(vacancy_unit))%>%
  #   dplyr::mutate(Row = 1:n()) %>%
  #   mutate(Percentage_Change = job.cnt/lag(job.cnt)) %>%
  #   ungroup%>%
  #   filter(year == "2022")%>%
  #   select(Percentage_Change)

  ## Online job vacancy units over time line chart ----

  jobad.time <- reactive({
    C_Vacancy_England %>%
    filter(LEP == input$lep5|
             LEP == input$lep6)%>%
    select(-LA, -pc_total, -region, -England)%>%
    group_by(year, LEP)%>%
    dplyr::summarise(total = sum(vacancy_unit))%>%
    ggplot(aes(x=year, y=total, colour=LEP))+
    geom_line()+
    theme_minimal()+
    labs(colour = "LEP")+
    theme(legend.position = "bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
    ggtitle("Online job vacancy units")+
      labs(shape = "", colour = "")
  })
  
  output$jobad.time <- renderPlotly({
      ggplotly(jobad.time())%>%
        layout(legend=list(orientation = "h", x = 0, y = -0.1))
    })

  # Ach_time <- reactive({
  #   C_Achieve_ILR1621 %>%
  #     select(time_period, area, LEP, level_or_type, achievements)%>%
  #     filter(
  #       #area == "England" |
  #       LEP == input$lep3 |
  #         LEP == input$lep4,
  #       level_or_type == "Apprenticeships: Total"|
  #         level_or_type =="Further education and skills: Total") %>%
  #     group_by(time_period, LEP, level_or_type)%>%
  #     summarise(Achievements=sum(achievements))%>%
  #     ggplot(aes(x=time_period, y=Achievements, colour = LEP, shape = level_or_type,
  #                group=interaction(level_or_type, LEP)))+
  #     geom_point() + 
  #     geom_line()+
  #     theme(legend.position = "bottom")+
  #     theme_minimal()+
  #     #expand_limits(y = 0)+
  #     labs(shape = "Type", colour = "Area")+
  #     # theme(legend.position="bottom")+
  #     ggtitle("FE and Apprenticeship achievements \n 2017-2021")+
  #     xlab("Year")
  # })
  # 
  # 
  # output$Ach_time <- renderPlotly({
  #   ggplotly(Ach_time())%>%
  #     layout(legend=list(x=0.2,y=-1.5,
  #                        xanchor='left',
  #                        yanchor='bottom',
  #                        orientation='h'))
  # })

# Stop app ---------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
