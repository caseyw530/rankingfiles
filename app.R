library(geosphere)
library(bslib)
library(shiny)

options(device.ask.default = FALSE)
ui <-  fluidPage(
  theme = bs_theme(
    bootswatch  = "cerulean"
  ),
  tabsetPanel(
    tabPanel("What we are", fluid = TRUE,
             headerPanel("College Rankings are confusing!"),
             textOutput("main")
    ),
    tabPanel("Generate a List!", fluid= TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("geo", "How important is geographic proximity when considering a school?", 
                             min=0, max=10, value=0),
                 textInput('zip', "What is the ZIP code you'd like to be close to?"),
                 sliderInput("size", "How important is a school's size to you? (i.e. do you care if a school is large, medium or small?)", 
                             min=0, max=10, value=0),
                 selectInput("size_pref", "How big would your ideal college be?", c("Small: <5000", "Medium: Between 5000 and 15000", "Large: >15000")),
                 sliderInput("loc", "How important is a school's setting to you? (i.e. urban, suburban or rural?)", 
                             min=0, max=10, value=0),
                 selectInput("loc_pref", "What setting would your ideal college be?", c("Rural", "Suburban", "Urban")),
                 sliderInput("academic", "In your opinion, how important is a college's academic prestige relative to these other factors?", 
                             min=0, max=10, value=0),
                 sliderInput("race", "How important is racial diversity to you when looking at schools?", 
                             min=0, max=10, value=0),
                 sliderInput("avail", "How important is the availability of financial aid resources when choosing a school?", 
                             min=0, max=10, value=0),
                 sliderInput("afford", "How important is the affordability of a school when searching for colleges?", 
                             min=0, max=10, value=0),
                 sliderInput("gender", "How important is gender equity to you when looking at schools?", 
                             min=0, max=10, value=0),
                 radioButtons("test_yn", "Are you planning on submitting test scores?", 
                              c("Yes", "No")),
                 textInput("tstscore", "What was your SAT score?", value = "1000"),
                 sliderInput("test", "How well would you like your test scores to fit with the college, relative to these other factors?",
                             min=0, max=10, value=0),
                 sliderInput("res", "How much would a school's resources affect your decision on choosing a college?",
                             min=0, max=10, value=0),
                 sliderInput("outcomes", "How important is the alumni outcomes of a college relative to the other factors listed here?", 
                             min=0, max=10, value=0),
                 sliderInput("ret", "How important are retention  and completion rates when considering a college?", 
                             min=0, max=10, value=0),
                 sliderInput("comp", "How 'safe' of a list would you like to generate? (With a zero not considering competitiveness of admissions at all and a ten weighing noncompetitiveness it highly)", 
                             min=0, max=10, value=0),
                 actionButton("generate", "Generate List", class = "btn-success"),
               ),
               mainPanel(
                 dataTableOutput('table')
               )
             )
    )
  )
)

server <- function(input, output, session) {
  output$main <- renderText({
    "We're all familiar with college rankings and the problems that face them. 
    Even as early as 2005, over 50% of students applying to colleges say that 
    rankings are either important or very important factors in deciding which 
    colleges they will apply to, and with students applying to more schools, 
    that number has only gone up. Meanwhile, big companies who produce these
    lists are not particularly transparent. Not only does this make their lists
    harder to evaluate as an applicant, but it also leads to the recent controversies
    surrounding the most popular of these lists. I remember searching for colleges
    and feeling completely lost, especially coming from a big public school. That
    is why I developed this app. The goal is to allow you to customize rankings
    of colleges to your specifications, while also allowing for some transparency
    in the search process. This app was developed for a computing seminar I am taking at Kenyon
    College, and it is meant to be a culmination of my research into college
    rankings. That being said, the content included should be generally comprehensible
    it extends beyond that and interacts heavily with my work in economics,
    and it extends to my experience in mathematics, history and college admissions. 
    Feel free to play around with the app, and if you have any feedback, reach out to me at 
    caseywatkins530@gmail.com. I hope you find it useful, whether you want to sort
    through individual metrics or you want to search for a college."
  })
  out <- reactive({
    shiny::validate(
      need(is.numeric(as.numeric(input$zip)) == TRUE, message = "Please input a valid five-digit ZIP code")
      
    )
    input$generate
    size_num = 0
    loc_num = 0
    isolate(
      if(input$size_pref == "Small: <5000") {
        size_num = 1
      }
      else if(input$size_pref == "Medium: Between 5000 and 15000") {
        size_num = 2
      }
      else {
        size_num = 3
      }) 
    
    isolate (
      if(input$loc_pref == "Urban") {
        loc_num = 1
      }
      else if(input$loc_pref == "Suburban") {
        loc_num = 2
      }
      else {
        loc_num = 3
      })
    isolate({
      inputd <- read.csv("usedata.csv")
      zipdata <- read.csv("zipdata.csv")
      gen_list <- function(GEO_WT = geo, ZIP = zip, AP_WT = academic, SIZE_WT = size, SIZE = size_pref, 
                           RACE_WT = race, AFF_WT = afford, FA_WT = avail, TEST_YN = test, SAT_WT = tst_pref, 
                           SAT = tstscore, RES_WT = res, GEN_WT = gender, OUT_WT = outcomes, RET_WT = retention,
                           COMP_WT = compete, LOC = loc_pref, LOC_WT = loc) {
        find = 7
        for (i in 1:33788) {
          if(ZIP == zipdata$ZIP[i]) {
            find = i
          }
        }
        lati = zipdata$lat[find]
        long = zipdata$lng[find]
        for (i in 1:1344) {
          lat = inputd$LATITUDE[i]
          lng = inputd$LONGITUDE[i]
          inputd$dist[i] <<- distHaversine(c(lng, lat), c(long, lati))
        }
        for (i in 1:1344) {
          inputd$DIST_GEO[i] <<- ((3.056103413*(10^-13)*(inputd$dist[i])^2) - (0.00000781422745525 * (inputd$dist[i])) + 100)
        }
        small = 100
        for (i in 1:1344) {
          if(inputd$DIST_GEO[i] < small) {
            small <- inputd$DIST_GEO[i]
          }
        }
        for (i in 1:1344) {
          inputd$DIST_GEO[i] <<- ((inputd$DIST_GEO[i] - small) * (100 / (100 - small)))
        }
        for (i in 1:1344) {
          if(inputd$SAT_AVG_ALL[i] > SAT) {
            inputd$TST_RANK[i] <<- 100 - (inputd$SAT_AVG_ALL[i] - SAT) * (100 / (1566 - SAT))
          }
          else {
            inputd$TST_RANK[i] <<- (inputd$SAT_AVG_ALL[i] - 785) * (100 / (SAT - 785))
          }
        }
        
        if (LOC == 1) {
          for (i in 1:1344) {
            if(inputd$LOCALE[i] == 1) {
              inputd$LOC_RANK[i] <<- 100
            }
            else if(inputd$LOCALE[i] == 2) {
              inputd$LOC_RANK[i] <<- 50
            }
            else {
              inputd$LOC_RANK[i] <<- 0
            }
          }
        }
        
        else if (LOC == 2) {
          for (i in 1:1344) {
            if(inputd$LOCALE[i] == 1) {
              inputd$LOC_RANK[i] <<- 50
            }
            else if(inputd$LOCALE[i] == 2) {
              inputd$LOC_RANK[i] <<- 100
            }
            else {
              inputd$LOC_RANK[i] <<- 50
            }
          }
        }
        
        else {
          for (i in 1:1344) {
            if(inputd$LOCALE[i] == 1) {
              inputd$LOC_RANK[i] <<- 0
            }
            else if(inputd$LOCALE[i] == 2) {
              inputd$LOC_RANK[i] <<- 50
            }
            else {
              inputd$LOC_RANK[i] <<- 100
            }
          }
        }
        
        TOT_WT = GEO_WT + AP_WT + SIZE_WT + RACE_WT + AFF_WT + FA_WT + ifelse(TEST_YN == "YES", SAT_WT, 0) + RES_WT + GEN_WT + OUT_WT + RET_WT + COMP_WT + LOC_WT
        GEO_M = GEO_WT / TOT_WT
        AP_M = AP_WT / TOT_WT
        SIZE_M = SIZE_WT / TOT_WT
        RACE_M = RACE_WT / TOT_WT
        AFF_M = AFF_WT / TOT_WT
        FA_M = FA_WT / TOT_WT
        SAT_M = SAT_WT / TOT_WT
        RES_M = RES_WT / TOT_WT
        GEN_M = GEN_WT / TOT_WT
        OUT_M = OUT_WT / TOT_WT
        RET_M = RET_WT / TOT_WT
        COMP_M = COMP_WT / TOT_WT
        LOC_M = LOC_WT / TOT_WT
        if (TEST_YN == "Yes") {
          if (SIZE == 1) {
            for (i in 1:1344) {
              inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_TEST[i] + SIZE_M * inputd$SIZE_SMALL[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + COMP_M * inputd$COMPETE[i] + LOC_M * inputd$LOC_RANK[i])
            }
          }
          else if (SIZE == 2) {
            for (i in 1:1344) {
              inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_TEST[i] + SIZE_M * inputd$SIZE_MED[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
            }
          }
          else {
            for (i in 1:1344) {
              inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_TEST[i] + SIZE_M * inputd$SIZE_LARGE[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
            }
          }
        }
        else {
          if (SIZE == 1) {
            for (i in 1:1344) {
              inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_NOTEST[i] + SIZE_M * inputd$SIZE_SMALL[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + SAT_M * inputd$TST_RANK[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
            }
          }
          else if (SIZE == 2) {
            for (i in 1:1344) {
              inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_NOTEST[i] + SIZE_M * inputd$SIZE_MED[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + SAT_M * inputd$TST_RANK[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
            }
          }
          else {
            for (i in 1:1344) {
              inputd$SCORE[i] <<- (GEO_M * inputd$DIST_GEO[i] + AP_M * inputd$PRESTIGE_NOTEST[i] + SIZE_M * inputd$SIZE_LARGE[i] + RACE_M * inputd$RACE_DIVERSITY[i] + AFF_M * inputd$AFFORD[i] + FA_M * inputd$FIN_AVAIL[i] + GEN_M * inputd$GENDER_EQUITY[i] + OUT_M * inputd$OUTCOMES[i] + RET_M * inputd$RET_AND_COMPLETE[i] + SAT_M * inputd$TST_RANK[i]+ COMP_M * inputd$COMPETE[i]+ LOC_M * inputd$LOC_RANK[i])
            }
          }
        }
        
        return(inputd)
      }
      })
    
    out <- isolate(gen_list(GEO_WT = input$geo, ZIP = input$zip, AP_WT = input$academic, SIZE_WT = input$size, SIZE = size_num, 
                            RACE_WT = input$race, AFF_WT = input$afford, FA_WT = input$avail, TEST_YN = input$test_yn, SAT_WT = input$test, 
                            SAT = as.numeric(input$tstscore), RES_WT = input$res, GEN_WT = input$gender, OUT_WT = input$outcomes, RET_WT = input$ret, 
                            COMP_WT = input$comp, LOC = loc_num, LOC_WT = input$loc))
    out <- isolate(out[order(-out$SCORE), ])
    return(out)
    size_num <<- 0
    loc_num <<- 0
  })
  output[["table"]] <- renderDataTable(out())
}


shinyApp(ui = ui, server=server)