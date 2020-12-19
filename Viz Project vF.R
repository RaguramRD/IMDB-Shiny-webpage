# #FIT5147 Visualisation Project
# Author: Raguram Ramakrishnasamy Dhandapani
# Student ID: 30151325
# Lab: 25
# Tutor: Frank Wu

#-----------------------------loading libraries------------------------------#

require(shiny)
require(tidyr)
require(readr)
require(dplyr)
require(ggplot2)
require(tm)
require(wordcloud)
require(RColorBrewer)
require(scales)
require(stringr)
require(plotly)
require(networkD3)
require(DT)
#----------------------------------------------------------------------------#
#Reading the input data file

#loading data
genre_analysis <- read.csv("Genre_title_type.csv")
movie_analysis <- read.csv("media_releases.csv")
search_wc_data <- read.csv("Title_names.csv")
ratings_data <- read.csv("Ratings_analysis.csv")
cast_crew_data <- read.csv("Cast_crew_details.csv")

#Data manipulation for the visualisation
genres <- levels(unique(genre_analysis$Genres))
search_wc_data$Release_Year <- as.numeric(search_wc_data$Release_Year)
genres_ratings <- levels(unique(ratings_data$Genres))

media_count <- data.frame(movie_analysis %>%
                            group_by(Media) %>%
                            summarise(count = sum(Total_Releases))
                          )


director <- cast_crew_data %>%
  filter(Category=="DIRECTOR")%>%
  arrange(desc(desc(Name)))
director_list <- unique(director$Name)



#Using shiny to create interactive visualisations
#---------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------UI Starts----------------------------------------------------#

ui <- navbarPage(

  title = "IMDb Analysis",
  position = "static-top",
#---------------------------------------TAB PANEL - 01 starts----------------------------------#
  tabPanel("Genre analysis",
           style = "border:2px solid #48ca3b;margin-top:10px;margin-left:2px;margin-right:2px;",
  fluidRow(
    h1(column(width=12, textOutput("Title_01")),style = "font-family: 'times';color:#48ca3b;")
    ),
  fluidRow(
    h3(column(width=6, textOutput("title_Introduction_01")),align='justify', style = "font-family: 'times';color: #a12e0d;"),
    h3(column(width=6, textOutput("title_pie")),align='center',style = "font-family: 'times';color: #a12e0d;")
  ),
  fluidRow(
    p(column(width=6, htmlOutput("Introduction_01")),align='justify',style = "font-family: 'times';color: #47363c ;font-si20pt"),
    column(width = 6, plotOutput("Pie_chart",width = "100%", height = "300px"))
  ),
  hr(),
  fluidRow(
    h3(column(width=12,textOutput("title_gen")), align = "center", style = "font-family: 'times'; font-si16pt;color: #a12e0d;"),
    p(column(width = 12, htmlOutput("Genre_text")), align = "justify", style = "font-family: 'times'; font-si16pt")
  ),
  br(),
  fluidRow(
    sidebarPanel(
      radioButtons("types", "Media Types", choices = c('Movie', 'TV Series', 'Video Game'), selected = 'Movie', inline=TRUE),
      selectInput("Genre", "Genres", choices = genres, selected = "Drama", multiple = TRUE),
      width = 3
      ),
    column(width = 9, plotlyOutput("Genres",width = "100%", height = "300px"))
    ),
  hr(),
  fluidRow(
    p(column(width = 12, htmlOutput("text_WC_releases")), align = "justify", style = "font-family: 'times';")
  ),
  fluidRow(
    h3(column(width=5,textOutput("title_media")), align = "center", style = "font-family: 'times'; font-si16pt;color: #a12e0d;"),
     h3(column(width=7,textOutput("title_WC")), align = "center", style = "font-family: 'times'; font-si16pt;color: #a12e0d;")
     ),
  fluidRow(
    column(width = 5,plotlyOutput("Media",width = "100%", height = "300px")),
    column(width = 3, plotOutput("WC",width = "100%", height = "300px")),    
    column(width = 4, plotOutput("BarChart",width = "100%", height = "300px"))
          ),
  hr(),
  fluidRow(
    h4(column(width = 12, htmlOutput("Page_01_02_text")), align = "right", style = "font-family: 'times';color:#f63a11;")
  ),
  hr()
  ),
#--------------------------------------------- TAB PANEL - 01 ends ---------------------------------------#

#-----------------------------------------------TAB PANEL - 02 starts -------------------------------------#
tabPanel("Top Rated",
         style = "border: 2px solid #484d79;margin-top:10px;margin-left:2px;margin-right:2px;",
         fluidRow(
           h1(column(width=12, textOutput("Title_02")),style = "font-family: 'times';color:#484d79;"),
           h4(column(width=12, textOutput("Introduction_02")),style = "font-family: 'times';color: #f40f0f;")
         ),
         hr(),
         fluidRow(
           column(width = 2, selectInput("types_02", "Media Types", choices = c('Movie', 'TV Series', 'Video Game'), selected = 'Movie',multiple = FALSE)),
           column(width = 2, selectInput("Genre_02", "Genres", choices = c('All',genres), selected = "All", multiple = FALSE)),
           column(width=3, sliderInput("NumVotes", "No of Votes",value = 200,min = 200, max = 2000000, step = 5)),
           column(width=3, sliderInput("ReleaseYear", "Release Year",value = 1960,min = 1960, max = 2020, step = 1, animate=FALSE)),
           column(width=2, textAreaInput("Text_title", "Text in Title", value="", placeholder = "enter a random text in title"))
         ),
         fluidRow(
           em(h4(column(width=12, textOutput("Filters")),align = 'left',style = "font-family: 'times';color: #c2aca6 ;font-si:14pt;"))
         ),
         br(),
         fluidRow(
           h3(column(width=12,htmlOutput("page_title")), align = "center", style = "font-family: 'times';color: #a12e0d;"),
           p(column(width=12,htmlOutput("Table_text")), align = "left", style = "font-family: 'times';")
         ),
         fluidRow(
           h3(column(width=6,textOutput("bar_title")), align = "center", style = "font-family: 'times';color: #a12e0d;"),
           h3(column(width=6,htmlOutput("table_title")), align = "center", style = "font-family: 'times';color: #a12e0d;")
         ),
         fluidRow(
           column(width=6, plotlyOutput("bar_chart", width = "100%",  height = "500px")),
           column(width=6, DT::dataTableOutput("Table_02"))
         ),
         hr(),
         fluidRow(
           h4(column(width = 12, htmlOutput("Page_02_03_text")), align = "right", style = "font-family: 'times';color:#f63a11;")
         ),
         hr()

         ),
#------------------------------------------TAB PANEL - 02 ENDS --------------------------------------#

#------------------------------------------TAB PANEL - 03 STARTS --------------------------------------#
tabPanel("Exploring Cast", 
         style = "border: 2px solid #a03b1e;",
         fluidRow(
           h1(column(width=12, textOutput("Title_03")),style = "font-family: 'times';color:#a03b1e;"),
           h4(column(width=12, textOutput("Introduction_03")),style = "font-family: 'times';color: #833582;")
         ),
         hr(),
         fluidRow(
           p(column(width = 12, htmlOutput("text_directors")),style = "font-family: 'times';color: #e75b0e;")
         ),
         fluidRow(
           column(width = 2),
           column(width = 4,selectInput("Director01", "Director Name", choices = director_list,selected = "A.L. Vijay", multiple = FALSE)),
           column(width = 4,selectInput("Director02", "Director Name", choices = director_list,selected = "A.R. Murugadoss", multiple = FALSE))
         ),
         fluidRow(
           column(12, forceNetworkOutput("force", width = "100%", height = "500px"))
         ),
         br(),
         fluidRow(
           p(column(width=12, htmlOutput("Title_cast_detail")),align='center', style = "font-family: 'times';color: #e75b0e;")
         ),
         fluidRow(
           column(6, plotlyOutput("movie_bar", width = "100%", height = "550px")),
           column(6,
             fluidRow(
               h4(column(6, htmlOutput("Direct_01_WC_text")), align='center', style = "font-family: 'times';color: #513111;"),
               h4(column(6, htmlOutput("Direct_02_WC_text")), align='center', style = "font-family: 'times';color: #513111;"),
               column(6,plotOutput("Direc_01_WC",width = "100%", height = "200px")),
               column(6,plotOutput("Direc_02_WC",width = "100%", height = "200px"))
             ),
             br(),
             fluidRow(
               h4(column(12, htmlOutput("table_title_cast")), align = 'center', style = "font-family: 'times';color: #513111;"),
               column(12,tableOutput("cast_table"), align='center')
             )
           )
         ),
         hr(),
         br()
         
         )
#------------------------------------------TAB PANEL - 03 ENDS --------------------------------------#

)

#---------------------------------------------------------UI Ends------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------SERVER starts-----------------------------------------------#

server <- function(input, output, session) {

#---------------------------------------TEXT PARTS OF THE VIZ - PAGE 01----------------------------------------------#
  
  output$Title_01 <- renderText(paste0("Looking at the Trends of Genres across years..."))
  output$Introduction_01 <- renderText(paste0("The Media Industry has evolved over the last century from making small videos and shows to a multi-million dollar industry today. <B>IMDb (Internet Movie Database)</B> is an online database containing information on Films, TV Series and Video games, along with cast, production crew, fan reviews and Ratings. Based on the data from IMDb, 
this page and susequet pages show the trend of the media industry over the last 60 years (1960-2020). Across the whole spectrum of media, 
Movies, TV Series and Video Games are considered for the current scenario, as these three are the current dominants of the entertainment industry.",hr(),
"The Chart to the right, shows the total number of Movies, TV series and Video Games that were released in the last 60 years. 
As seen from it the Movies have the majority of releases followed by TV series and Video Games."))
  output$Dummy2 <- renderText(paste0(""))
  output$title_pie <- renderText(paste0("The Split of Total releases in last 60 years"))
  output$title_gen <- renderText(paste0("The Trend of Genres for ",input$types,"s across years"))
  output$title_media <- renderText(paste0("Number of releases for ",input$types,"s across years"))
  output$title_Introduction_01 <-  renderText(paste0("Introduction"))
  output$Genre_text <- renderText(paste0("For the three different media considered, the below graph shows
                                         the trend of different genres across time. By selecting any one of the media and 
                                         selecting different genres from the list, the graph shows their trends in the past 60 years.\n
                                         Inferring from the them, the <B> Drama & Comedy </B> Genre has dominated the Movie and TV series industry,
                                         while most of the Video Games released were in the Genre of <B> Action & Adventure </B>"))
  output$Page_01_02_text <- renderText(paste0("<B>Go onto the next Page to look at the Top-rated items in each of the media...</B>"))
  output$text_WC_releases <- renderText(paste0("This section of the page shows the Total number of releases in a year for a media and the associated top genres based on selected year. Click on a column in the chart to left to see the top genres for that year.."))
  
#----------------------------------------TEXT PARTS OF THE VIZ - PAGE 01----------------------------------------------#
  
  
#-------------------------------------------------------PAGE 01 start---------------------------------------#  
  output$Pie_chart <- renderPlot({
    pie <- ggplot(media_count,aes(x="",y=count, fill=Media))+
      geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y", start=0) +
      theme(legend.position="bottom")+
      geom_text(aes(label = paste0( number(count,big.mark = ","))), position = position_stack(vjust = 0.5))+
      theme_void()
    pie
  })
  
  output$Media <- renderPlotly({
    plot_2 <- ggplot(subset(movie_analysis, movie_analysis$Media==input$types), aes(Release_Year, Total_Releases))+geom_bar(stat = 'identity') +
      theme(axis.line = element_line(colour = "black")) + theme(legend.position="none")+
      theme(panel.background = element_rect(fill='white'), plot.background = element_rect(fill='white'))+
      xlab("Release Year") + ylab("Total releases")
    ggplotly(plot_2, tooltip = c("x", "y"), source = "myClickSource") %>% config(displayModeBar = F)
  })
  
  observe({
    myClicks <- event_data("plotly_click", source = "myClickSource")
    year <- myClicks$x
    
    output$WC <- renderPlot({
    if(!is.null(year))
    {
       output$title_WC <- renderText(paste0("The Top Genres across ", input$types,"s in the year - ",year))
       year <- as.numeric(year)
       subdata <- subset(search_wc_data, search_wc_data$Media==input$types & search_wc_data$Release_Year == year)
    }
    else
    {
      output$title_WC <- renderText(paste0("The Top Genres across ", input$types,"s in all years"))
      subdata <- subset(search_wc_data, search_wc_data$Media==input$types)
    }
    word_df <- data.frame(table(subdata$Genres))
    names(word_df) <- c("word", "freq")

    par(mar = rep(0, 4))
    wordcloud(words = word_df$word, freq = word_df$freq, scale = c(3,1), 
              random.order=FALSE,colors=brewer.pal(8, "Dark2"))
      })
  })
  
  observe({
    Clicks <- event_data("plotly_click", source = "myClickSource")
    year <- Clicks$x
    
    output$BarChart <- renderPlot({
      if(!is.null(year))
      {
        year <- as.numeric(year)
        subdata <- subset(search_wc_data, search_wc_data$Media==input$types & search_wc_data$Release_Year == year)
      }
      else
      {
        subdata <- subset(search_wc_data, search_wc_data$Media==input$types)
      }
      
      word_df <- data.frame(table(subdata$Genres))
      names(word_df) <- c("word", "freq")
      word_df <- word_df[order(word_df$freq, decreasing =TRUE),]

      word_df$word <- factor(word_df$word, levels=unique(word_df$word))
      plot_3 <- ggplot(head(word_df, 7), aes(word, freq))+geom_bar(aes(fill = word), stat = 'identity')+
        theme(axis.line = element_line(colour = "black")) + theme(legend.position="none")+
        theme(panel.background = element_rect(fill='white'), plot.background = element_rect(fill='white'))+
        xlab("Genres") + ylab("Total releases")
      plot_3
    })
  })
  
  output$Genres <- renderPlotly({
    plot_1 <- ggplot(subset(genre_analysis, genre_analysis$Media==input$types & genre_analysis$Genres %in% input$Genre), aes(Release_Year, Total_Releases, color = Genres))+geom_point() + geom_line()+
      theme(axis.line = element_line(colour = "black")) +theme(legend.position="right")+
      theme(panel.background = element_rect(fill='white'), plot.background = element_rect(fill='white'))+
      xlab("Release Year") + ylab("Total releases")
    ggplotly(plot_1, tooltip = c("x", "y", "Genres")) %>% config(displayModeBar = F)
  })
#-------------------------------------------------------PAGE 01 end----------------------------------------------#  

#-------------------------------------------------------PAGE 02 start--------------------------------------------# 
  
  output$Title_02 <- renderText(paste0("Finding out the Top Rated media in the Industry..."))
  output$Introduction_02 <- renderText(paste0("Its always curious to find out leaders in the movies and games industry who have topped the tables with their performances.
This page shows the top rated (top 20) media in the respective fields. Along with the top 20 you can also check out the competitors in the field with the same rating."))

  output$Table_text <- renderText(paste0("This section shows us the Top 20 ratings in the selected list of media based on the filters.
  The chart lists down the <B> top item </B> among the top 20 ratings and the table to the right dynamically shows all the other movies/tv series/video games with the same RATING sorted by the Number of Votes."))
  output$page_title <- renderText(paste0("The Top 20 Ratings for ",input$types_02, "s"))
  output$Filters <- renderText(paste0("Please select the relevant filters from the above options."))
  output$bar_title <- renderText(paste0("The Top Voted item in each class of Rating for ", input$types_02))
  output$Page_02_03_text <- renderText(paste0("<B> Go onto the next page to explore the Network of Cast and Crew..</B>"))
  
  output$bar_chart <- renderPlotly({
    
    if(input$Text_title=="")
    {
      data_1 <- subset(ratings_data,Media == input$types_02 & Num_of_Votes >input$NumVotes & Release_Year>=input$ReleaseYear)
    }
    else
    {
      data_1 <- subset(ratings_data,Media == input$types_02 & Num_of_Votes >input$NumVotes & Release_Year>=input$ReleaseYear) %>%
        filter(str_detect(tolower(Title), tolower(input$Text_title)))
    }
    
    if(input$Genre_02=='All')
    {
      data2 <- data_1 %>%
                     arrange(desc(Rating), desc(Num_of_Votes)) %>%
                     select(Title, Release_Year,Rating, Num_of_Votes,Genres) 
      
      data  <- head(data2  %>%
                      group_by(Rating) %>% 
                      filter(Num_of_Votes==max(Num_of_Votes)) %>% 
                      ungroup(),20)
    }
    else
    {
      data2 <- data_1  %>%
                     filter(str_detect(tolower(Genres), tolower(input$Genre_02))) %>%
                     arrange(desc(Rating), desc(Num_of_Votes)) %>%
                     select(Title, Release_Year,Rating, Num_of_Votes,Genres)
                   
      data  <- head(data2  %>%
                    group_by(Rating) %>% 
                      filter(Num_of_Votes==max(Num_of_Votes)) %>% 
                      ungroup(),20)
    }


    data$Num_of_Votes <- factor(data$Num_of_Votes, levels = data$Num_of_Votes)
    plot_1 <- ggplot(data)+ geom_col(aes(x=Num_of_Votes, y=Rating, fill=Title))+ 
      geom_text(aes(x=Num_of_Votes, y=Rating,label= Rating),vjust=-1,size=3)+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+ 
      theme(legend.position="none")+ theme(axis.line = element_line(colour = "black")) +
      theme(panel.background = element_rect(fill='white'), plot.background = element_rect(fill='white'))+
      ylab("Average User Rating")+ coord_flip()+scale_x_discrete(limits = rev(levels(data$Num_of_Votes)))
    
    ggplotly(plot_1, tooltip=c("Title"), source = "bar_click") %>%  config(displayModeBar = F)
    
  })
  
  observe({

    clicks <- event_data("plotly_click", source = "bar_click")
    rat <-clicks$x
    
    output$Table_02 <- DT::renderDataTable({
     
      if(input$Text_title=="")
      {
        data_1 <- subset(ratings_data,Media == input$types_02 & Num_of_Votes >input$NumVotes & Release_Year>input$ReleaseYear)
      }
      else
      {
        data_1 <- subset(ratings_data,Media == input$types_02 & Num_of_Votes >input$NumVotes & Release_Year>input$ReleaseYear) %>%
          filter(str_detect(tolower(Title), tolower(input$Text_title)))
      }
      
      if(is.null(rat))
      {
        rat=max(data_1$Rating, na.rm = FALSE)
      }
      else
      {
        rat <- as.numeric(clicks$x)
      }
      output$table_title <- renderText(paste0("The ",input$types_02,"s with the rating of ", rat))
      
      if(input$Genre_02=='All')
      {
        if (input$types_02=='TV Series'){
          data <- data_1 %>%
            filter(Rating==rat) %>%
            arrange(desc(Num_of_Votes)) %>%
            select(Title, Release_Year, Num_of_Votes, Genres)
        }
        else{
          data <- data_1 %>%
            filter(Rating==rat) %>%
            arrange(desc(Num_of_Votes)) %>%
            select(Title, Director, Release_Year, Num_of_Votes)
        }
        
      }
      else
      {
        if (input$types_02=='TV Series'){
          data <- data_1  %>%
            filter(str_detect(tolower(Genres), tolower(input$Genre_02)) & Rating==rat) %>%
            arrange(desc(Num_of_Votes)) %>%
            select(Title, Release_Year, Num_of_Votes, Genres)
        }
        else
        {
          data <- data_1  %>%
            filter(str_detect(tolower(Genres), tolower(input$Genre_02)) & Rating==rat) %>%
            arrange(desc(Num_of_Votes)) %>%
            select(Title, Director, Release_Year, Num_of_Votes)
        }
        
      }
      DT::datatable(
        data, rownames = FALSE,
        options = list(
          dom = 'irtp'
          )
        )
    })
    
  })
  
#-------------------------------------------------------PAGE 02 end----------------------------------------------# 

#-------------------------------------------------------PAGE 03 start----------------------------------------------# 
  output$Title_03 <- renderText(paste0("Relationship between the Cast and Crew..."))
  output$Introduction_03 <- renderText(paste0("The Movie industry is one of the biggest and the top media in the entertainment industry.
                                              More than 70% of the media market is dominated by it. Most crucial factors of the movie industry are the directors and cast who make sure the movie reaches the quality needed.
                                              With a lot of movies releasing world wide and the count increasing every year, In this page we will look at the top directors and the casting involved in thier movies."))
  
  output$Title_cast_detail <- renderText(paste0("This section is for the detailed analysis of the two directors and thier movies. Genres handled by directors, thier movies in order of rating and the crew for each of them is shown in this section.\n <B> Click on the bar to see the crew of a movie!!</B>"))
  output$text_directors <- renderText(paste0("This section of the page revolves around any of the two directors of your choice, and they can be selected from the list below. The network of the directors with their films and respective crew
                                             members is shown just below <em>(click on a node to see their IMDb webpage)</em> and the details of cast are at the bottom of the page."))
  output$force <- renderForceNetwork({
      df1 <- cast_crew_data %>%
        subset(Name %in% c(input$Director01, input$Director02) & Category=='DIRECTOR')
      d <- df1[,c(1,2,4,7,12,13)]
      new_df <- subset(cast_crew_data, tconst %in% d$tconst) %>%
        filter(!(Name  %in% c(input$Director01, input$Director02)) & Category !='DIRECTOR')
      
      newdf <- new_df[,c(1,2,4,13)]
      fin_df1 <- left_join(newdf,d ,by="tconst")
      fin_df <- fin_df1[c(3,6,7,1)]
      
      fin_df <- fin_df %>%
        filter(Name.x != '<NA>')

      graph_df <- data.frame(rbind(as.matrix(fin_df[c('Name.y', 'Title')]),
                                   as.matrix(fin_df[c('Title', 'Name.x')])))

      graph_new <- data.frame(graph_df,stringsAsFactors = FALSE)

      
      list1 <- as.character(unlist(graph_new$Name.y))
      list2 <- as.character(unlist(graph_new$Title))
      
      nodes <- data.frame(name = unique(c(list1, list2)), stringsAsFactors = FALSE)
      nodes$id <- 0:(nrow(nodes) - 1)
      
      #getting links for movies, cast
      dr <- unique(d %>%
                      select(Name, Name_link))
      mov <- unique(d %>%
                      select(Title,Title_link))
      cst <- unique(new_df %>%
                      filter(Name != '<NA>') %>%
                      select(Name, Name_link))
      names(dr) <- c("name", "link")
      names(mov) <- c("name", "link")
      names(cst) <- c("name", "link")
      
      finalDf <- bind_rows(dr, mov, cst)
      
      
      #Calculating edges and nodes
      edges <- graph_new %>%
        left_join(nodes, by = c("Name.y" = "name")) %>%
        select(-Name.y) %>%
        rename(source = id) %>%
        left_join(nodes, by = c("Title" = "name")) %>%
        select(-Title) %>%
        rename(target = id)

      
      nodes$group <- ifelse(nodes$name %in% c(input$Director01, input$Director02), "Director", ifelse(nodes$name %in% fin_df$Title, "Movie", "Cast & Crew"))

      nodes <- left_join(nodes, finalDf,by="name") 

      
      ColourScale <- 'd3.scaleOrdinal()
                    .domain(["Director", "Movie", "Cast & Crew"])
                    .range(["#F70E1B", "#0E19F7", "#08F11B"]);'
      
      script <- 'window.open(d.var1);'

      fn <- forceNetwork(Links = edges, Nodes = nodes, 
                         Source = "source",
                         Target = "target",
                         NodeID ="name",
                         Group = "group",
                         Value = 1,
                         opacity = 1,
                         zoom = TRUE,
                         fontSize = 12,
                         fontFamily = 'times',
                         colourScale = JS(ColourScale),
                         legend = TRUE,
                         opacityNoHover = TRUE,
                         linkWidth = 1,
                         clickAction = script)
      
      fn$x$nodes$var1 <- nodes$link
      
      fn
  })
  
  output$movie_bar <- renderPlotly({
    
    list_1 <- cast_crew_data %>%
      subset(Name %in% c(input$Director01, input$Director02) & Category=='DIRECTOR')

    list_2 <- subset(cast_crew_data, tconst %in% list_1$tconst) %>%
      filter(!(Name  %in% c(input$Director01, input$Director02)) & Category != 'DIRECTOR')
    
    d1 <- list_1[,c(1:4,7,12,13)]
    d2 <- list_2[,c(1:4,10,13)]
    
    d3 <- left_join(d2,d1,by="tconst") 
    
    d4 <- d3 %>%
            group_by(tconst, Title, Name.y, Rating) %>%
            summarise(n=n()) %>%
      arrange(desc(Rating))
    names(d4) <- c("tconst", "Title", "Director", "Rating", "count")
    d4$tconst <- factor( d4$tconst, levels =  d4$tconst)

    plot_25 <- ggplot(d4)+ geom_col(aes(x=tconst, y=Rating, fill=Director,label=Title))+
      geom_text(aes(x=tconst, y=Rating,label= Rating),size=3)+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+ theme(legend.position='top')+
      theme(axis.line = element_line(colour = "black")) +
      theme(panel.background = element_rect(fill='white'), plot.background = element_rect(fill='white'))+
      ylab("Average User Rating")+ coord_flip()+scale_x_discrete(limits = rev(levels(d4$tconst)))

    ggplotly(plot_25, tooltip=c("Title"), source = "bar_click_1") %>%  config(displayModeBar = F)
  })
  
  output$Direct_01_WC_text <- renderText(paste0("Genres Handled by <B>",input$Director01,"</B>"))
  output$Direct_02_WC_text <- renderText(paste0("Genres Handled by <B>",input$Director02,"</B>"))
  
  output$Direc_01_WC <- renderPlot({
   
     list_d1 <- cast_crew_data %>%
      filter(Name == input$Director01 & Category=='DIRECTOR') %>%
       select(Name, Genres)
                         
    word_df1 <- data.frame(table(unlist(str_split(list_d1$Genres, ","))))
    names(word_df1) <- c("word", "freq")
    
    par(mar = rep(0, 4))
    wordcloud(words = word_df1$word, freq = word_df1$freq,min.freq = 1,  scale=c(3,0.5),
              random.order=FALSE,colors=brewer.pal(8, "Dark2"))
  })
  
  output$Direc_02_WC <- renderPlot({
    list_d2 <- cast_crew_data %>%
      filter(Name == input$Director02 & Category=='DIRECTOR') %>%
      select(Name, Genres)
    
    word_df2 <- data.frame(table(unlist(str_split(list_d2$Genres, ","))))
    names(word_df2) <- c("word", "freq")
    
    par(mar = rep(0, 4))
    wordcloud(words = word_df2$word, freq = word_df2$freq,min.freq = 1, scale=c(3,0.5),
              random.order=FALSE,colors=brewer.pal(8, "Dark2"))
  })
  
  observe({

    clickas <- event_data("plotly_click", source = "bar_click_1")
    id <- clickas$y

    
    output$cast_table <- renderTable({
      list_1 <- cast_crew_data %>%
        filter(Name %in% c(input$Director01, input$Director02) & Category=='DIRECTOR')
      
      list_2 <- subset(cast_crew_data, tconst %in% list_1$tconst) %>%
        filter(!(Name  %in% c(input$Director01, input$Director02)) & Category !='DIRECTOR' )

      d1 <- list_1[,c(1,4,7,9)]
      d2 <- list_2[,c(1,3,4,5,6,8)]
      
      d3 <- left_join(d1,d2,by="tconst") %>%
        filter(Name.y !='NA') %>%
        mutate(Current_Age = ifelse(BirthYear=='NA','NA',paste0(2019-BirthYear," years"))) %>%
        select(tconst, Category, Name.y,BirthYear, Current_Age, Release_Year) 
     
      names(d3) <- c("tconst", "Category", "Name", "BirthYear",  "Current_Age", "Release_Year")
      
      #getting necessary data for table
      d11 <- list_1[,c(1:4,7,12,13)]
      d21 <- list_2[,c(1:4,10,13)]

      d31 <- left_join(d21,d11,by="tconst")

      d41 <- data.frame(d31 %>%
        group_by(tconst, Title, Name.y, Rating) %>%
        summarise(n=n()) %>%
        arrange(desc(Rating)))
      names(d41) <- c("tconst", "Title", "Director", "Rating", "count")
      #d41$tconst <- factor( d41$tconst, levels =  d41$tconst)
      
      if (is.null(id))
      {
        tcons <-as.factor(d41[1,1])
      }
      else
      {
        tcons <- as.factor(d41[nrow(d41)-id+1,1])
      }
      mov <-   (d41 %>%
                      filter(tconst==tcons) %>%
                      select(Title))[1,1]
      
      dir <-    (d41 %>%
                     filter(tconst==tcons) %>%
                     select(Director))[1,1]
     
      yr <- as.character(unique(d3 %>%
                                   filter(tconst==tcons) %>%
                                   select(Release_Year)))


      output$table_title_cast <- renderText(
        paste0("Cast of the movie <B>", mov, "</B> directed by <B>", dir, "</B> released on ", yr)
      )
      
      d3 %>%
        filter(tconst==tcons) %>%
        select(Name, Category, Current_Age)
      
    })
  })

#-------------------------------------------------------PAGE 03 end----------------------------------------------# 
  
}
#--------------------------------------------------------------SERVER ends-------------------------------------------------#
#------------------------------------------------------------------------------------------------------#

shinyApp(ui = ui, server= server)