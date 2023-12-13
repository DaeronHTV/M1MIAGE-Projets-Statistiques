library(shiny)
library(shinydashboard)
if (!require("DT")) install.packages('DT')
if (!require("shinyBS")) install.packages("shinyBS")
# Define UI ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Projet Statistiques"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Presentation du jeu de donnees", tabName="intro", icon = icon("dashboard")),
      menuItem("Analyses Quanti*Quali", tabName = "Statistiques", icon = icon("bar-chart"),
               menuSubItem("Genre*EU_Sales", tabName = "GEUS"),
               menuSubItem("Genre*Global_Sales", tabName = "GGS"),
               menuSubItem("Platform*EU_Sales", tabName = "PLEUS"),
               menuSubItem("Platform*Global_Sales", tabName = "PLGS")
      ),
      menuItem("Analyse Quali*Quali", tabName = "QualiQuali", icon = icon("bar-chart")),
      menuItem("Analayse Quanti*Quanti", tabName = "QuantiQuanti", icon = icon("bar-chart")),
      menuItem("Database", tabName = "Database", icon = icon("database")),
      menuItem("About", tabName = "About", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #####################################
      #            INTRODUCTION           #
      #####################################
      tabItem(tabName = "intro",
        h2("Presentation du jeu de donnees vgslaes.csv"),
        p("La base de donnees vgsales contient une liste de jeux videos dont les ventes sont superieures a 100 000 
          exemplaires en 2016. Cet ensemble de donnees a ete genere par un extrait de https://www.vgchartz.com. Elle est 
          constituee de 16 598 individus differents, et de 11 variables. Nous verrons par la suite si nous conserverons la 
          totalite des individus, ou si nous utiliserons seulement un echantillon de la base pour l'analyse, en fonction des 
          variables choisies."),
        h3("Description des variables"),
        fluidRow(tag("b","Nom de la variable"),tag("b","Description de la variable")),
        fluidRow("Rank "," => "," Classement des ventes globales"),
        fluidRow("Name "," => "," Nom du jeu"),
        fluidRow("Platform "," => "," Plateforme de la version des jeux (WII, PC, PS4, .)"),
        fluidRow("Year "," => "," Annee de sortie du jeu"),
        fluidRow("Genre "," => "," Genre du jeu"),
        fluidRow("Publisher "," => "," Editeur du jeu"),
        fluidRow("NA_Sales "," => "," Ventes en Amerique du Nord (en millions d'Euros)"),
        fluidRow("EU_Sales "," => "," Ventes en Europe (en millions d'Euros)"),
        fluidRow("JP_Sales "," => "," Ventes au Japon (en millions d'Euros)"),
        fluidRow("Other_Sales "," => "," Ventes dans le reste du monde (en millions d'Euros)"),
        fluidRow("Global_Sales "," => "," Ventes mondiales totales (en millions d'Euros)"),
        h3("Valeur manquantes"),
        p("On peut remarquer que cette base de donnees n'est pas totalement complete, nous pouvons voir qu'il y a parfois la 
          valeur N/A. En effet, nous la retrouvons 271 fois dans la colonne Year (c'est le cas par exemple pour l'individu 180), 
          et 58 fois dans la colonne Publisher (c'est le cas par exemple pour l'individu 1664). Quelques individus ont comme donnees 
          *manquantes l'annee, mais aussi l'editeur, c'est le cas pour l'individu 471. Au total, cela fait 329 donnees manquantes 
          correspondant a 307 individus.
          Pour realiser notre analyse, nous ne prendrons pas en compte ces individus."),
        h3("Choix des variables etudiees"),
        p("Nous avons choisi d'etudier 4 variables parmi les 11 de la base de donnees:"),
        tag("li","2 variables qualitatives nominales : Platform et Genre"),
        tag("li", "2 variables quantitatives continues : EU_Sales et Global_Sales")
 
      ),
      #####################################
      #     STATISTIQUES MULTIVARIEES     #
      #####################################
      ####    Analyse Quanti*Quali     ####
      tabItem(tabName = "GEUS",
        h2("Analyse des variables Genre * EU_Sales"),
        sidebarLayout(
          sidebarPanel(
            h3("Filtre"),
            selectInput("GenreGEUS", "Choix du Genre", c("Action" = "Action", "Strategy" = "Strategy", "Misc" = "Misc")),
            verbatimTextOutput("sumGenreEuSales"),br(),width = 4
          ),
          mainPanel(
            plotOutput("histGenreEuSales"),br(),
            shinyBS::bsCollapse(id = "collapseAbout",
              shinyBS::bsCollapsePanel("Test des moyennes sur les ventes europeennes entre les genres de jeux Action et Strategy", 
                textOutput("textTestVarEuGenre"),br(),
                verbatimTextOutput("testVarEuGenre"),
                textOutput("textEuGenre1"),br(),
                verbatimTextOutput("testPValueEuGenre"),br(),
                textOutput("textEuGenre2"),
                style = "info"
              )
            ),width=8
          )
        ),
      ),
      tabItem(tabName = "GGS",
        h2("Analyse des variables Genre * Global_Sales"),
        sidebarLayout(
          sidebarPanel(
            h3("Filtre"),
            selectInput("GenreGGS", "Choix du Genre", c("Action" = "Action", "Strategy" = "Strategy", "Misc" = "Misc")),
            verbatimTextOutput("sumGenreGlobalSales"),br(),width=4
          ),
          mainPanel(
            plotOutput("histGenreGlobalSales"),br(),
            shinyBS::bsCollapse(id = "collapseAbout",
              shinyBS::bsCollapsePanel("Test des moyennes sur les ventes globales entre les genres de jeux Action et Strategy", 
                textOutput("textTestVarGlobalGenre"),br(),
                verbatimTextOutput("testVarGlobalGenre"),
                textOutput("textGlobalGenre1"),br(),
                verbatimTextOutput("testPValueGlobalGenre"),br(),
                textOutput("textGlobalGenre2"),
                style = "info"
              )
            ),width=8
          )
        ),
      ),
      tabItem(tabName = "PLEUS",
        h2("Analyse des variables Platform * EU_Sales"),
        sidebarLayout(
          sidebarPanel(
            h3("Filtre"),
            selectInput("PlatformPLEUS","Choix de la plateforme",
              c("Nintendo" = "Nintendo","PC" = "PC", "Playstation" = "Playstation", "Xbox" = "Xbox")
            ),verbatimTextOutput("sumPlatEuSales"),br(),width=4
          ),
          mainPanel(
            plotOutput("histPlatEuSales"),br(),
            shinyBS::bsCollapse(id = "collapseAbout",
              shinyBS::bsCollapsePanel("Test des moyennes sur les ventes europennes entre les plateformes Playstation et Nintendo", 
                textOutput("textTestVarEuPlat"),br(),
                verbatimTextOutput("testVarEuPlat"),
                textOutput("textEuPlat1"),br(),
                verbatimTextOutput("testPValueEuPlat"),br(),
                textOutput("textEuPlat2"),
                style = "info"
              )
            ),width=8
          )
        )
      ),
      tabItem(tabName = "PLGS",
        h2("Analyse des variables Platform * Global_Sales"),
        sidebarLayout(
          sidebarPanel(
            h3("Filtre"),
            selectInput("PlatformPLGS","Choix de la plateforme",
              c("Nintendo" = "Nintendo","PC" = "PC", "Playstation" = "Playstation", "Xbox" = "Xbox")
            ),
            verbatimTextOutput("sumPlatGlobalSales"),br(),width=4
          ),
          mainPanel(
            plotOutput("histPlatGlobalSales"),br(),
            shinyBS::bsCollapse(id = "collapseAbout",
              shinyBS::bsCollapsePanel("Test des moyennes sur les ventes globales entre les plateformes Playstation et Nintendo", 
                textOutput("textTestVarGlobalPlat"),br(),
                verbatimTextOutput("testVarGlobalPlat"),br(),
                textOutput("textGlobalPlat1"),br(),
                verbatimTextOutput("testPValueGlobalPlat"),br(),
                textOutput("textGlobalPlat2"),
                style = "info"
              )
            ),width=8
          )
        )
      ),
      ####    Analyse Quali*Quali    ####
      tabItem(tabName = "QualiQuali",
        h2("Analyse quanti*quanti"),
        sidebarLayout(
          sidebarPanel(
            h3("Tableau Contingence"),
            tableOutput("contingenceTable"),
            width=4
          ),
          mainPanel(
            plotOutput("QualiQuali"),br(),
            htmlOutput("textQualiQuali"),br(),
            shinyBS::bsCollapse(id = "collapseAbout",
              shinyBS::bsCollapsePanel("Independance des variables", 
                p("Pour tester l'independance des variables Genre et Platform, nous avons realise un test du Chi, avec comme
                  hypothese: H0 : les variables Genre et Platform sont independantes, H1 : les variables Genre et Platform sont liees"),
                verbatimTextOutput("indenpendanceTest"),
                style = "info"
              )
            ),
            width=7
          ),
          fluid = TRUE
        )
      ),
      ####   Analyse Quanti*Quanti   ####
      tabItem(tabName = "QuantiQuanti",
        h2("Analyse quanti*quanti"),
        plotOutput("plotest2"),br(),
        htmlOutput("plotest2Text"),br(),
        shinyBS::bsCollapse(id = "CollapseQuanti",
          shinyBS::bsCollapsePanel("Coefficient de correlation", textOutput("coeffCorrelation"),br(),textOutput("coeffCorText"),
                          br(), verbatimTextOutput("coeffDetail"), style = "info"),
          shinyBS::bsCollapsePanel("Analyses des residus", plotOutput("residus"),br(),htmlOutput("residusText"), style = "info")
        )
      ),
      #####################################
      #              DATABASE             #
      #####################################
      tabItem(
        tabName = "Database",
        h1("Base de donnees"),br(),
        p("Cet ensemble de donnees a ete genere par un extrait de ",
          actionLink("", "https://www.vgchartz.com")),
        p("L'affichage a ete genere via l'utilisation du package DT"),br(),
        DT::dataTableOutput("table")
      ),
      #####################################
      #                ABOUT              #
      #####################################
      tabItem(
        tabName = "About",
        h2("About"),
        p("Cette interface a ete realise dans le cadre du projet statistiques de Master MIAGE premiere annee."),
        h4(tag("b", "Fichier de donnees : "), tag("i","vgsales.csv")),
        h4(tag("b","Source du fichier : "),br()),
        h4(tag("b","Binome : ")),
        fluidRow(
          infoBox("GOURDON Stephanie", value = NULL, subtitle = "stephanie.gourdon@etu.univ-grenoble-alpes.fr", icon = icon("user"),
                  color = "aqua", width = 5, href = "mailto:stephanie.gourdon@etu.univ-grenoble-alpes.fr"),
          infoBox("AVANZIZNO Aurelien", value = NULL, subtitle = "aurelien.avanzino@etu.univ-grenoble-alpes.fr", icon = icon("user"),
                  color = "aqua", width = 5, href = "mailto:aurelien.avanzino@etu.univ-grenoble-alpes.fr")
        ),
        shinyBS::bsCollapse(id = "collapseAbout",
          shinyBS::bsCollapsePanel("Composition du projet", 
            p("Le projet est decompose en deux parties distinctes :"),
            tag("li", "Le rapport markdown contenant une presentation globale du contexte et du eu de donnees ainsi que l'ensemble 
              des graphique, des analyses effectuees et les differentes conclusions qui en decoulent"),
            tag("li", "L'application Shiny developpee avec le langage R, dont la composition et le fonctionnement est decrit ci-dessous"),
            style = "info"
          ),
          shinyBS::bsCollapsePanel("Packages utilises", 
            p("Voici la liste des packages utilises :"),
            tag("li", "shiny : "),
            tag("li", "shinyBS : "),
            tag("li", "shinydashboard : Pour la gestion de l'interface graphique de l'application"),
            tag("li", "DT : Pour l'affichage des donnees du jeu dans l'onglet Database"), 
            style="info"
          ),
          shinyBS::bsCollapsePanel("Composition de l'application", 
            p("Voici la liste des packages utilises :"),
            tag("li", "Introduction : Il s'agit d'un equivalent du rapport markdown mais integre directement dans l'application."),
            tag("li", "Statistiques multivariees :"),
            tag("li", "Database : Donne un affichage complet de l'ensemble de la base de donnees utilisee durant l'analyse."),
            tag("li", "About : Contient toutes les donnees complementaires sur le projet"),
            style="info"
          )
        ),
      )
    )
  )
)