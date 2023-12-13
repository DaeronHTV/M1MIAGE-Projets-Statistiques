# Define server logic required to draw a histogram
server <- function(input, output) {
  vgsales <- read.csv("vgsales.csv",header = TRUE,sep = ",",stringsAsFactors = TRUE)
  vgsales$Year[which(vgsales$Year == "N/A")]<- NA
  vgsales$Year[which(vgsales$Publisher == "N/A")]<- NA
  base_complete <- na.omit(vgsales)
  base <- base_complete[,c(3,5,8,11)]
  bases <- base[which(base$Global_Sales > 0.4 & base$Platform != 'SCD' & base$Platform != 'WS' & 
                        base$Platform != 'DC' & base$Platform != 'SAT' & base$Platform != 'GEN' & 
                        base$Platform != 'PSV' & base$Platform != 'WiiU' & base$Platform != 'GB' & 
                        base$Platform != '2600' & base$Platform != 'XOne' & base$Platform != 'NES' & 
                        base$Platform != 'SNES'),]
  bases1 <- bases[2:4182,]
  nintendo <- bases1[which(bases1$Platform=="3DS" | bases1$Platform=="DS" | bases1$Platform=="GBA" | bases1$Platform=="GC" | bases1$Platform=="N64" | bases1$Platform=="Wii"),]
  pc <- bases1[which(bases1$Platform=="PC"),]
  playstation <- bases1[which(bases1$Platform=="PS" | bases1$Platform=="PS2" | bases1$Platform=="PS3" | bases1$Platform=="PS4" | bases1$Platform=="PSP"),]
  xbox <- bases1[which(bases1$Platform=="X360" | bases1$Platform=="XB"),]
  bases1$Global_Sales <- log(bases1$Global_Sales)
  bases1$EU_Sales <- log(bases1$EU_Sales)
  bases1$EU_Sales[which(bases1$EU_Sales == "-Inf")]<- NA
  bases1 <- na.omit(bases1)
  bases1$Platform.reg <- as.character(bases1$Platform)
  bases1$Platform.reg[which(bases1$Platform.reg=="3DS" | bases1$Platform.reg=="DS" | bases1$Platform.reg=="GBA" | bases1$Platform.reg=="GC" | bases1$Platform.reg=="N64" | bases1$Platform.reg=="Wii")] <- "Nintendo"
  bases1$Platform.reg[which(bases1$Platform.reg =="PS" | bases1$Platform.reg=="PS2" | bases1$Platform.reg=="PS3" | bases1$Platform.reg=="PS4" | bases1$Platform.reg=="PSP")] <- "Playstation"
  bases1$Platform.reg[which(bases1$Platform.reg =="X360" | bases1$Platform.reg=="XB")] <- "Xbox"
  bases1$Genre.reg <- as.character(bases1$Genre)
  bases1$Genre.reg[which(bases1$Genre.reg=="Action" | bases1$Genre.reg=="Adventure" | bases1$Genre.reg=="Fighting" | bases1$Genre.reg=="Racing" | bases1$Genre.reg=="Shooter" | bases1$Genre.reg=="Sports" )] <- "Action"
  bases1$Genre.reg[which(bases1$Genre.reg=="Platform" | bases1$Genre.reg=="Puzzle" | bases1$Genre.reg=="Role-Playing" | bases1$Genre.reg=="Simulation" | bases1$Genre.reg=="Strategy")] <- "Strategy"
  
  baseReduiteGenre <- bases1[which(bases1$Genre == "Action" | bases1$Genre == "Strategy"),]
  baseReduitePlatform <- bases1[which(bases1$Platform == "Nintendo" | bases1$Platform == "Playstation"),]
  
  bases2 <- bases1[,3:6]  
  genrePlat <- table(bases2$Platform.reg, bases2$Genre.reg)
  
  ##########################################
  ############Affichage Database############
  ##########################################
  output$table <- DT::renderDataTable({
    base <- na.omit(vgsales)
    bases <- base[which(base$Global_Sales > 0.4 & base$Platform != 'SCD' & base$Platform != 'WS' & base$Platform != 'DC' & base$Platform != 'SAT' & base$Platform != 'GEN' & base$Platform != 'PSV' & base$Platform != 'WiiU' & base$Platform != 'GB' & base$Platform != '2600' & base$Platform != 'XOne' & base$Platform != 'NES' & base$Platform != 'SNES'),]
    bases1 <- bases[2:4182,]
    DT::datatable(
      bases1, rownames = FALSE, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',buttons = c('colvis', 'copy', 'csv', 'pdf', 'print')
      )
    )
  })
  
  ##########################################
  ####GRAPHIQUE STATISTIQUES MULTIVARIES####
  ##########################################
  #Analayse Quanti*Quali
  #GENRE EU SALES
  output$sumGenreEuSales <- renderPrint({
    Gender <- bases1[which(bases1$Genre == input$GenreGEUS),c(2,3)]
    summary(Gender)
  })
  output$histGenreEuSales <- renderPlot({
    Gender <- bases1[which(bases1$Genre == input$GenreGEUS),c(2,3)]
    par(mfrow=c(1,2))
    hist(Gender$EU_Sales)
    boxplot(Gender$EU_Sales)
  })
  output$textTestVarEuGenre <- renderText({
    "Comme nous avons un croisement quanti*quali, pour pouvoir faire le test sur la moyenne, il faut d'abord faire le test 
    sur l'egalite des variances pour determiner l'ecriture du test. Pour cela, il faut soit que les sous-populations suivent 
    une loi gaussienne, soit que l'echantillon soit assez grand, ce qui est notre cas ici avec 3534 individus"
  })
  output$testVarEuGenre <- renderPrint({
    testmoyGenreEU <- baseReduiteGenre[,c(2,3)]
    var.test(EU_Sales~Genre,data = testmoyGenreEU)
  })
  output$testPValueEuGenre <- renderPrint({
    testmoyGenreEU <- baseReduiteGenre[,c(2,3)]
    t.test(EU_Sales~Genre,var.equal=F,data = testmoyGenreEU)
  })
  output$textEuGenre1 <- renderText({
    "La p-value est de 0.01381, donc elle est inferieure a 0.05, et on rejette H0. Donc au risque de 5%, les variances ne sont pas egales.
    On peut a present faire le test sur l'egalite des moyennes, avec les variances non egales, et l'echantillon est independant.
    Pour tester l'egalite des moyennes, on fait les hypotheses suivantes - m1 - moyenne des ventes europeennes des jeux d'action m2 - moyenne des ventes europeennes des jeux de strategie
    H0, m1 et m2 sont egales H1, m1 et m2 sont differentes"
  })
  output$textEuGenre2 <- renderText({
    "La p-value est de 0.002592, donc elle est inferieure a 0.05, et on rejette H0. Donc au risque de 5%, les moyennes des 
    ventes europeennes des jeux d'action et de strategie sont differentes."
  })
  #GENRE GLOBAL SALES
  output$sumGenreGlobalSales <- renderPrint({
    Gender <- bases1[which(bases1$Genre == input$GenreGGS),c(2,4)]
    summary(Gender)
  })
  output$histGenreGlobalSales <- renderPlot({
    Gender <- bases1[which(bases1$Genre == input$GenreGGS),c(2,4)]
    par(mfrow=c(1,2))
    hist(Gender$Global_Sales)
    boxplot(Gender$Global_Sales)
  })
  output$textTestVarGlobalGenre <- renderText({
    "Comme nous avons un croisement quanti*quali, pour pouvoir faire le test sur la moyenne, il faut d'abord faire le test 
    sur l'egalite des variances pour determiner l'ecriture du test. Pour cela, il faut soit que les sous-populations suivent 
    une loi gaussienne, soit que l'echantillon soit assez grand, ce qui est notre cas ici avec 3534 individus"
  })
  output$testVarGlobalGenre <- renderPrint({
    testmoyGenGl <- baseReduiteGenre[,c(2,4)]
    var.test(Global_Sales~Genre,data = testmoyGenGl)
  })
  output$testPValueGlobalGenre <- renderPrint({
    testmoyGenGl <- baseReduiteGenre[,c(2,4)]
    t.test(Global_Sales~Genre,var.equal=F,data = testmoyGenGl)
  })
  output$textGlobalGenre1 <- renderText({
    "La p-value est de 0.03062, donc elle est inferieure a 0.05, et on rejette H0. Donc au risque de 5%, les variances ne sont pas egales.
    On peut a present faire le test sur l'egalite des moyennes, avec les variances non egales, et l'echantillon est independant.
    Pour tester l'egalite des moyennes, on fait les hypotheses suivantes - m1 - moyenne des ventes globales des jeux d'action m2 - moyenne des ventes globales des jeux de strategie
    H0, m1 et m2 sont egales H1, m1 et m2 sont differentes"
  })
  output$textGlobalGenre2 <- renderText({
    "La p-value est de 0.08868, donc elle est legerement superieure a 0.05, et on rejette avec precaution H1. Donc au risque 
    de 5%, les moyennes des ventes globales des jeux d'action et de strategie sont egales."
  })
  #PLATEFORME EU SALES
  output$sumPlatEuSales <- renderPrint({
    Gender <- bases1[which(bases1$Platform.reg == input$PlatformPLEUS),c(1,3)]
    summary(Gender)
  })
  output$histPlatEuSales <- renderPlot({
    Gender <- bases1[which(bases1$Platform.reg == input$PlatformPLEUS),c(1,3)]
    par(mfrow=c(1,2))
    hist(Gender$EU_Sales)
    boxplot(Gender$EU_Sales)
  })
  output$textTestVarEuPlat <- renderText({
    "Comme nous avons un croisement quanti*quali, pour pouvoir faire le test sur la moyenne, il faut d'abord faire le test 
    sur l'egalite des variances pour determiner l'ecriture du test. Pour cela, il faut soit que les sous-populations suivent 
    une loi gaussienne, soit que l'echantillon soit assez grand, ce qui est notre cas ici avec 3062 individus"
  })
  output$testVarEuPlat <- renderPrint({
    testmoyGenGl <- baseReduiteGenre[,c(2,4)]
    var.test(Global_Sales~Genre,data = testmoyGenGl)
  })
  output$testPValueEuPlat <- renderPrint({
    testmoyGenGl <- baseReduiteGenre[,c(2,4)]
    t.test(Global_Sales~Genre,var.equal=F,data = testmoyGenGl)
  })
  output$textEuPlat1 <- renderText({
    "La p-value est de 3.72e-05, donc elle est inferieure a 0.05, et on rejette H0. Donc au risque de 5%, les variances ne sont pas egales.
    On peut a present faire le test sur l'egalite des moyennes, avec les variances non egales, et l'echantillon est independant.
    Pour tester l'egalite des moyennes, on fait les hypotheses suivantes - m1 - moyenne des ventes europeennes de la plateforme Nintendo m2 - moyenne des ventes europeennes de la plateforme Playstation
    H0, m1 et m2 sont egales H1, m1 et m2 sont differentes"
  })
  output$textEuPlat2 <- renderText({
    "La p-value est de 9.595e-10, donc elle est inferieure a 0.05, et on rejette H0. Donc au risque de 5%, les moyennes des 
    ventes europeennes des plateformes Nintendo et Playstation sont differentes."
  })
  #PLATEFORME GLOBAL SALES
  output$sumPlatGlobalSales <- renderPrint({
    GlPlaystation <- bases1[which(bases1$Platform.reg == input$PlatformPLGS),c(1,4)]
    summary(GlPlaystation)
  })
  output$histPlatGlobalSales <- renderPlot({
    GlPlaystation <- bases1[which(bases1$Platform.reg == input$PlatformPLGS),c(1,4)]
    par(mfrow=c(1,2))
    hist(GlPlaystation$Global_Sales)
    boxplot(GlPlaystation$Global_Sales)
  })
  output$textTestVarGlobalPlat <- renderText({
    "Comme nous avons un croisement quanti*quali, pour pouvoir faire le test sur la moyenne, il faut d'abord faire le test 
    sur l'egalite des variances pour determiner l'ecriture du test. Pour cela, il faut soit que les sous-populations suivent 
    une loi gaussienne, soit que l'echantillon soit assez grand, ce qui est notre cas ici avec 3062 individus"
  })
  output$testVarGlobalPlat <- renderPrint({
    testmoyGenGl <- baseReduiteGenre[,c(2,4)]
    var.test(Global_Sales~Genre,data = testmoyGenGl)
  })
  output$testPValueGlobalPlat <- renderPrint({
    testmoyGenGl <- baseReduiteGenre[,c(2,4)]
    t.test(Global_Sales~Genre,var.equal=F,data = testmoyGenGl)
  })
  output$textGlobalPlat1 <- renderText({
    "La p-value est de 0.0001953, donc elle est inferieure a 0.05, et on rejette H0. Donc au risque de 5%, les variances ne sont pas egales.
    On peut a present faire le test sur l'egalite des moyennes, avec les variances non egales, et l'echantillon est independant.
    Pour tester l'egalite des moyennes, on fait les hypotheses suivantes - m1 - moyenne des ventes globales de la plateforme Nintendo m2 - moyenne des ventes globales de la plateforme Playstation
    H0, m1 et m2 sont egales H1, m1 et m2 sont differentes"
  })
  output$textGlobalPlat2 <- renderText({
    "La p-value est de 0.3282, donc elle est superieure a 0.05, et on rejette H1. Donc au risque de 5%, les moyennes des ventes
    globales des plateformes Nintendo et Playstation sont egales."
  })
  
  
  
  
  
  #Analayse Quali*Quali
  output$QualiQuali <- renderPlot({
      barplot(genrePlat, main="Nombre de jeux video par plateforme en fonction des genres",legend.text = TRUE, col=rainbow(4), cex.names = 0.9, las=2, ylim = c(0,3000), ylab= "Nombre de jeux video", args.legend=list(bty="n"))
  })
  output$textQualiQuali <- renderUI({
    str1 <- paste("Sur cette representation, nous remarquons que les jeux video d'action et de sports sont les jeux 
              qui ont fait le plus de ventes dans le monde, toutes plateformes confondues, meme si l'on peut voir 
              que sur la plateforme playstation, les ventes sont doublees sur ces deux genres par rapport aux 
              autres genres.")
    HTML(paste(str1, sep='<br/>'))
  })
  output$contingenceTable <- renderTable({table(bases2$Platform.reg, bases2$Genre.reg)})
  output$indenpendanceTest <- renderPrint({chisq.test(genrePlat)})
  
  #Analyse Quanti*Quanti
  output$plotest2 <- renderPlot({
    plot(bases1$EU_Sales, bases1$Global_Sales, xlab = "Ventes europeennes (en millions d'euros)", ylab="Ventes globales (en millions d'euros)")
    legend(0.15,0, c("val. observees","droite des moindres carres"), lty=c(0,0,1), cex=.8, pch=c(1,3),col=c(1,2))
    text(1.5,-0.90,"eq. dte de reg : y=0.43x+0.65",cex=.8)
    lm(bases1$Global_Sales~bases1$EU_Sales)->lm.ge
    achap<-lm.ge$coefficients[2]
    bchap<-lm.ge$coefficients[1]
    abline(bchap,achap, col="red")
    
  })
  output$plotest2Text <- renderUI({
    str1 <- paste("Avec ce graphe, on peut voir que les points ont tendance a prendre la forme d'un cone. Lorsque l'on 
                  trace la droite des moindres carres,on remarque que le modele parait tres mauvais, il y a des valeurs
                  qui sont tres eloignees de la droite, sacahtn que nous avons deja applique la fonction du log sur les
                  variables EU_Sales et Global_Sales.")
    HTML(paste(str1, sep='<br/>'))
  })
  output$coeffCorrelation <- renderPrint({paste("Coefficient de correlation : ",cor(bases1$EU_Sales, bases1$Global_Sales))})
  output$coeffCorText <- renderText({
    "On peut voir que le coefficient de correlation lineaire r(EU_Sales,Global_Sales) est proche de 1, il est de 0.9, donc il
    semble y avoir une correlation positive entre les variables EU_Sales et Global_Sales, ce qui signifie que lorsqu'une
    variable augmente, alors l'autre augmente, et inversement."
  })
  output$coeffDetail <- renderPrint({
    lm(bases1$Global_Sales~bases1$EU_Sales)->lm.ge
    summary(lm.ge)
  })
  output$residusText = renderUI({
        str1 <- paste("Sur le graphe Residuals vs Fitted, nous voyons que les valeurs sont assez eloignees de la droite, 
              elles sont entre -1 et 4. Sur le graphe Normal Q-Q, on voit qu'une partie des points suit bien la 
              loi gaussienne, mais que pour les valeurs aux extremites, il y a quand meme beaucoup de points 
              aberrants, notamment un a 38. Sur le graphe Scale-Location, on voit qu'il y a une diminution 
              de la dispersion des residus lorsque les fitted values augmentent, donc les residus ont l'air d'etre 
              assez homogene. Comme le modele est plus que moyen, alors on ne peut pas predire les ventes globales 
              a partir des ventes europeennes, cela peut se confirmer avec l'equation de droite des moindres carres.")
        HTML(paste(str1, sep='<br/>'))
  })
  output$residus = renderPlot({
    lm(bases1$Global_Sales~bases1$EU_Sales)->lm.ge
    par(mfrow=c(2,2))
    plot(lm.ge)
  })
  
  output$genericPlot <- renderPlot(plot(rnorm(100)))
  observeEvent(input$p1Button, ({updateCollapse(session, "collapseExample", open = "Panel 1")}))
  observeEvent(input$styleSelect, ({updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))}))
}