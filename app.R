# Charger les bibliothèques nécessaires pour l'application
library(shiny)         # Framework pour créer des applications web interactives
library(shinythemes)   # Thèmes pour les applications Shiny
library(DT)            # Pour créer des tableaux interactifs
library(ggplot2)       # Pour les graphiques
library(leaflet)       # Pour générer des cartes interactives
library(shinymanager)  # Pour ajouter une authentification
library(sodium)        # Pour sécuriser les mots de passe (hashage)
library(ggrepel)       # Pour ajouter des étiquettes aux graphiques ggplot
library(dplyr)         # Pour permettre les filtres

#Chargement des données 

df = read.csv("data/DonnesNeufsAnciens.csv",header=TRUE,sep=",",dec=".")

colnames(df)

# Définir les utilisateurs et leurs mots de passe pour l'authentification
credentials <- data.frame(
  user = c("abc", "user1"),                  # Noms d'utilisateurs
  password = c("abcd1","pass1"),             # Mots de passe (non hashés ici, mais cela devrait être fait pour plus de sécurité)
  stringsAsFactors = FALSE                   # Empêcher la conversion en facteurs
)

# Interface utilisateur
ui <- secure_ui <- secure_app(                # Utilisation de shinymanager pour sécuriser l'application
  fluidPage(                                  # Début de la structure de l'interface utilisateur
    tags$head(
      tags$link(rel="stylesheet", href="styles.css")  # Lier une feuille de style CSS externe pour customiser l'apparence
    ),
    
    theme = shinytheme("united"),              # Thème de l'application
    navbarPage("Greentech Solutions",          # Menu principal de l'application avec plusieurs onglets
               
               # Premier onglet : Contexte de l'application
               tabPanel("Contexte", 
                        fluidRow(
                          column(12,
                                 div(style = "text-align: center; margin-bottom: 20px;", 
                                     h2("Contexte de l'application"),       # Titre de la page
                                     br(),
                                     verbatimTextOutput("contexte_text")    # Affichage d'un texte non modifiable qui décrit le contexte
                                 ))
                        )
               ),
               
               # Deuxième onglet : Carte interactive dans la région du Rhône
               tabPanel("Carte intéractive en Rhône", 
                        fluidRow(
                          # Colonne pour la carte Leaflet (à gauche)
                          column(8, 
                                 leafletOutput("map", height = "94.5vh")    # Carte interactive Leaflet prenant presque toute la hauteur de la fenêtre
                          ),
                          
                          # Colonne pour le filtre par code postal et les indicateurs clés (KPI)
                          column(4, 
                                 # Conteneur pour le filtre et les KPI
                                 div(class = "filter-container",
                                     # Liste déroulante pour filtrer les bâtiments par code postal
                                     div(class = "select-input-container",  
                                         tags$div(style = "text-align: center; margin-bottom: 20px;",  
                                                  selectInput("code_postal", "Filtrer par code postal :", 
                                                              choices = c("Tous les codes postaux", unique(df$`Code_postal_.BAN.`)), # Filtrage par code postal
                                                              selected = "69200", multiple = FALSE)  # Valeur par défaut
                                         )
                                     ),
                                     
                                     # Conteneur pour afficher les KPI
                                     div(class = "kpi-container",
                                         div(class = "kpi-box kpi-1",
                                             tags$h5(style = "text-align: center;", "Nombre total de bâtiments"),  # Titre du KPI
                                             h3(textOutput("kpi1_value"))    # Valeur du KPI (nombre total de bâtiments)
                                         ),
                                         div(class = "kpi-box kpi-2",
                                             tags$h5(style = "text-align: center;", "Nombre de bâtiments anciens"), # Nombre de bâtiments anciens
                                             h3(textOutput("kpi2_value")) 
                                         ),
                                         div(class = "kpi-box kpi-3",
                                             tags$h5(style = "text-align: center;", "Nombre de bâtiments neufs"),   # Nombre de bâtiments neufs
                                             h3(textOutput("kpi3_value")) 
                                         )
                                     ),
                                     
                                     # Tableau pour la répartition des étiquettes DPE et GES
                                     div(class = "table-container",
                                         tags$h4(style = "text-align: center;", "Répartition des étiquettes DPE"), # Titre tableau DPE
                                         tableOutput("kpi_dpe_table"),        # Tableau de répartition DPE
                                         tags$h4(style = "text-align: center;", "Répartition des étiquettes GES"), # Titre tableau GES
                                         tableOutput("kpi_ges_table")         # Tableau de répartition GES
                                     )
                                 )
                          )
                        )
               ),
               
               # Troisième onglet : Comparaison statistique entre bâtiments anciens et neufs
               tabPanel("Statistiques comparatifs",
                        fluidRow(
                          column(12,
                                 div(style = "text-align: center; margin-bottom: 20px;", 
                                     h2("Ancien vs Neufs"),                # Titre de la section
                                     br(),
                                     h4("Statistiques pour les habitations anciennes")) # Sous-titre pour les habitations anciennes
                          ),
                          # Première ligne de diagrammes circulaires pour les bâtiments anciens
                          column(4,
                                 plotOutput("pie_chart_1", height = "150px")  # Premier diagramme circulaire
                          ),
                          column(4,
                                 plotOutput("pie_chart_2", height = "150px")  # Deuxième diagramme
                          ),
                          column(4,
                                 plotOutput("pie_chart_3", height = "150px")  # Troisième diagramme
                          )
                        ),
                        # Deuxième ligne pour les bâtiments neufs
                        fluidRow(
                          br(),
                          column(12,
                                 div(style = "text-align: center; margin-bottom: 20px;", 
                                     h4("Statistiques pour les habitations neuves")) # Sous-titre pour les habitations neuves
                          ),
                          column(4,
                                 plotOutput("pie_chart_4", height = "150px")   # Premier diagramme circulaire pour les neufs
                          ),
                          column(4,
                                 plotOutput("pie_chart_5", height = "150px")   # Deuxième diagramme
                          ),
                          column(4,
                                 plotOutput("pie_chart_6", height = "150px")   # Troisième diagramme
                          )
                        )
               ),
               
               # Quatrième onglet : Statistiques générales avec histogramme et boxplot
               tabPanel("Statistiques Générales",
                        fluidRow(
                          column(4,
                                 wellPanel(
                                   h2("Options de visualisation"),           # Titre du panneau
                                   radioButtons("plot_var", "Sélectionnez une variable :", # Sélection d'une variable à visualiser
                                                choices = c("Coût de refroidissement (€)" = "Coût_refroidissement",
                                                            "Coût ECS (€)" = "Coût_ECS",
                                                            "Coût d'éclairage (€)" = "Coût_éclairage",
                                                            "Coût de chauffage (€)" = "Coût_chauffage",
                                                            "Coût total des 5 usages (€)" = "Coût_total_5_usages",
                                                            "Conso_chauffage_é_finale (kWhef/an)"="Conso_chauffage_é_finale",
                                                            "Conso_5_usages_é_finale (kWhef/an)"="Conso_5_usages_é_finale",
                                                            "Conso_refroidissement_é_finale (kWhef/an)"="Conso_refroidissement_é_finale",
                                                            "Conso_ECS_é_finale (kWhef/an)"="Conso_ECS_é_finale",
                                                            "Conso_éclairage_é_finale (kWhef/an)"="Conso_éclairage_é_finale"), # Choix multiples
                                                selected = "Coût_chauffage"),  # Valeur par défaut
                                   sliderInput("range_values", "Plage de valeurs à afficher :",  # Plage de valeurs
                                               min = 0, max = 50000, value = c(0, 50000)),  # Plage par défaut
                                   numericInput("bins", "Nombre de bins (pour l'histogramme) :", value = 30, min = 1)  # Nombre de classes pour l'histogramme
                                 )
                          ),
                          
                          # Colonne pour les graphiques
                          column(8,
                                 tabsetPanel(
                                   tabPanel("Statistiques", 
                                            uiOutput("statistics_ui")  # Zone de sortie pour afficher les statistiques
                                   ),
                                   tabPanel("Histogramme",
                                            plotOutput("histogram_output", height = "400px"),  # Histogramme
                                            downloadButton("download_histogram", "Télécharger l'histogramme")  # Bouton pour télécharger
                                   ),
                                   tabPanel("Boxplot",
                                            plotOutput("boxplot_output", height = "400px"),   # Boxplot
                                            downloadButton("download_boxplot", "Télécharger le boxplot")  # Téléchargement
                                   )
                                 )
                          )
                        )
               ),
               
               # Cinquième onglet : Analyses descriptives
               tabPanel("Analyses descriptives",
                        fluidRow(
                          column(4,
                                 wellPanel(
                                   h2("Analyses descriptives"),
                                   selectInput("x_varh", "Sélectionnez une variable X",    # Variable en X
                                               choices = c("Etiquette_DPE", "Etiquette_GES"), selected = "Etiquette_DPE"),
                                   selectInput("y_varh", "Sélectionnez une variable Y",    # Variable en Y
                                               choices = c("Coût_chauffage", "Coût_refroidissement", "Coût_éclairage", "Coût_total_5_usages", "Coût_ECS", "Type_hab", "Type_bâtiment","Conso_chauffage_é_finale","Conso_refroidissement_é_finale","Conso_éclairage_é_finale","Conso_5_usages_é_finale","Conso_ECS_é_finale"), selected = "Coût_chauffage")
                                 )
                          ),
                          
                          column(8,
                                 plotOutput("histogram_plot", height = "400px"),  # Graphique descriptif
                                 downloadButton("download_plot", "Télécharger le graphique")  # Bouton pour télécharger
                          )
                        )
               ),
               
               # Sixième onglet : Régression linéaire
               tabPanel("Regression linéaire",
                        fluidRow(
                          column(4,
                                 wellPanel(
                                   h2("Regression linéaire"),               # Titre du panneau
                                   
                                   selectInput("num_var_1", "Sélectionnez une variable X",    # Variable explicative
                                               choices = c("Coût_chauffage", "Coût_refroidissement", "Coût_éclairage", "Coût_total_5_usages", "Coût_ECS","Conso_chauffage_é_finale","Conso_refroidissement_é_finale","Conso_éclairage_é_finale","Conso_5_usages_é_finale","Conso_ECS_é_finale","Aucun"), selected = "Aucun"),
                                   
                                   selectInput("num_var_2", "Sélectionnez une variable Y",    # Variable dépendante
                                               choices = c("Coût_chauffage", "Coût_refroidissement", "Coût_éclairage", "Coût_total_5_usages", "Coût_ECS","Conso_chauffage_é_finale","Conso_refroidissement_é_finale","Conso_éclairage_é_finale","Conso_5_usages_é_finale","Conso_ECS_é_finale","Aucun"), selected = "Aucun"),
                                   
                                   checkboxInput("show_regression", "Créer la régression linéaire", value = FALSE)  # Option pour activer la régression
                                 )
                          ),  
                          column(8,
                                 plotOutput("plot_1", height = "400px"),   # Graphique de régression
                                 br(),
                                 textOutput("formule_reg"),                # Formule de la régression linéaire
                                 br(),
                                 downloadButton("download_plot", "Télécharger la régression"), # Bouton de téléchargement
                                 br(),
                                 conditionalPanel(
                                   condition = "input.show_regression == true",  # Afficher les détails seulement si l'option est cochée
                                   div(style = "text-align: center;",
                                       h3("Paramètres de la régression"),
                                       div(class = "centered-table", tableOutput("regression_summary")), # Paramètres de la régression
                                       h3("Coefficients et corrélation"),
                                       div(class = "centered-table", tableOutput("model_statistics"))     # Statistiques du modèle
                                   )
                                 )
                          )
                        )
               ),
               
               # Dernier onglet : Tableau de données
               tabPanel("Tableau de Données",
                        fluidRow(
                          column(12,
                                 div(class = "no-style",
                                     DTOutput("data_table")     # Afficher le tableau interactif des données
                                 )
                          )
                        ),
                        fluidRow(
                          column(8,
                                 downloadButton("download_data_selected", "Télécharger les données sélectionnées"), # Téléchargement des données filtrées
                                 downloadButton("download_data_all", "Télécharger toutes les données")  # Téléchargement des données complètes
                          ),
                          column(4,
                                 align = "right",  # Aligner les boutons à droite
                                 actionButton("prev", "Précédent"),  # Bouton pour les données précédentes
                                 actionButton("nexte", "Suivant")   # Bouton pour les données suivantes
                          )
                        )
               )
    )
  )
)



# Serveur
server <- function(input, output, session) {
  
  # Authentification
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$contexte_text <- renderText({
    paste(
      "Le changement climatique et l'augmentation des prix de l'énergie placent la sobriété énergétique au cœur des préoccupations des Français. Cela implique de réduire la consommation d'énergie sans compromettre la qualité de vie ou le confort des individus.\n\n",
      "Pour encourager cette pratique, qui pourrait atténuer les effets et les enjeux du réchauffement climatique en France, GreenTech Solutions propose une étude statistique. Cette étude comprend des analyses et des chiffres clés concernant l'un des leviers pour réduire les émissions de gaz à effet de serre : l'amélioration de la performance énergétique des bâtiments.\n\n",
      "À cet effet, un grand nombre de logements sont évalués sur le plan énergétique à l'aide du Diagnostic de Performance Énergétique (DPE). Ce diagnostic évalue la performance énergétique et climatique d'un bâtiment à l'aide d'une étiquette qui va de A à G.\n\n",
      "Cette évaluation indique la consommation d'énergie ainsi que l'impact en termes d'émissions de gaz à effet de serre (GES).\n\n",
      "Elle permet ainsi d'identifier les 'passoires énergétiques', dont la mise en location sera progressivement interdite. De plus, le DPE informe les locataires sur la valeur verte de leurs logements et leur suggère d'éventuels travaux d'amélioration climatique, tout en leur permettant de réduire leurs charges énergétiques.\n\n",
      "L'objectif principal de cette étude est donc de quantifier, sur la période de 2021 à 2024, l'impact d'un changement de classe DPE en termes de consommation et de charges énergétiques.\n\n",
      "Ce travail vise à inciter la population à entreprendre des rénovations, en mettant en évidence les gains potentiels sur les factures énergétiques et l'impact en termes de GES sur l'environnement."
    )
  })
  
  
  donnees_filtree <- reactive({
    if (input$code_postal == "Tous les codes postaux") {
      df  # Retourner tous les points si "Tous les codes postaux" est sélectionné
    } else {
      df %>% filter(`Code_postal_.BAN.` == input$code_postal)  # Filtrer par code postal
    }
  })
  
  # Génération de la carte
  output$map <- renderLeaflet({
    leaflet(donnees_filtree()) %>%  
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,  
        color = ~Couleur_echelle,                  
        radius = 8,                                
        fillOpacity = 0.8,                         
        stroke = FALSE,                            
        popup = ~paste0("<br><b>Numéro DPE :</b> ", `N.DPE`,
                        "<br><b>Etiquette DPE :</b> ", Etiquette_DPE,
                        "<br><b>Etiquette GES :</b> ", Etiquette_GES,
                        "<br><b>Age du bâtiment :</b> ", Type_hab,
                        "<br><b>Type de bâtiment :</b> ", Type_bâtiment,
                        "<br><b>Adresse :</b> ", `Adresse_.BAN.`,
                        "<br><b>Longitude :</b> ", Longitude,
                        "<br><b>Latitude :</b> ", Latitude),
        clusterOptions = markerClusterOptions() 
      )
  })
  
  # KPI réactifs
  output$kpi1_value <- renderText({
    nrow(donnees_filtree())
  })
  
  output$kpi2_value <- renderText({
    nrow(donnees_filtree()[donnees_filtree()$Type_hab == "ancien", ])
  })
  
  output$kpi3_value <- renderText({
    nrow(donnees_filtree()[donnees_filtree()$Type_hab == "neuf", ])
  })
  
  # Rendu du tableau pour les KPI DPE
  output$kpi_dpe_table <- renderTable({
    dpe_table <- as.data.frame(table(donnees_filtree()$Etiquette_DPE))
    colnames(dpe_table) <- c("Étiquette DPE", "Nombre") 
    
    dpe_table_transposed <- t(dpe_table)
    colnames(dpe_table_transposed) <- dpe_table$`Étiquette DPE`
    
    dpe_table_transposed[-1, , drop = FALSE]
  })
  
  # Rendu du tableau pour les KPI GES
  output$kpi_ges_table <- renderTable({
    ges_table <- as.data.frame(table(donnees_filtree()$Etiquette_GES))
    colnames(ges_table) <- c("Étiquette GES", "Nombre") 
    
    ges_table_transposed <- t(ges_table)
    colnames(ges_table_transposed) <- ges_table$`Étiquette GES`
    
    ges_table_transposed[-1, , drop = FALSE]
  })
  
  
  # Diagrammes pour les bâtiments anciens
  output$pie_chart_1 <- renderPlot({
    pie_data <- df %>% filter(Type_hab == "ancien") %>% count(Type_bâtiment)
    ggplot(pie_data, aes(x = "", y = n, fill = Type_bâtiment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Proportion par type de bâtiment (Anciens)") +
      theme_void() +
      theme(legend.position = "right", 
            text = element_text(size = 7.5)) 
  })
  
  # Diagrammes pour les bâtiments anciens
  output$pie_chart_2 <- renderPlot({
    pie_data <- df %>% filter(Type_hab == "ancien") %>% count(Etiquette_DPE)
    ggplot(pie_data, aes(x = "", y = n, fill = Etiquette_DPE)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Proportion d'étiquette DPE (Anciens)") +
      scale_fill_manual(values = c("A" = "darkgreen", 
                                   "B" = "green", 
                                   "C" = "green", 
                                   "D" = "yellow", 
                                   "E" = "orange", 
                                   "F" = "darkorange", 
                                   "G" = "red")) +
      theme_void() +
      theme(legend.position = "right", 
            text = element_text(size = 7.5))  # Ajustement de la taille des labels
  })
  
  output$pie_chart_3 <- renderPlot({
    pie_data <- df %>% filter(Type_hab == "ancien") %>% count(Etiquette_GES)
    ggplot(pie_data, aes(x = "", y = n, fill = Etiquette_GES)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Proportion d'étiquette GES (Anciens)") +
      scale_fill_manual(values = c("A" = "darkgreen", 
                                   "B" = "green", 
                                   "C" = "green", 
                                   "D" = "yellow", 
                                   "E" = "orange", 
                                   "F" = "darkorange", 
                                   "G" = "red")) +
      theme_void() +
      theme(legend.position = "right", 
            text = element_text(size = 7.5))  # Ajustement de la taille des labels
  })
  
  # Diagrammes pour les bâtiments neufs
  output$pie_chart_4 <- renderPlot({
    pie_data <- df %>% filter(Type_hab == "neuf") %>% count(Type_bâtiment)
    ggplot(pie_data, aes(x = "", y = n, fill = Type_bâtiment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Proportion par type de bâtiment (Neufs)") +
      theme_void() +
      theme(legend.position = "right", 
            text = element_text(size = 7.5)) 
    
  })
  
  # Diagrammes pour les bâtiments neufs
  output$pie_chart_5 <- renderPlot({
    pie_data <- df %>% filter(Type_hab == "neuf") %>% count(Etiquette_DPE)
    ggplot(pie_data, aes(x = "", y = n, fill = Etiquette_DPE)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Proportion d'étiquette DPE (Neufs)") +
      scale_fill_manual(values = c("A" = "darkgreen", 
                                   "B" = "green", 
                                   "C" = "green", 
                                   "D" = "yellow", 
                                   "E" = "orange", 
                                   "F" = "darkorange", 
                                   "G" = "red")) +
      theme_void() +
      theme(legend.position = "right", 
            text = element_text(size = 7.5))  # Ajustement de la taille des labels
  })
  
  output$pie_chart_6 <- renderPlot({
    pie_data <- df %>% filter(Type_hab == "neuf") %>% count(Etiquette_GES)
    ggplot(pie_data, aes(x = "", y = n, fill = Etiquette_GES)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Proportion d'étiquette GES (Neufs)") +
      scale_fill_manual(values = c("A" = "darkgreen", 
                                   "B" = "green", 
                                   "C" = "green", 
                                   "D" = "yellow", 
                                   "E" = "orange", 
                                   "F" = "darkorange", 
                                   "G" = "red")) +
      theme_void() +
      theme(legend.position = "right", 
            text = element_text(size = 7.5))  # Ajustement de la taille des labels
  })
  
  
  # Filtrer les données en fonction de la variable sélectionnée et de la plage de valeurs
  donnees_filtree_st <- reactive({
    df %>%
      filter(.data[[input$plot_var]] >= input$range_values[1],
             .data[[input$plot_var]] <= input$range_values[2])
  })
  
  # Dictionnaire d'unités
  units <- list(
    "Coût_refroidissement" = "€",
    "Coût_ECS" = "€",
    "Coût_éclairage" = "€",
    "Coût_chauffage" = "€",
    "Coût_total_5_usages" = "€",
    "Conso_chauffage_é_finale" = "kWhef/an",
    "Conso_5_usages_é_finale" = "kWhef/an",
    "Conso_refroidissement_é_finale" = "kWhef/an",
    "Conso_ECS_é_finale" = "kWhef/an",
    "Conso_éclairage_é_finale" = "kWhef/an"
  )
  
  # Histogramme réactif
  output$histogram_output <- renderPlot({
    req(input$plot_var)
    
    ggplot(donnees_filtree_st(), aes(x = .data[[input$plot_var]])) +
      geom_histogram(bins = input$bins, fill = "lightblue", color = "darkblue") +
      labs(title = paste("Histogramme de la variable", input$plot_var),
           x = paste(input$plot_var, "(", units[[input$plot_var]], ")", sep = ""),
           y = "Fréquence") +
      theme_minimal()
  })
  
  # Télécharger l'histogramme en PNG
  output$download_histogram <- downloadHandler(
    filename = function() {
      paste("histogram_", input$plot_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 800)
      print(
        ggplot(donnees_filtree_st(), aes(x = .data[[input$plot_var]])) +
          geom_histogram(bins = input$bins, fill = "lightblue", color = "darkblue") +
          labs(title = paste("Histogramme de la variable", input$plot_var),
               x = paste(input$plot_var, "(", units[[input$plot_var]], ")", sep = ""),
               y = "Fréquence") +
          theme_minimal()
      )
      dev.off()
    }
  )
  
  
 # Boxplot réactif
output$boxplot_output <- renderPlot({
  req(input$plot_var)
  
  # Ajoutez les unités ici. Remplacez "unit" par l'unité réelle que vous utilisez.
  unit <- "unit"  # Remplacez "unit" par l'unité appropriée pour votre variable
  
  ggplot(donnees_filtree_st(), aes(x = "", y = .data[[input$plot_var]])) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = paste("Boxplot de la variable", input$plot_var),
         y = paste(input$plot_var, "(", unit, ")", sep = "")) +  # Ajout des unités à l'axe Y
    theme_minimal() +
    theme(axis.text.x = element_blank())
})

# Boxplot réactif
output$boxplot_output <- renderPlot({
  req(input$plot_var)
  
  # Récupérer l'unité correspondante à la variable sélectionnée
  unit <- units[[input$plot_var]]  # Récupère l'unité depuis le dictionnaire
  
  ggplot(donnees_filtree_st(), aes(x = "", y = .data[[input$plot_var]])) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = paste("Boxplot de la variable", input$plot_var),
         y = paste(input$plot_var, "(", unit, ")", sep = "")) +  # Ajout des unités à l'axe Y
    theme_minimal() +
    theme(axis.text.x = element_blank())
})

# Télécharger le boxplot en PNG
output$download_boxplot <- downloadHandler(
  filename = function() {
    paste("boxplot_", input$plot_var, "_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 1600, height = 800)
    
    # Récupérer l'unité correspondante à la variable sélectionnée pour le téléchargement
    unit <- units[[input$plot_var]]  # Récupère l'unité depuis le dictionnaire
    
    print(
      ggplot(donnees_filtree_st(), aes(x = "", y = .data[[input$plot_var]])) +
        geom_boxplot(fill = "lightblue", color = "darkblue") +
        labs(title = paste("Boxplot de la variable", input$plot_var),
             y = paste(input$plot_var, "(", unit, ")", sep = "")) +  # Ajout des unités à l'axe Y
        theme_minimal() +
        theme(axis.text.x = element_blank())
    )
    dev.off()
  }
)
  
  
  # Statistiques réactives
  output$statistics_ui <- renderUI({
    req(input$plot_var)
    
    data <- donnees_filtree_st()[[input$plot_var]]
    
    # Calcul des statistiques
    mean_value <- sprintf("Moyenne : %.2f %s", mean(data, na.rm = TRUE), units[[input$plot_var]])
    median_value <- sprintf("Médiane : %.2f %s", median(data, na.rm = TRUE), units[[input$plot_var]])
    variance_value <- sprintf("Variance : %.2f", var(data, na.rm = TRUE))  # Variance n'a pas d'unité
    std_dev_value <- sprintf("Écart-type : %.2f %s", sd(data, na.rm = TRUE), units[[input$plot_var]])
    q1_value <- sprintf("1er quartile : %.2f %s", quantile(data, 0.25, na.rm = TRUE), units[[input$plot_var]])
    q3_value <- sprintf("3e quartile : %.2f %s", quantile(data, 0.75, na.rm = TRUE), units[[input$plot_var]])
    
    # Créer chaque boîte individuellement
    tags$div(class = "statistics-container",
             tags$div(class = "statistic-box mean", mean_value),
             tags$div(class = "statistic-box median", median_value),
             tags$div(class = "statistic-box variance", variance_value),
             tags$div(class = "statistic-box std-dev", std_dev_value),
             tags$div(class = "statistic-box q1", q1_value),
             tags$div(class = "statistic-box q3", q3_value)
    )
  })
  
  
  
  
  # Fonction réactive pour générer le graphique
  plot_data_h <- reactive({
    req(input$x_varh, input$y_varh)  # Assurez-vous que les variables sont sélectionnées
    p <- ggplot(df) +
      aes_string(x = input$x_varh, fill = input$y_varh)  # Utiliser df pour les données
    
    # Vérifie si la variable y est catégorique ou numérique
    if (input$y_varh %in% c("Type_hab", "Type_bâtiment")) {
      p + geom_bar(position = "dodge", na.rm = TRUE) +  
        labs(title = paste("Histogramme de la répartition de la variable", input$y_varh, "selon l'", input$x_varh), 
             x = paste(input$x_varh, "(", units[[input$x_varh]], ")", sep = ""),  # Ajout d'unité à l'axe x
             y = input$y_varh) +  # Pas d'unité pour les types
        theme_minimal()
    } else {
      p + geom_bar(aes(y = !!sym(input$y_varh)), stat = "identity", fill = "lightblue", na.rm = TRUE) +  
        labs(title = paste("Histogramme de la répartition de la variable", input$y_varh, "selon l'", input$x_varh), 
             x = paste(input$x_varh, "(", units[[input$x_varh]], ")", sep = ""),  # Ajout d'unité à l'axe x
             y = paste(input$y_varh, "(", units[[input$y_varh]], ")", sep = "")) +  # Ajout d'unité à l'axe y
        theme_minimal()
    }
  })
  
  # Génération du graphique
  output$histogram_plot <- renderPlot({
    plot_data_h()  # Appel à la fonction réactive
  })
  
  # Télécharger le graphique en PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Spécifier la largeur et la hauteur souhaitées en pixels
      png(file, width = 1600, height = 800)  # Par exemple, 1600x800 pixels
      print(plot_data_h())  # Utiliser plot_data_h() pour obtenir le graphique à télécharger
      dev.off()
    }
  )
  
  
  
  
  # Dictionnaire d'unités
  units <- list(
    "Coût_refroidissement" = "€",
    "Coût_ECS" = "€",
    "Coût_éclairage" = "€",
    "Coût_chauffage" = "€",
    "Coût_total_5_usages" = "€",
    "Conso_chauffage_é_finale" = "kWhef/an",
    "Conso_5_usages_é_finale" = "kWhef/an",
    "Conso_refroidissement_é_finale" = "kWhef/an",
    "Conso_ECS_é_finale" = "kWhef/an",
    "Conso_éclairage_é_finale" = "kWhef/an"
  )
  
  # Fonction réactive pour les données du graphique
  plot_data <- reactive({
    req(input$num_var_1 != "Aucun", input$num_var_2 != "Aucun")
    plot_data <- df  # Assurez-vous que 'df' est bien défini dans votre code
    
    num_var_1 <- input$num_var_1
    num_var_2 <- input$num_var_2
    
    p <- ggplot(plot_data, aes_string(x = num_var_1, y = num_var_2)) + 
      geom_point() +
      ggtitle(paste("Régression linéaire de la variable", num_var_2, "selon la variable", num_var_1)) +
      labs(x = paste(num_var_1, "(", units[[num_var_1]], ")", sep = ""),  # Ajout d'unité sur l'axe x
           y = paste(num_var_2, "(", units[[num_var_2]], ")", sep = ""))  # Ajout d'unité sur l'axe y
    
    if (input$show_regression) {
      lm_model <- lm(as.formula(paste(num_var_2, "~", num_var_1)), data = plot_data)
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "blue")
      
      coefficients <- summary(lm_model)$coefficients
      r2 <- summary(lm_model)$r.squared
      r_adj <- summary(lm_model)$adj.r.squared
      
      correlation <- cor(plot_data[[num_var_1]], plot_data[[num_var_2]], use = "complete.obs")
      
      output$regression_summary <- renderTable({
        regression_summary <- data.frame(
          Parameter = rownames(coefficients),
          Estimate = coefficients[, "Estimate"],
          `Std. Error` = coefficients[, "Std. Error"],
          `t value` = coefficients[, "t value"],
          `Pr(>|t|)` = coefficients[, "Pr(>|t|)"]
        )
        regression_summary
      })
      
      output$model_statistics <- renderTable({
        model_stats <- data.frame(
          Statistic = c("R^2", "Adjusted R^2", "Correlation"),
          Value = c(r2, r_adj, correlation)
        )
        model_stats
      })
      
      # Met à jour le texte de la formule de régression
      output$formule_reg <- renderText({
        a <- coefficients[1, "Estimate"]
        b <- coefficients[2, "Estimate"]
        paste("Formule: ", a, "+", b, "*", num_var_1)
      })
    } else {
      output$formule_reg <- renderText("")
    }
    
    p  # Retourne l'objet ggplot
  })
  
  # Afficher le graphique dans l'application
  output$plot_1 <- renderPlot({
    plot_data()
  })
  
  # Télécharger le graphique en PNG
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1600, height = 800)
      print(plot_data())
      dev.off()
    }
  )
  
  
  # Nombre de colonnes à afficher
  cols_per_page <- 6
  
  # Indice de la première colonne à afficher
  current_col_index <- reactiveVal(1)
  
  # Nombre total de colonnes
  total_cols <- ncol(df)
  
  # Observer l'événement du bouton Précédent
  observeEvent(input$prev, {
    new_index <- current_col_index() - cols_per_page
    current_col_index(max(new_index, 1))  # Ne pas aller en dessous de 1
  })
  
  # Observer l'événement du bouton Suivant
  observeEvent(input$nexte, {
    new_index <- current_col_index() + cols_per_page
    current_col_index(min(new_index, total_cols - cols_per_page + 1))  # Ne pas dépasser le nombre de colonnes
  })
  
  output$data_table <- renderDT({
    # Sélection des colonnes à afficher
    start_col <- current_col_index()
    end_col <- min(start_col + cols_per_page - 1, total_cols)
    
    # Sous-ensemble des données à afficher
    subset_df <- df[, start_col:end_col, drop = FALSE]
    
    datatable(
      subset_df,
      options = list(pageLength = 100, autoWidth = TRUE), 
      filter = 'top',
      selection = 'multiple'
    )
  })
  
  # Télécharger les données sélectionnées en CSV avec encodage UTF-8
  output$download_data_selected <- downloadHandler(
    filename = function() {
      "data_selected.csv"
    },
    content = function(file) {
      selected_rows <- input$data_table_rows_selected  # Obtenir les lignes sélectionnées
      if (length(selected_rows) > 0) {
        # Filtrer les données pour ne garder que les lignes sélectionnées
        selected_data <- df[selected_rows, ]
        write.csv(selected_data, file, row.names = FALSE, fileEncoding = "UTF-8") 
      } else {
        # Si aucune ligne n'est sélectionnée, avertir l'utilisateur ou créer un fichier vide
        write.csv(data.frame(), file, row.names = FALSE, fileEncoding = "UTF-8")  # Fichier vide
      }
    }
  )
  
  # Télécharger toutes les données en CSV avec encodage UTF-8
  output$download_data_all <- downloadHandler(
    filename = function() {
      "data_all.csv"
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")  # Écrit toutes les données
    }
  )
  
  
  
  
}

# Lancer l'application Shiny
shinyApp(ui = secure_ui, server = server)

