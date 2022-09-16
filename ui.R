

library(shiny)


ui <- fluidPage(
    
    tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"),
              tags$title("IIEG: Encuesta de desempeño gubernamental a empresas 2020
                         ")),
    
    fluidRow(
        column(width = 12,
               tags$h2(id = "title", "Encuesta de desempeño gubernamental a empresas 2020")
            )
        ),
    #
    fluidRow(id="fijo",
        fluidRow(id="options",
            column(width = 5,
                   selectInput("munic", tags$h4(class="subtitle", "Seleccione un municipio:"), 
                             choices = c("Arandas",
                                         "Ocotlán",
                                         "Puerto Vallarta",
                                         "Tala",
                                         "Tepatitlán",
                                         "Zapotlán el Grande",
                                         "ZMG"),
                             selected = "Arandas"),
                   
                   fluidRow(id="zn",
                            tags$p(id="zona", "*La ZMG se refiere a los municipios de Guadalajara, Tlajomulco de Zúñiga, San Pedro Tlaquepaque, Tonalá y Zapopan")
                   ),
                   
                   
                   selectInput("P", tags$h4(class="subtitle", "Seleccione un indicador:"), width = "100%",
                               choices = c("Gravedad en corrupción en el municipio",
                                           "Gravedad en corrupción en Jalisco",
                                           "Leyes, normas, trámites, solicitudes e inspecciones como obstáculo",
                                           "Satisfacción con servicio de agua potable",
                                           "Satisfacción con servicio de drenaje y alcantarillado",
                                           "Satisfacción con servicio de alumbrado público en su municipio",
                                           "Satisfacción con servicio de recolección de basura en su municipio",
                                           "Satisfacción con servicio de calles y avenidas",
                                           "Satisfacción con servicio de carreteras y caminos sin cuota en Jalisco",
                                           "Oferta de remuneración para agilizar o evitar trámites, inspecciones o pagos",
                                           "Solicitud de algún beneficio por parte de un servidor público",
                                           "Solicitud de regalos o dinero para un servidor público por parte de un tercero (coyote)",
                                           "Percepción de inseguridad en el municipio",
                                           "Percepción de inseguridad en bancos",
                                           "Percepción de inseguridad en centrales de abasto",
                                           "Percepción de inseguridad en centros comerciales",
                                           "Percepción de inseguridad en corredores industriales",
                                           "Percepción de inseguridad en carreteras o autopistas",
                                           "Victimización de delitos"),
                               selected = "Gravedad en corrupción en el municipio"),
            
            fluidRow(id="question",
                     column(width = 12,
                            tags$h4(class="subtitle", "Pregunta en encuesta:"),
                            textOutput("pregunta"))
                )
            ),
        
            
            column(width = 7, align = "center",
                   tags$h4(id="tdona", class="subtitle", "Proporción de empresas por respuesta"),
                   plotOutput("dona")
            )
        )
    ), 
    
    fluidRow(id="estimations",
            column(width = 5, align = "center",
                    tags$h4(id="test", class="subtitle", "Estimaciones"),
                    tableOutput("estimaciones"),
                    tags$p(id="nsnr", "*NS/NR = No sabe/No responde")),
             column(width = 7, align = "center",
                    tags$h4(id="tbarras", class="subtitle", "Comparación entre municipios"),
                    plotOutput("barras")
                    )
    )
    
)
