library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(scales)


function(input, output) {

  empresas <- read_rds("data/empresas.rds") %>% tbl_df()

  datos <- read_rds("data/datos.rds") %>% tbl_df()

  corrupcion <- read_rds("data/corrupcion.rds") %>% tbl_df()

  gobierno <- read_rds("data/gobierno.rds") %>% tbl_df()

  seguridad <- read_rds("data/seguridad.rds") %>% tbl_df()

  ZMG <- c("GUADALAJARA",
           "ZAPOPAN",
           "SAN PEDRO TLAQUEPAQUE",
           "TLAJOMULCO DE ZUÑIGA",
           "TONALÁ")

  empresas <- empresas %>% mutate(municipio = ifelse(municipio %in% ZMG, "ZMG", municipio))

  
  mun <- reactive({
    
     case_when(input$munic == "Arandas" ~ "ARANDAS",
               input$munic == "Ocotlán" ~ "OCOTLÁN",
               input$munic == "Puerto Vallarta" ~ "PUERTO VALLARTA",
               input$munic == "Tala" ~ "TALA",
               input$munic == "Tepatitlán" ~ "TEPATITLÁN DE MORELOS",
               input$munic == "Zapotlán el Grande" ~ "ZAPOTLÁN EL GRANDE",
               input$munic == "ZMG" ~ "ZMG")
  })
  
  
 
  Q <- reactive({
    
    case_when(input$P == "Gravedad en corrupción en el municipio" ~ "S2.2.4.1",
              input$P == "Gravedad en corrupción en Jalisco" ~ "S2.2.4.2",
              input$P == "Leyes, normas, trámites, solicitudes e inspecciones como obstáculo" ~ "S3.A.3.1",
              input$P == "Satisfacción con servicio de agua potable" ~ "S3.B.3.4.1",
              input$P == "Satisfacción con servicio de drenaje y alcantarillado" ~ "S3.B.3.4.2",
              input$P == "Satisfacción con servicio de alumbrado público en su municipio" ~ "S3.B.3.4.3",
              input$P == "Satisfacción con servicio de recolección de basura en su municipio" ~ "S3.B.3.4.4",
              input$P == "Satisfacción con servicio de calles y avenidas" ~ "S3.B.3.4.5",
              input$P == "Satisfacción con servicio de carreteras y caminos sin cuota en Jalisco" ~ "S3.B.3.4.6",
              input$P == "Oferta de remuneración para agilizar o evitar trámites, inspecciones o pagos" ~ "S3.E.3.20.1",
              input$P == "Solicitud de algún beneficio por parte de un servidor público" ~ "S3.E.3.20.2",
              input$P == "Solicitud de regalos o dinero para un servidor público por parte de un tercero (coyote)" ~ "S3.E.3.20.3",
              input$P == "Percepción de inseguridad en el municipio" ~ "S4.A.4.1",
              input$P == "Percepción de inseguridad en bancos" ~ "S4.A.4.2.1",
              input$P == "Percepción de inseguridad en centrales de abasto" ~ "S4.A.4.2.2",
              input$P == "Percepción de inseguridad en centros comerciales" ~ "S4.A.4.2.3",
              input$P == "Percepción de inseguridad en corredores industriales" ~ "S4.A.4.2.4",
              input$P == "Percepción de inseguridad en carreteras o autopistas" ~ "S4.A.4.2.5",
              input$P == "Victimización de delitos" ~ "S4.C.4.9..cualquier.delito.")
  })
  
  Qstn <- reactive({
    
    case_when(input$P == "Gravedad en corrupción en el municipio" ~ "¿Qué tan grave califica el problema de corrupción en su municipio?",
              input$P == "Gravedad en corrupción en Jalisco" ~ "¿Qué tan grave califica el problema de corrupción en Jalisco?",
              input$P == "Leyes, normas, trámites, solicitudes e inspecciones como obstáculo" ~ "Durante el último año, ¿considera que las leyes, normas, trámites, solicitudes e inspecciones para los establecimientos dedicados a su actividad, han sido un obstáculo para el éxito de su negocio?",
              input$P == "Satisfacción con servicio de agua potable" ~ "Durante 2019, de acuerdo con la experiencia de este establecimiento, ¿qué tan satisfecho está con el servicio de agua potable?",
              input$P == "Satisfacción con servicio de drenaje y alcantarillado" ~ "Durante 2019, de acuerdo con la experiencia de este establecimiento, ¿qué tan satisfecho está con el servicio de drenaje y alcantarillado?",
              input$P == "Satisfacción con servicio de alumbrado público en su municipio" ~ "Durante 2019, de acuerdo con la experiencia de este establecimiento, ¿qué tan satisfecho está con el servicio de alumbrado público en su municipio?",
              input$P == "Satisfacción con servicio de recolección de basura en su municipio" ~ "Durante 2019, de acuerdo con la experiencia de este establecimiento, ¿qué tan satisfecho está con el servicio de recolección de basura en su municipio?",
              input$P == "Satisfacción con servicio de calles y avenidas" ~ "Durante 2019, de acuerdo con la experiencia de este establecimiento, ¿qué tan satisfecho está con las calles y avenidas?",
              input$P == "Satisfacción con servicio de carreteras y caminos sin cuota en Jalisco" ~ "Durante 2019, de acuerdo con la experiencia de este establecimiento, ¿qué tan satisfecho está con las carreteras y caminos sin cuota de Jalisco?",
              input$P == "Oferta de remuneración para agilizar o evitar trámites, inspecciones o pagos" ~ "Durante 2019, ¿usted o algún miembro de la empresa ofreció algún tipo de remuneración con el fin de agilizar o evadir algún trámite, inspección o pago?",
              input$P == "Solicitud de algún beneficio por parte de un servidor público" ~ "Durante 2019, ¿algún servidor público intentó apropiarse o le solicitó de forma directa o indirecta algún beneficio (dinero, regalos o favores) que usted o este establecimiento pudieran otorgarle?",
              input$P == "Solicitud de regalos o dinero para un servidor público por parte de un tercero (coyote)" ~ "Durante 2019, ¿alguna tercera persona o coyote le insinuó o pidió de forma directa o indirecta dinero, regalos o favores para algún servidor público?",
              input$P == "Percepción de inseguridad en el municipio" ~ "En términos de delincuencia, ¿considera que operar este establecimiento en su municipio es...?",
              input$P == "Percepción de inseguridad en bancos" ~ "En términos de delincuencia, ¿considera que, para este establecimiento, hacer transacciones en bancos es...?",
              input$P == "Percepción de inseguridad en centrales de abasto" ~ "En términos de delincuencia, ¿considera que, para este establecimiento, hacer compra-venta en centrales de abasto es...?",
              input$P == "Percepción de inseguridad en centros comerciales" ~ "En términos de delincuencia, ¿considera que, para este establecimiento, hacer compra-venta en centros comerciales es...?",
              input$P == "Percepción de inseguridad en corredores industriales" ~ "En términos de delincuencia, ¿considera que, para este establecimiento, hacer movilización de productos en corredores industriales es...?",
              input$P == "Percepción de inseguridad en carreteras o autopistas" ~ "En términos de delincuencia, ¿considera que, para este establecimiento, transportar sus productos en carreteras o autopistas es...?",
              input$P == "Victimización de delitos" ~ "Durante 2019, ¿el establecimiento sufrió directamente algún delito?")
  })


  
  data <- reactive({

    nsnr <- case_when(Q() %in% c("S2.2.4.2","S2.2.4.1") ~ 99,
                      Q() %in% c("S3.B.3.4.1", "S3.B.3.4.2",
                                 "S3.B.3.4.3", "S3.B.3.4.4", "S3.B.3.4.5", "S3.B.3.4.6") ~ 5,
                      Q() %in% c("S3.A.3.1", "S3.E.3.20.1",
                                 "S3.E.3.20.2", "S3.E.3.20.3",
                                 "S4.A.4.1", "S4.A.4.2.1",
                                 "S4.A.4.2.2", "S4.A.4.2.3",
                                 "S4.A.4.2.4", "S4.A.4.2.5",
                                 "S4.C.4.9..cualquier.delito.") ~ 9,
                      TRUE ~ 999)
    nopc <- case_when(Q() %in% c("S2.2.4.2","S2.2.4.1", "S3.B.3.4.1", "S3.B.3.4.2",
                                 "S3.B.3.4.3", "S3.B.3.4.4", "S3.B.3.4.5", "S3.B.3.4.6") ~ 5,
                      Q() %in% c("S3.A.3.1", "S3.E.3.20.1",
                                 "S3.E.3.20.2", "S3.E.3.20.3") ~ 3,
                      Q() %in% c("S4.A.4.1", "S4.A.4.2.1",
                                 "S4.A.4.2.2", "S4.A.4.2.3",
                                 "S4.A.4.2.4", "S4.A.4.2.5") ~ 4,
                      Q() == "S4.C.4.9..cualquier.delito." ~ 2,
                      TRUE ~ 999)

    if(Q() %in% c("S2.2.4.1", "S2.2.4.2")){
      respuestas <- c("Muy grave", "Algo grave", "Poco grave", "Nada grave", "NS/NR")
    } else if(Q() %in% c("S3.A.3.1", "S3.E.3.20.1",
                       "S3.E.3.20.2", "S3.E.3.20.3")){
      respuestas <- c("Sí", "No", "NS/NR")
    } else if(Q() %in% c("S3.B.3.4.1", "S3.B.3.4.2",
                       "S3.B.3.4.3", "S3.B.3.4.4", "S3.B.3.4.5", "S3.B.3.4.6")) {
      respuestas <- c("Mucho", "Algo", "Poco", "Nada", "NS/NR")
    } else if(Q() %in% c("S4.A.4.1", "S4.A.4.2.1",
                       "S4.A.4.2.2", "S4.A.4.2.3",
                       "S4.A.4.2.4", "S4.A.4.2.5")){
      respuestas <- c("Seguro", "Inseguro", "No aplica", "NS/NR")
    } else if(Q() == "S4.C.4.9..cualquier.delito."){
      respuestas <- c("Sí", "No")
    } else {
      respuestas <- "Error"
    }
    
  

    bd <- inner_join(empresas, datos, by = "id") %>%
      inner_join(corrupcion, by = "id") %>%
      inner_join(gobierno, by = "id") %>%
      inner_join(seguridad, by = "id") %>%
      select(municipio, zmg, estrato, fac_exp, matches(paste0("^",Q(),"$"))) %>%
      setNames(c("municipio", "zmg", "estrato", "fac_exp", "respuesta")) %>%
      filter(municipio == mun()) %>%
      mutate(respuesta = ifelse(is.na(respuesta), nsnr, respuesta))


    param <- bd %>%
      group_by(fac_exp, respuesta, .drop = FALSE) %>%
      summarise(empresas = n()) %>%
      mutate(ph = empresas/sum(empresas), nh = sum(empresas), Nh = fac_exp*nh, N = sum(bd$fac_exp),
             SE2 = ifelse(nh>1, (1-nh/Nh)*(Nh/N)^2*ph*(1-ph)/(nh-1), (1-nh/Nh)*(Nh/N)^2*ph*(1-ph)/(nh)))

    p <- se <- c()
    resultado <- intconf <- list()
    final <- matrix(nrow = nopc, ncol = 3)

    for(i in 1:nopc){
      if(Q() != "S4.C.4.9..cualquier.delito.") {
        if(i!=nopc){
          resultado[[i]] <- param %>%
            filter(respuesta == i)
        } else {
          resultado[[i]] <- param %>%
            filter(respuesta == nsnr)
        }
      } else {
        resultado[[i]] <- param %>%
          filter(respuesta == i)
      }

      p[i] <- sum(resultado[[i]]$ph*resultado[[i]]$Nh/resultado[[i]]$N) # Estimación puntual
      se[i] <- sqrt(sum(resultado[[i]]$SE2)) # Error estándar
      intconf[[i]] <- p[i] + c(-1,1)*qnorm(.975)*se[i] # Intervalo de confianza
      final[i,] <- c(p[i], intconf[[i]])
    }

    final[final<0] <- 0
    colnames(final) <- c("Porcentaje promedio", "Límite inferior", "Límite superior")
    rownames(final) <- respuestas
    final


  
  })

  output$estimaciones <- renderTable({
    props <- matrix(paste0(round(unlist(data())*100,2), "%"), nrow = nrow(data()))
    colnames(props) <- colnames(data())
    rownames(props) <- rownames(data())
    cbind(Respuesta = rownames(data()),props)
  })

  output$dona <- renderPlot({

    if(Q() %in% c("S2.2.4.1", "S2.2.4.2")){
      respuestas <- c("Muy grave", "Algo grave", "Poco grave", "Nada grave", "NS/NR")
    } else if(Q() %in% c("S3.A.3.1", "S3.E.3.20.1",
                         "S3.E.3.20.2", "S3.E.3.20.3")){
      respuestas <- c("Sí", "No", "NS/NR")
    } else if(Q() %in% c("S3.B.3.4.1", "S3.B.3.4.2",
                         "S3.B.3.4.3", "S3.B.3.4.4", "S3.B.3.4.5", "S3.B.3.4.6")) {
      respuestas <- c("Mucho", "Algo", "Poco", "Nada", "NS/NR")
    } else if(Q() %in% c("S4.A.4.1", "S4.A.4.2.1",
                         "S4.A.4.2.2", "S4.A.4.2.3",
                         "S4.A.4.2.4", "S4.A.4.2.5")){
      respuestas <- c("Seguro", "Inseguro", "No aplica", "NS/NR")
    } else if(Q() == "S4.C.4.9..cualquier.delito."){
      respuestas <- c("Sí", "No")
    } else {
      respuestas <- "Error"
    }

    g <- data.frame(
      respuesta = respuestas,
      prop = data()[,1])

    g$ymax = cumsum(g$prop)
    g$ymin = c(0, head(g$ymax, n=-1))
    g$labelPosition <- (g$ymax + g$ymin) / 2
    g$label <- paste0(g$respuesta, "\n", round(100*g$prop,2),"%")

    g
    colores <- c(1:18)
    ggplot(g, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=respuesta)) +
      geom_rect(color = "black") +
      geom_label_repel(x=3.5, aes(y=labelPosition, label=label), size=4.4, fill = "white") +
      scale_fill_brewer(palette= sample(colores, 1)) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  output$pregunta <- renderText({
    paste0('"', Qstn(), '"')
  })
  
  
  #
  output$barras <- renderPlot({
    
    nsnr <- case_when(Q() %in% c("S2.2.4.2","S2.2.4.1") ~ 99,
                      Q() %in% c("S3.B.3.4.1", "S3.B.3.4.2",
                                 "S3.B.3.4.3", "S3.B.3.4.4", "S3.B.3.4.5", "S3.B.3.4.6") ~ 5,
                      Q() %in% c("S3.A.3.1", "S3.E.3.20.1",
                                 "S3.E.3.20.2", "S3.E.3.20.3",
                                 "S4.A.4.1", "S4.A.4.2.1",
                                 "S4.A.4.2.2", "S4.A.4.2.3",
                                 "S4.A.4.2.4", "S4.A.4.2.5",
                                 "S4.C.4.9..cualquier.delito.") ~ 9,
                      TRUE ~ 999)
    nopc <- case_when(Q() %in% c("S2.2.4.2","S2.2.4.1", "S3.B.3.4.1", "S3.B.3.4.2",
                                 "S3.B.3.4.3", "S3.B.3.4.4", "S3.B.3.4.5", "S3.B.3.4.6") ~ 5,
                      Q() %in% c("S3.A.3.1", "S3.E.3.20.1",
                                 "S3.E.3.20.2", "S3.E.3.20.3") ~ 3,
                      Q() %in% c("S4.A.4.1", "S4.A.4.2.1",
                                 "S4.A.4.2.2", "S4.A.4.2.3",
                                 "S4.A.4.2.4", "S4.A.4.2.5") ~ 4,
                      Q() == "S4.C.4.9..cualquier.delito." ~ 2,
                      TRUE ~ 999)
    
    if(Q() %in% c("S2.2.4.1", "S2.2.4.2")){
      respuestas <- c("Muy grave", "Algo grave", "Poco grave", "Nada grave", "NS/NR")
    } else if(Q() %in% c("S3.A.3.1", "S3.E.3.20.1",
                         "S3.E.3.20.2", "S3.E.3.20.3")){
      respuestas <- c("Sí", "No", "NS/NR")
    } else if(Q() %in% c("S3.B.3.4.1", "S3.B.3.4.2",
                         "S3.B.3.4.3", "S3.B.3.4.4", "S3.B.3.4.5", "S3.B.3.4.6")) {
      respuestas <- c("Mucho", "Algo", "Poco", "Nada", "NS/NR")
    } else if(Q() %in% c("S4.A.4.1", "S4.A.4.2.1",
                         "S4.A.4.2.2", "S4.A.4.2.3",
                         "S4.A.4.2.4", "S4.A.4.2.5")){
      respuestas <- c("Seguro", "Inseguro", "No aplica", "NS/NR")
    } else if(Q() == "S4.C.4.9..cualquier.delito."){
      respuestas <- c("Sí", "No")
    } else {
      respuestas <- "Error"
    }
    
    peor <- case_when(Q() %in% c("S2.2.4.1","S2.2.4.2") ~ "Muy grave",
                      Q() == "S3.A.3.1" ~ "Sí",
                      Q() %in% c("S3.B.3.4.1",
                               "S3.B.3.4.2",
                               "S3.B.3.4.3",
                               "S3.B.3.4.4",
                               "S3.B.3.4.5",
                               "S3.B.3.4.6") ~ "Nada",
                      Q() %in% c("S3.E.3.20.1",
                               "S3.E.3.20.2",
                               "S3.E.3.20.3") ~ "No",
                      Q() %in% c("S4.A.4.1",
                               "S4.A.4.2.1",
                               "S4.A.4.2.2",
                               "S4.A.4.2.3",
                               "S4.A.4.2.4",
                               "S4.A.4.2.5") ~ "Inseguro",
                      Q() == "S4.C.4.9..cualquier.delito." ~ "Sí")
    
    
    todos <- c("ZMG", "ZAPOTLÁN EL GRANDE", "PUERTO VALLARTA", "TEPATITLÁN DE MORELOS", "ARANDAS",
               "TALA", "OCOTLÁN")
    
    bases <- list()
    parametros <- list()
    estimaciones <- list()
    
    for(f in 1:length(todos)){
      
      bases[[f]] <- inner_join(empresas, datos, by = "id") %>%
        inner_join(corrupcion, by = "id") %>%
        inner_join(gobierno, by = "id") %>%
        inner_join(seguridad, by = "id") %>%
        select(municipio, zmg, estrato, fac_exp, matches(paste0("^",Q(),"$"))) %>%
        setNames(c("municipio", "zmg", "estrato", "fac_exp", "respuesta")) %>%
        filter(municipio == todos[f]) %>%
        mutate(respuesta = ifelse(is.na(respuesta), nsnr, respuesta))
      
      
      parametros[[f]] <- bases[[f]] %>%
        group_by(fac_exp, respuesta, .drop = FALSE) %>%
        summarise(empresas = n()) %>%
        mutate(ph = empresas/sum(empresas), nh = sum(empresas), Nh = fac_exp*nh, N = sum(bases[[f]]$fac_exp),
               SE2 = ifelse(nh>1, (1-nh/Nh)*(Nh/N)^2*ph*(1-ph)/(nh-1), (1-nh/Nh)*(Nh/N)^2*ph*(1-ph)/(nh)))
      
      p <- se <- c()
      resultado <- intconf <- list()
      estimaciones[[f]] <- matrix(nrow = nopc, ncol = 3)
      
      for(i in 1:nopc){
        if(Q() != "S4.C.4.9..cualquier.delito.") {
          if(i!=nopc){
            resultado[[i]] <- parametros[[f]] %>%
              filter(respuesta == i)
          } else {
            resultado[[i]] <- parametros[[f]] %>%
              filter(respuesta == nsnr) 
          }
        } else {
          resultado[[i]] <- parametros[[f]] %>%
            filter(respuesta == i) 
        }
        
        p[i] <- sum(resultado[[i]]$ph*resultado[[i]]$Nh/resultado[[i]]$N) # Estimación puntual
        se[i] <- sqrt(sum(resultado[[i]]$SE2)) # Error estándar
        intconf[[i]] <- p[i] + c(-1,1)*qnorm(.975)*se[i] # Intervalo de confianza
        estimaciones[[f]][i,] <- c(p[i], intconf[[i]])
      }
      
      estimaciones[[f]][estimaciones[[f]]<0] <- 0
      colnames(estimaciones[[f]]) <- c("Puntual", "Inf", "Sup")
      rownames(estimaciones[[f]]) <- respuestas
    }
    
    intervalos <- data.frame()
    
    for(j in 1:length(todos)){
      intervalos <- bind_rows(intervalos, estimaciones[[j]][peor,])
    }
    
    todos <- case_when(todos=="ZMG" ~ "ZMG",
                       todos=="ZAPOTLÁN EL GRANDE" ~ "Zapotlán el Grande",
                       todos=="PUERTO VALLARTA" ~ "Puerto Vallarta",
                       todos=="TEPATITLÁN DE MORELOS" ~ "Tepatitlán",
                       todos=="ARANDAS" ~ "Arandas",
                       todos=="TALA" ~ "Tala",
                       todos=="OCOTLÁN" ~ "Ocotlán")
    
    intervalos <- cbind(Municipio = todos, intervalos)
    
    colores <- c(1:5,7:18)
    ggplot(intervalos) +
      geom_bar(aes(x = factor(Municipio), y = Puntual, fill = factor(Municipio)), 
               stat = "identity",
               color = "black",
               width = .5) +
      geom_errorbar(aes(x = factor(Municipio), ymin = `Inf`, ymax = Sup),
                    width = 0.2, position = position_dodge(0.8)) + 
      geom_label(aes(x = factor(Municipio), y = Puntual,
                     label = paste0(round(100*Puntual,2),"%"))) +
      scale_y_continuous(breaks = trans_breaks(identity, identity, n = 6),
                         labels = percent)+
      scale_fill_brewer(palette = sample(colores, 1)) +
      theme(legend.position = "none",
            panel.grid.major.y  = element_line(colour = "grey", size = .3, 
                                               linetype = 'dashed'),
            panel.grid.minor.y  = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = "white"),
            axis.line = element_line(colour = 'black'), 
            axis.ticks = element_line(colour = 'black'),
            plot.title = element_text(hjust = .5),
            plot.caption = element_text(hjust =0),
            axis.text.x = element_text(angle = 90, face = "bold"),
            axis.text = element_text(size = 13),
            plot.subtitle = element_text(size = 15, hjust = 0.5)
      ) +
      labs(subtitle = paste0('Proporción de empresas por municipio que respondieron "',
                             peor, '"'),
           x = "", y = "Porcentaje")
    
    
  })

}
  
