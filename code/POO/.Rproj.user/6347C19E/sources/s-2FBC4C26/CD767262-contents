grafico.OBP_algorithms <- function(datos, obp_algorithm, time, type_experiment, type_experiment.labs) {
  grafico <- ggplot(data=datos, mapping = aes(x = obp_algorithm, y=log10(time), fill=type_experiment)) +
    stat_boxplot(geom ='errorbar', width = 0.6) +
    geom_boxplot(width = 0.8) +
    facet_grid(~type_experiment, labeller = labeller(type_experiment = type_experiment.labs)) +
    scale_y_continuous(breaks = seq(0, to=10000, by=1000)) + 
    theme_bw() +
    xlab("Heurísticas de Agrupamiento de Pedidos") +
    ylab("Tiempos de ejecución (ns)") +
    theme(legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 14, colour = "black"),
          axis.line = element_line(colour = "black"))
  
  return (grafico)
}

grafico.PRP_algorithms <- function(datos, prp_algorithm, time, type_experiment, type_experiment.labs) {
  grafico <- ggplot(data=datos, mapping = aes(x = prp_algorithm, y=log10(time), fill=type_experiment)) +
    stat_boxplot(geom ='errorbar', width = 0.6) +
    geom_boxplot(width = 0.8) +
    facet_grid(~type_experiment, labeller = labeller(type_experiment = type_experiment.labs)) +
    scale_y_continuous(breaks = seq(0, to=10000, by=1000)) + 
    theme_bw() +
    xlab("Algoritmos de Recogida de Pedidos") +
    ylab("Tiempos de ejecución (ns)") +
    theme(legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 14, colour = "black"),
          axis.line = element_line(colour = "black"))
  
  return (grafico)
}

grafico.densidad <- function(datos) {
  ggplot(data = datos, aes(x=log10(time), y= ..density..)) + 
    geom_density(alpha= .25) +
    labs(title= 'Gráfico de densidad para los tiempos de ejecución', y= "Densidad", x= "Tiempos de ejecución (ns)")
}

grafico.LS_algorithms <- function(datos, ls_algorithm, time, type_experiment, type_experiment.labs) {
  grafico <- ggplot(data=datos, mapping = aes(x = ls_algorithm, y=log10(time), fill=type_experiment)) +
    stat_boxplot(geom ='errorbar', width = 0.6) +
    geom_boxplot(width = 0.8) +
    facet_grid(~type_experiment, labeller = labeller(type_experiment = type_experiment.labs)) +
    scale_y_continuous(breaks = seq(0, to=10000, by=1000)) + 
    theme_bw() +
    xlab("Heurísticas de Agrupamiento de Pedidos") +
    ylab("Tiempos de ejecución (ns)") +
    theme(legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 14, colour = "black"),
          axis.line = element_line(colour = "black"))
  
  return (grafico)
}