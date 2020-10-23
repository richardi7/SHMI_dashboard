theme_i7 <- function(base_size=10, font=NA){
  theme(
    line = element_line(color = "lightblue"),
    rect = element_rect( colour = NA,  linetype = 1),
    text = element_text(color = "steelblue"),
    axis.line = element_line(size = rel(0.8)),
    axis.line.y = element_blank(),
    axis.text = element_text(size = rel(1)),
    axis.ticks = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    panel.grid.major  = element_line(color = "white"),
    panel.background = element_blank(),
    panel.grid.minor = element_line(linetype = "dotted"),
    panel.border = element_rect(color = "lightblue", fill = NA),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.title = element_blank(),
    legend.position= "bottom",
    plot.title = element_text(size = rel(1.5),hjust = 0, face = "bold"),
    plot.margin = unit(c(6, 5, 6, 5) * 2, "points")
  )
}

# colour groups
RAG.colors <- c(Red = "#FF0000", Green = "#00B050")
color_group <- c("blue","orange","darkblue")
lines_group<-c("steelblue","orange")
SHMI_color_group <- c("grey","steelblue","grey")