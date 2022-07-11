# Udeli polova, senioriteta, velicina firmi i pozicija u odnosu na ukupne podatke
visualizationOfMetrics <- function(column, column_name){
  mean_value <- mean(column)
  print(paste("Mean value for column ", column_name, " is:", toString(mean_value)))
  median_value <- median(column)
  print(paste("Median value for column ", column_name, " is:", toString(median_value)))
  varinse_value <- var(column)
  print(paste("Varianse value for column ", column_name, " is:", toString(varinse_value)))
  deviation_value <- sd(column)
  print(paste("Standard deviation value for column ", column_name, " is:", toString(deviation_value)))
  num_of_elements <- length(column)
  print(paste("Number of usable elemets in column ", column_name, " is:", toString(num_of_elements)))
}

visulizePie <- function(column, title) {
  table <- table(column)
  labels <- names(table)
  percent <- round(table/length(column)*100, 2)
  full_labels <- paste(labels, percent)
  full_labels_percent <- paste(full_labels,"%",sep= "")
  pie3D(table,labels = full_labels_percent, col=hcl.colors(length(full_labels), "Dark 2"), border="white", main=title, mar = rep(1.75, 4))
}

visulizeLolipop <- function(column, xLabel, yLabel) {
  table <- table(column)
  labels <- names(table)
  percent <- round(table/length(column)*100, 2)
  
  df <- data.frame(x=labels, y = percent)
  ggplot(df, aes(x = reorder(labels, -y.Freq) , y = y.Freq)) +
    geom_segment(aes(x = reorder(labels, -y.Freq), xend = reorder(labels, -y.Freq), y = 0, yend = y.Freq),
                 color = "gray", lwd = 1) +
    geom_point(size = 10, pch = 21, bg = 4, col = 1) +
    xlab(xLabel)+
    ylab(yLabel)+
    geom_text(aes(label = y.Freq), color = "white", size = 3) +
    scale_x_discrete(labels= labels) +
    coord_flip() +
    theme_minimal()

}

visulizeBar <- function(column, xLabel, yLabel){
  table <- table(column)
  percent <- round(table/length(column)*100, 2)
  percent <- percent[ percent >= 1 ]
  
  labels <- names(percent)
  df <- data.frame(x=labels, y = percent)
  ggplot(df, aes(x = reorder(labels, -y.Freq) , y = y.Freq, color=labels)) +
    geom_bar(stat = "identity", width = 0.5, lwd=2, fill="white") +
    xlab(xLabel)+
    ylab(yLabel)+
    coord_flip()+
    theme(legend.position = "none")
}

visualizeSexEquality <- function(data) {
  ggplot(data, mapping = aes(x=Gender, fill=Size))+ 
    geom_bar(width = 0.5, position = "dodge", stat = "identity")+
    facet_wrap(~Size)+ 
    ggtitle("Equality")+ 
    labs(fill="Gender")
}
