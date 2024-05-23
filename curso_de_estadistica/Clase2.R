# Create a data frame
ratings_df <- data.frame(
  Rating = c("Sobresaliente", "Excelente", "Buena", "Mala"),
  Count = c(102, 58, 30, 10)
)


# Calculate the total count
total_count <- sum(ratings_df$Count)


# Add a text string to the data frame
ratings_df$Text <- paste(ratings_df$Rating, ":", ratings_df$Count)
  

# Print the updated data frame
print(ratings_df)


# Print the total count
print(paste("Total count:", total_count))


# Load necessary library
library(ggplot2)


# Create a histogram
ggplot(ratings_df, aes(x = Rating, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Busqueda de Paginas Web") +
  xlab("Rating") +
  ylab("Count")


# Create a pie chart
pie(ratings_df$Count, 
    labels = ratings_df$Rating, 
    main = "Busqueda de Paginas Web", 
    col = rainbow(length(ratings_df$Count)))


################################
######### Ejemplo # 2 #########
###############################

# Creating data-frame
bebidas_df <- data.frame(
  Bebida = c("Coca-Plus", "Coca-Cola", "Pepsi", "Lima-limon"),
  Frecuencia = c(40, 25, 20,15)
)

# sum "Frecuencia"
abs_frequency = sum(bebidas_df$Frecuencia)
print(abs_frequency)


# relative frecuency
for(i in 1:nrow(bebidas_df)){
  bebidas_df$Frecuencia_relativa[i] <- round(bebidas_df$Frecuencia[i]/abs_frequency,2)
}

print(bebidas_df)

# bar chart
ggplot(bebidas_df, aes(x = Bebida, y = Frecuencia ))+
  geom_bar(stat = "identity", fill = "skyblue")+
  ggtitle("Tabla de Frecuencia")+
  xlab("Bebidas")
  ylab("Count")


  
  
