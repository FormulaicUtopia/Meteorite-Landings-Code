library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(tidyverse)
library(hrbrthemes)

# Datasource
#https://catalog.data.gov/dataset/meteorite-landings (dataset source)
#https://www.fdmuseum.org/exhibit/m1-abrams-tank/ (tank weight & image source)

# Load data (relics represent 0.05% (0.00055) of data records)
meteorites = read.csv('PATH/Meteorite_Landings.csv')
meteorites = na.omit(meteorites)
meteorites_2013 = subset(meteorites,meteorites$year<=2013)
order_meteorites = meteorites_2013[order(meteorites_2013$year),]
order_meteorites$mass..g. = as.numeric(order_meteorites$mass..g.)
View(order_meteorites)

quant = quantile(order_meteorites$mass..g.,0.99)
data1 = subset(order_meteorites,order_meteorites$mass..g.<=quant)
mean_mass = mean(order_meteorites$mass..g.)
median_mass = median(order_meteorites$mass..g.)

# histogram
p = ggplot(order_meteorites, aes(x=mass..g.)) + 
  geom_histogram(bins = 15, fill="white", color="red", alpha=0.9) +
  geom_vline(xintercept = mean(order_meteorites$mass..g.), color = "blue", linetype = "longdash") +
  geom_vline(xintercept = median(order_meteorites$mass..g.), color = "black", linetype = "longdash") +
  labs(x ='Mass - g', y='Number of Landings', title = 'Distribution of Meteorite Landings Mass')+
  theme(
    plot.title = element_text(size=11)
  ) 
print(p)

# histogram less than %tile
p = ggplot(data1, aes(x=mass..g.)) + 
  geom_histogram(bins = 100, fill="white", color="red", alpha=0.9) +
  geom_vline(xintercept = mean_mass, color = "blue", linetype = "longdash") +
  geom_vline(xintercept = median_mass, color = "black", linetype = "longdash") +
  labs(x ='Mass - g', y='Number of Landings', title = 'Distribution of Meteorite Landings Mass (<99th %tile)')+
  theme(
    plot.title = element_text(size=11)
  ) 
print(p)


# Define quantile breaks
quantile_breaks = c(0, 0.05, 0.25, 0.50, 0.75, 0.95, 1)

# Create quantile groups
quantile_groups = cut(order_meteorites$mass..g., breaks = quantile(order_meteorites$mass..g., probs = quantile_breaks), labels = FALSE)

for(i in 1:length(quantile_groups)){
  if(quantile_groups[[i]]==1|is.na(quantile_groups[[i]])){
    quantile_groups[[i]] = '1)  -5th'
  }else if(quantile_groups[[i]]==2){
    quantile_groups[[i]] = '2)  5th-25th'
  }else if(quantile_groups[[i]]==3){
    quantile_groups[[i]] = '3)  25th-50th'
  }else if(quantile_groups[[i]]==4){
    quantile_groups[[i]] = '4)  50th-75th'
  }else if(quantile_groups[[i]]==5){
    quantile_groups[[i]] = '5)  75th-95th'
  }else if(quantile_groups[[i]]==6){
    quantile_groups[[i]] = '6)  95th+' 
  }
}

# Create a data frame with original data and quantile group
result_meteorites <- data.frame(order_meteorites, Quantile = quantile_groups)
View(result_meteorites)

# Plotting the world map
world_map = map_data("world")

p = ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgreen") +
  geom_point(data = result_meteorites, aes(x = reclong, y = reclat, size = Quantile), color = "black") +
  scale_size_discrete(range = c(1, 4)) +
  labs(title = paste("Map of Recorded Meteorites 860", 2013, sep = "-")) +
  theme_minimal()

# cumulative year plots
j = 0
for (i in seq(min(order_meteorites$year), max(order_meteorites$year))) {
  j = j + 1
  yearly_data <- subset(result_meteorites, result_meteorites$year < i)
  current_year_data <- subset(result_meteorites, result_meteorites$year == i)  
  # Create a plot with points
    
  p = ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgreen") +
      geom_point(data = yearly_data, aes(x = reclong, y = reclat, size = Quantile), color = "black") +
      geom_point(data = current_year_data, aes(x = reclong, y = reclat, size = Quantile), color = "red") +
      scale_size_discrete(range = c(1, 4)) +
      labs(title = paste("Map of Cumulative Recorded Meteorites 860", i, sep = "-")) +
      theme_minimal()
  
  # Save the plot using ggsave
  ggsave(paste(paste(paste(paste("PATH/Cumulative Landings Each Year/",j,sep=""),"Map of Cumulative Recorded Meteorites",sep="_"), i,sep="_"), ".png", sep = ""),
           plot = p, width = 10, height = 6)
}



# yearly plots
j = 0
for (i in seq(min(order_meteorites$year), max(order_meteorites$year))) {
  j = j + 1
  yearly_data <- subset(result_meteorites, result_meteorites$year < i)
  current_year_data <- subset(result_meteorites, result_meteorites$year == i)  
  if(nrow(current_year_data==0)){
    # Create a plot with points
    
    p = ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgreen") +
      scale_size_discrete(range = c(1, 4)) +
      labs(title = paste("Map of Recorded Meteorites", i, sep = " ")) +
      theme_minimal()
    
    # Save the plot using ggsave
    ggsave(paste(paste(paste(paste("PATH/Cumulative Landings Each Year/",j,sep=""),"Map of Cumulative Recorded Meteorites",sep="_"), i,sep="_"), ".png", sep = ""),
           plot = p, width = 10, height = 6) 
  }else{
    # Create a plot with points
    
    p = ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgreen") +
      geom_point(data = yearly_data, aes(x = reclong, y = reclat, size = Quantile), color = "black") +
      geom_point(data = current_year_data, aes(x = reclong, y = reclat, size = Quantile), color = "red") +
      scale_size_discrete(range = c(1, 4)) +
      labs(title = paste("Map of Recorded Meteorites", i, sep = " ")) +
      theme_minimal()
    
    # Save the plot using ggsave
    ggsave(paste(paste(paste(paste("PATH/Landings Each Year/",j,sep=""),"Map of Cumulative Recorded Meteorites",sep="_"), i,sep="_"), ".png", sep = ""),
           plot = p, width = 10, height = 6)
  }
}

