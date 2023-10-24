library("tidyverse")
library("plyr")
library("dplyr")
library("here")
library("ggplot2")
library("stringr")
library('ggforce')
library('ggpubr')

#import data
var.data <- read_csv("Data/var_lit_clean_final.csv")

var.data <- as.data.frame(var.data)

#colors
blue <- c("#a9d6e5", "#89c2d9", "#61a5c2", "#468faf", "#2c7da0", "#2a6f97", "#014f86", "#014f86")
greenlim <- c("#aad576", "#73a942", "#538d22")
yellow <- c("#f9ea9a", "#FFDD00", "#FFB700", "#FF8800", "#ca6702")

#--------------------------------------------------------
  #full figure
#--------------------------------------------------------

#preliminary figures were first arranged using ggarrange, but final figures were re-arranged outside of R 
full <- ggarrange(B, C, D, E, F, G, H, I, J,
                  labels = c("B", "C", "D", "E", "F", "G", "H", "I", "J"),
                   nrow = 3, ncol = 3,
                  heights = c(1, 1.5, 1.5))

fullA <- ggarrange(A, full, labels = "A",
                   nrow = 2,
                   heights = c(1,3))

ggsave('fullfig.png',
       width = 9.36,
       height = 10.81,
       dpi = 600)

#--------------------------------------------------------------------------------
#a - time series of data types
#---------------------------------------------------------------------------------

#annotate data set with the category each study falls under
conditions <- list()

for(i in 1:nrow(var.data)) {
  conditions[[i]] <- if(var.data$inc_cvar[i] == "Yes") {
    conditions[[i]] <- "Include 𝚫𝛔^2"
  } else if (var.data$inc_var[i] == "Yes" && var.data$inc_cvar[i] == "No") {
    conditions[[i]] <- "Mention 𝚫𝛔^2"
  } else if (var.data$inc_mean[i] == "Yes" && var.data$inc_var[i] == "No" && var.data$inc_cvar[i] == "No") {
    conditions[[i]] <- "Include 𝚫μ"
  } else if (var.data$ment_mean[i] == "Yes" && var.data$inc_mean[i] == "No" && var.data$inc_var[i] == "No" && var.data$inc_cvar[i] == "No") {
    conditions[[i]] <- "Mention 𝚫μ"
  } else if (var.data$ment_mean[i] == "No") {
    conditions[[i]] <- "No Mention"
  }
}

conditions <- as.data.frame(t(conditions))

var.data$conditions <- conditions$V1
var.data$conditions <- unlist(var.data$conditions)

years <- ddply(var.data, c("Date", "conditions"), nrow)
names(years)[names(years) == 'V1'] <- 'n'

years_comp <- complete(years, conditions, Date = 1998:2023)
years_comp[is.na(years_comp)] <- 0

years_comp$aggcount <- ave(years_comp$n, years_comp$conditions, FUN = cumsum)

#time series area plot for studies in each category
A <- ggplot(years_comp, aes(x = Date, y = aggcount, fill = factor(conditions, levels = c("Include 𝚫𝛔^2", "Mention 𝚫𝛔^2", "Include 𝚫μ",
                                                                        "Mention 𝚫μ", "No Mention")))) +
  geom_area() +
  scale_fill_manual(values = c("#538d22","#aad576","#05668d","#a9d6e5","gray"), labels=c(expression("Include 𝚫𝛔"^2), expression("Mention 𝚫𝛔"^2), "Include 𝚫μ",
                                                                                         "Mention 𝚫μ", "No Mention")) +
  xlab("Year") +
  ylab("Number of Studies") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() + 
  guides(fill = guide_legend(title = "Category")) +
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.6),
    legend.text.align = 0)
  
#-------------------------------------------------------------------------------
  #b/c/d - maps 
#----------------------------------------------------------------------------------

  library(sf)
  library(tmap)
  library(RColorBrewer)
  library(cartogram)
  library(geodaData)

#import shapefiles for countries + continents  
countries <- read_sf(dsn = 'Data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp', layer = 'ne_50m_admin_0_countries') %>%
  st_transform(crs = '+proj=longlat')

#extract number of studies in each region
regs <- data.frame(count(var.data$region))
names(regs)[names(regs) == "x"] <- "CONTINENT"

#add number of studies in categories of interest, removing categories that are not needed
reggs <- ddply(var.data, c("region", "conditions"), nrow)
reggs$conditions[reggs$conditions == "Mention 𝚫μ"] <- NA
reggs$conditions[reggs$conditions == "No Mention"] <- NA
reggs$conditions[reggs$conditions == "Mention 𝚫𝛔^2"] <- NA
reggs <- na.omit(reggs)

regi <- spread(reggs, conditions, V1)
names(regi)[names(regi) == "region"] <- "CONTINENT"
regs <- left_join(regs, regi, by = "CONTINENT")

#for each region, add the number of studies in each category
regions <- left_join(countries, regs, by = "CONTINENT")
regions$freq[is.na(regions$freq)] <- 0
regions$freq <- regions$freq + 17 #add in global studies
names(regions)[names(regions) == "Include 𝚫μ"] <- "mean"
names(regions)[names(regions) == "Include 𝚫𝛔^2"] <- "var"

#b - studies in all countries
B <- tm_shape(regions) +
  tm_fill("freq", title = "Studies", palette = yellow, textNA = "0", legend.is.portrait = FALSE) +
  tm_layout(frame = FALSE,
            legend.outside.position = "top",
            legend.outside.size = 0.2,
            legend.outside = TRUE,
            legend.text.size = 1,
            legend.position = c(0.25, 0.25))
B <- tmap_grob(B)

tmap_save(filename = "countries.full.png",
          width = 7.64,
          height = 5.87,
          dpi = 600)

#c - studies that included changing mean 
C <- tm_shape(regions) +
  tm_fill("mean", title = "Studies", palette = blue, textNA = "0", legend.is.portrait = FALSE) +
  tm_layout(frame = FALSE,
            legend.outside.position = "top",
            legend.outside.size = 0.2,
            legend.outside = TRUE,
            legend.text.size = 1,
            legend.position = c(0.45, 0.25))
C <- tmap_grob(C)

tmap_save(filename = "countries.mean.png",
          width = 7.64,
          height = 5.87,
          dpi = 600)

#d - studies that included changing variance
D <- tm_shape(regions) +
  tm_fill("var", title = "Studies", palette = greenlim, textNA = "0", legend.is.portrai = FALSE) +
  tm_layout(frame = FALSE,
            legend.outside.position = "top",
            legend.outside.size = 0.2,
            legend.outside = TRUE,
            legend.text.size = 1,
            legend.position = c(0.45, 0.25))
D <- tmap_grob(D)

tmap_save(filename = "countries.var.png",
          width = 7.64,
          height = 5.87,
          dpi = 600)

#-------------------------------------------------------------------------------
# e/f/g - measure categories
#-------------------------------------------------------------------------------

#measure types for all data
var.data <- mutate(var.data, 
                    mean = ifelse(var.data$inc_mean == "Yes", "Yes", "No" ),
                    var = ifelse(var.data$inc_cvar == "Yes", "Yes", "No" ))

#extract the columns desired to a separate dataframe
stack.full <- var.data[,c(11, 12, 15, 23)]
stack.full$ID <- seq(1:100)
names(stack.full)[names(stack.full) == "no_static"] <- "Static"
names(stack.full)[names(stack.full) == "no_dynamic"] <- "Dynamic"
stack.full$Static <- as.numeric(stack.full$Static)
stack.full$Dynamic <- as.numeric(stack.full$Dynamic)
stack.full <- pivot_longer(stack.full, c("Static", "Dynamic"))
names(stack.full)[names(stack.full) == "name"] <- "type"
stack.full[is.na(stack.full)] <- 0

#add in the columns to explicitly plot out studies that included mean and variance
stack.full$varinc <- ifelse(stack.full$var == "No", "DNI", stack.full$type)
stack.full$meaninc <- ifelse(stack.full$mean == "No", "DNI", stack.full$type)

#order the data according to the number of dynamic measures used
ORDER <- stack.full[which(stack.full$type == "Dynamic"),]
ORDER <- aggregate(value ~ ID, data = ORDER, FUN = "sum")
ORDER <- ORDER[rev(order(ORDER$value)),]
stack.full$ID <- factor(stack.full$ID, ordered = TRUE, levels = ORDER$ID)

E <- ggplot(stack.full, aes(fill = factor(type,  levels = c("Static", "Dynamic")), y = value, x = ID)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#ffd000", "#ff9500")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(fill = "Measure") +
  ylab("Variables (n)") +
  xlab("Study") + 
  theme_classic() +
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.3, 0.85))

F <- 
  ggplot(stack.full, aes(fill = factor(meaninc,  levels = c("Static", "Dynamic", "DNI")), y = value, x = ID)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Dynamic" = "#05668d", "Static" = "#a9d6e5", "DNI" = "grey")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(fill = "Measure") +
  ylab("Variables (n)") +
  xlab("Study") + 
  theme_classic() +
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.3, 0.8))


G <- ggplot(stack.full, aes(fill = factor(varinc,  levels = c("Static", "Dynamic", "DNI")), y = value, x = ID)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Dynamic" = "#538d22", "Static" = "#aad576", "DNI" = "grey")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(fill = "Measure") +
  ylab("Variables (n)") +
  xlab("Study") + 
  theme_classic() +
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.3, 0.8))


#------------------------------------------------------------------------------
# h/i/j - variable types
#-------------------------------------------------------------------------------

#separate out only studies that included changing mean or variance
mean.yes <- var.data %>%
  filter(inc_mean == "Yes")

var.yes <- var.data %>%
  filter(inc_cvar == "Yes")

#variable types for all data
variables <- tibble(
  Biodiversity = sum(grepl("Biodiversity", var.data$var_type)),
  Meteorological = sum(grepl("Meteorological", var.data$var_type)),
  Geophysical = sum(grepl("Geophysical", var.data$var_type)),
  'Human use' = sum(grepl("Human use", var.data$var_type)),
  Productivity = sum(grepl("Productivity", var.data$var_type))
)

variables.f <- as.data.frame(t(variables))
colnames(variables.f)[1] <- 'n'
variables.f <- rownames_to_column(variables.f, "vartype")

H <- ggplot(data = variables.f) + 
  geom_bar(aes(x = reorder(vartype, -n), y = n), stat = "identity", fill = "#ffd000") + 
  xlab("Variable Type") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    axis.text.x = element_text(angle = 15, vjust = 0.7, hjust = 0.5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#variable types for mean only
variables.mean <- tibble(
  Biodiversity = sum(grepl("Biodiversity", mean.yes$var_type)),
  Meteorological = sum(grepl("Meteorological", mean.yes$var_type)),
  Geophysical = sum(grepl("Geophysical", mean.yes$var_type)),
  'Human use' = sum(grepl("Human use", mean.yes$var_type)),
  Productivity = sum(grepl("Productivity", mean.yes$var_type))
)

variables.m <- as.data.frame(t(variables.mean))
colnames(variables.m)[1] <- 'n'
variables.m <- rownames_to_column(variables.m, "vartype")

I <- ggplot(data = variables.m) + 
  geom_bar(aes(x = reorder(vartype, -n), y = n), stat = "identity", fill = "#05668d") + 
  xlab("Variable Type") +
  ylab("Frequency") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() + 
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    axis.text.x = element_text(angle = 15, vjust = 0.7, hjust = 0.5),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#variable types for variance only
cvar.variables <- tibble(
  Biodiversity = sum(grepl("Biodiversity", var.yes$var_type)),
  Meteorological = sum(grepl("Meteorological", var.yes$var_type)),
  Geophysical = sum(grepl("Geophysical", var.yes$var_type)),
  'Human use' = sum(grepl("Human", var.yes$var_type)),
  Productivity = sum(grepl("Productivity", var.yes$var_type))
)

cvar.var.f <- as.data.frame(t(cvar.variables))
colnames(cvar.var.f)[1] <- 'n'
cvar.var.f <- rownames_to_column(cvar.var.f, "vartype")

J <- ggplot(data = cvar.var.f) + 
  geom_bar(aes(x = reorder(vartype, -n), y = n), stat = "identity", fill = '#538d22') + 
  xlab("Variable Type") +
  ylab("Frequency") +
  scale_y_continuous(breaks = c(1, 2), expand = c(0,0)) +
  theme_classic() + 
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    axis.text.x = element_text(angle = 15, vjust = 0.7, hjust = 0.5),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
regions$freq[is.na(regions$freq)] <- 0
regions$freq <- regions$freq + 17 #add in global studies
names(regions)[names(regions) == "Changing Mean"] <- "mean"
names(regions)[names(regions) == "Changing Variance"] <- "var"

#b - all countries
B <- tm_shape(regions) +
  tm_fill("freq", title = "Studies", palette = yellow, textNA = "NA") +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside.size = 0.2,
            legend.outside = TRUE,
            legend.text.size = 0.5)
B <- tmap_grob(B)

#c - mean countries
C <- tm_shape(regions) +
  tm_fill("mean", title = "Studies", palette = blue, textNA = "NA") +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside.size = 0.2,
            legend.outside = TRUE,
            legend.text.size = 0.5)
C <- tmap_grob(C)

#d - var countries
D <- tm_shape(regions) +
  tm_fill("var", title = "Studies", palette = greenlim, textNA = "NA") +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside.size = 0.2,
            legend.outside = TRUE,
            legend.text.size = 0.5)
D <- tmap_grob(D)

#---------------------------------------------------------
# e/f/g - variable types
#-----------------------------------------------------------

mean.yes <- var.data %>%
  filter(inc_mean == "Yes")

var.yes <- var.data %>%
  filter(inc_cvar == "Yes")

#variable types for all data
variables <- tibble(
  Biodiversity = sum(grepl("Biodiversity", var.data$var_type)),
  Meteorological = sum(grepl("Meteorological", var.data$var_type)),
  Geophysical = sum(grepl("Geophysical", var.data$var_type)),
  'Human use' = sum(grepl("Human use", var.data$var_type)),
  Productivity = sum(grepl("Productivity", var.data$var_type))
)

variables.f <- as.data.frame(t(variables))
colnames(variables.f)[1] <- 'n'
variables.f <- rownames_to_column(variables.f, "vartype")

E <- ggplot(data = variables.f) + 
  geom_bar(aes(x = n, y = vartype), stat = "identity", fill = "#ffd000", color = "black") + 
  ylab("Variable Type") +
  xlab("Frequency") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic() + 
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#variable types for mean only
variables.mean <- tibble(
  Biodiversity = sum(grepl("Biodiversity", mean.yes$var_type)),
  Meteorological = sum(grepl("Meteorological", mean.yes$var_type)),
  Geophysical = sum(grepl("Geophysical", mean.yes$var_type)),
  'Human use' = sum(grepl("Human use", mean.yes$var_type)),
  Productivity = sum(grepl("Productivity", mean.yes$var_type))
)

variables.m <- as.data.frame(t(variables.mean))
colnames(variables.m)[1] <- 'n'
variables.m <- rownames_to_column(variables.m, "vartype")

F <- ggplot(data = variables.m) + 
  geom_bar(aes(x = n, y = vartype), stat = "identity", fill = "#05668d", color = "black") + 
  ylab("Variable Type") +
  xlab("Frequency") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic() + 
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#variable types for variance only
cvar.variables <- tibble(
  Biodiversity = sum(grepl("Biodiversity", var.yes$var_type)),
  Meteorological = sum(grepl("Meteorological", var.yes$var_type)),
  Geophysical = sum(grepl("Geophysical", var.yes$var_type)),
  'Human use' = sum(grepl("Human", var.yes$var_type)),
  Productivity = sum(grepl("Productivity", var.yes$var_type))
)

cvar.var.f <- as.data.frame(t(cvar.variables))
colnames(cvar.var.f)[1] <- 'n'
cvar.var.f <- rownames_to_column(cvar.var.f, "vartype")

G <- ggplot(data = cvar.var.f) + 
  geom_bar(aes(x = n, y = vartype), stat = "identity", fill = '#679436', color = "black") + 
  ylab("Variable Type") +
  xlab("Frequency") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_flip() +
  theme_classic() + 
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#---------------------------------------------------------
# h/i/j - measure categories
#-----------------------------------------------------------

#code works, but reorder according to number of dynamic measures
library(forcats)

#measure types for all data
stack.full <- var.data[,c(1, 11, 12)]
names(stack.full)[names(stack.full) == "no_static"] <- "static"
names(stack.full)[names(stack.full) == "no_dynamic"] <- "dynamic"
stack.full$dynamic <- as.numeric(stack.full$dynamic)
stack.full$static <- as.numeric(stack.full$static)
stack.full <- pivot_longer(stack.full, c("static", "dynamic"))

H <- ggplot(stack.full, aes(fill = name, y = value, x = fct_rev(fct_infreq(Author)))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#ff9500", "#ffd000")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(fill = "Measure Type") +
  ylab("Proportion") +
  xlab("Study") + 
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#measure type for mean only
stack.mean <- mean.yes[,c(1, 11, 12)]
names(stack.mean)[names(stack.mean) == "no_static"] <- "static"
names(stack.mean)[names(stack.mean) == "no_dynamic"] <- "dynamic"
stack.mean$dynamic <- as.numeric(stack.mean$dynamic)
stack.mean$static <- as.numeric(stack.mean$static)
stack.mean <- pivot_longer(stack.mean, c("static", "dynamic"))

I <- ggplot(stack.mean, aes(fill = name, y = value, x = Author)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#05668d", "#a9d6e5")) +
  scale_y_discrete(expand = c(0,0)) +
  labs(fill = "Measure Type") +
  ylab("Proportion") +
  xlab("Study") + 
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#measure type for variance only
stack.var <- var.yes[,c(1, 11, 12)]
names(stack.var)[names(stack.var) == "no_static"] <- "static"
names(stack.var)[names(stack.var) == "no_dynamic"] <- "dynamic"
stack.var$dynamic <- as.numeric(stack.var$dynamic)
stack.var$static <- as.numeric(stack.var$static)
stack.var <- pivot_longer(stack.var, c("static", "dynamic"))

J <- ggplot(stack.var, aes(fill = name, y = value, x = Author)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#679436", "#e8fccf")) +
  scale_y_discrete(expand = c(0,0)) +
  labs(fill = "Measure Type") +
  ylab("Proportion") +
  xlab("Study") + 
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
