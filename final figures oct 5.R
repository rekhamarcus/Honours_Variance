library("tidyverse")
library("dplyr")
library("here")
library("ggplot2")
library("stringr")
library('ggforce')
library('ggpubr')

colors <- c("salmon", "indianred3", "goldenrod1", "yellowgreen", "steelblue1", "mediumpurple", "orchid", "firebrick", "peachpuff1")

#change to keep mean and var together, swap out blue
main.colors <- c("yellowgreen", "goldenrod1", "indianred3", "mediumpurple", "steelblue1", "firebrick")
colors2 <- c("#05668d", "#679436","#427aa1","#a5be00","gray80")

yellow <- c("#ffea00", "#ffdd00", "#ffd000", "#ffc300", "#ffb700", "#ffaa00", "#ffa200", "#ff9500")

pink <- c("#fff0f3", "#ffccd5", "#ffb3c1", "#ff8fa3", "#ff758f", "#ff4d6d", "#c9184a", "#a4133c")

purple <- c("#e0aaff", "#c77dff", "#9d4edd", "#7b2cbf", "#5a189a", "#3c096c", "#240046", "#10002b")


var.data <- read_csv("Data/var_lit_clean_final.csv")

var.data <- as.data.frame(var.data)


#--------------------------------------------------------
  #sort data into different categories for easier plotting
#--------------------------------------------------------
  
  years <- var.data %>%
  count(Date, sort = TRUE) %>% 
  mutate(relative_frequency = n / sum(n))

countries <- var.data %>%
  count(loc, sort = TRUE) %>%
  mutate(relative_frequency = n/sum(n))
factor(countries$location, levels = countries$location[order(countries$n, decreasing = TRUE)])

regions <- var.data %>%
  count(region, sort = TRUE) %>%
  mutate(relative_frequency = n/sum(n))
regions <- regions %>% 
  mutate(end = 2 * pi * cumsum(n)/sum(n),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
regions$percent = round(100*regions$relative_frequency/sum(regions$relative_frequency), digits = 1)
regions$labels = paste(regions$region," (", regions$percent,"%)", sep = "")
regions <- regions[c(5, 1:4, 6:8),]

jif <- var.data %>%
  count(jif, sort = TRUE) %>% 
  mutate(relative_frequency = n / sum(n))
jif <- jif[-c(33, 34), ]
jif$jif <- as.numeric(jif$jif)

#code for final figure

#fix aspect ratio of maps, and fix map legends
full <- ggarrange(B, C, D, E, F, G, H, I, J,
                  labels = c("B", "C", "D", "E", "F", "G", "H", "I", "J"),
                   nrow = 3, ncol = 3,
                  heights = c(1, 1.5, 1.5))

fullA <- ggarrange(A, full, labels = "A",
                   nrow = 2,
                   heights = c(1,3))

ggsave('fullfig.png',
       width = 11.46,
       height = 10.68,
       dpi = 600)

#-------------------------------------------------------
#a - time series of data types
#-------------------------------------------------------

conditions <- list()

for(i in 1:nrow(var.data)) {
  conditions[[i]] <- if(var.data$inc_cvar[i] == "Yes") {
    conditions[[i]] <- "Changing Variance"
  } else if (var.data$inc_var[i] == "Yes" && var.data$inc_cvar[i] == "No") {
    conditions[[i]] <- "Included Variance"
  } else if (var.data$inc_mean[i] == "Yes" && var.data$inc_var[i] == "No" && var.data$inc_cvar[i] == "No") {
    conditions[[i]] <- "Changing Mean"
  } else if (var.data$ment_mean[i] == "Yes" && var.data$inc_mean[i] == "No" && var.data$inc_var[i] == "No" && var.data$inc_cvar[i] == "No") {
    conditions[[i]] <- "Mentioned Mean"
  } else if (var.data$ment_mean[i] == "No") {
    conditions[[i]] <- "No Mention"
  }
}

conditions <- as.data.frame(t(conditions))

var.data$conditions <- conditions$V1
var.data$conditions <- unlist(var.data$conditions)

years <- ddply(var.data, c("Date", "conditions"), nrow)
names(years)[names(years) == 'V1'] <- 'n'

#add in percentage labels !
#main$labels = c("No mention (43.2%)", "Mention change (29.5%)", "Changing mean (13.6%)",
                #"Variability (9.1%)", "Changing variability (4.5%)")
  
A <- ggplot(years, aes(x = Date, y = n, fill = factor(conditions, levels = c("Changing Variance", "Included Variance", "Changing Mean",
                                                                        "Mentioned Mean", "No Mention")))) +
  geom_area() +
  scale_fill_manual(values = c("#679436","#a5be00","#05668d","#427aa1","gray80")) +
  xlab("Year") +
  ylab("Number of Studies") +
  theme_classic() + 
  guides(fill = guide_legend(title = "Category"))
  theme(
    axis.title.x = element_text(size=10, family = "sans", face = "bold"),
    axis.title.y = element_text(size=10, family = "sans", face = "bold"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
  
#---------------------------------------------------------
  #b/c/d - maps - FIX AUSTRALIA STUDY !!!!
#-----------------------------------------------------------

  library(sf)
  library(tmap)
  library(RColorBrewer)
  library(cartogram)
  library(geodaData)
  library(stringr)
  library(ggplotify)
  
countries <- read_sf(dsn = 'Data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp', layer = 'ne_50m_admin_0_countries') %>%
  st_transform(crs = '+proj=longlat')

regions <- read_sf(dsn = 'Data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp', layer = 'TM_WORLD_BORDERS-0.3') %>%
  st_transform(crs = '+proj=longlat')
regions <- st_make_valid(regions)

blue <- c("#a9d6e5", "#89c2d9", "#61a5c2", "#468faf", "#2c7da0", "#2a6f97", "#014f86", "#014f86")
green <- c( "#e8fccf",  "#c0dfa1",  "#aad576", "#73a942", "#538d22", "#245501","#1a4301", "#143601" )
greenlim <- c("#aad576", "#73a942", "#538d22")
yellow <- c("#f9ea9a", "#FFDD00", "#FFB700", "#FF8800", "#ca6702")

yellow2 <- colorRampPalette(c("#f9ea9a","#ca6702"), alpha = T)

#aextract number of studies in each region
regs <- tibble(count(var.data$region))
names(regs)[names(regs) == "x"] <- "CONTINENT"

#add number of studies in categories of interest, removing categories that are not needed
reggs <- ddply(var.data, c("region", "conditions"), nrow)
reggs$conditions[reggs$conditions == "Mentioned Mean"] <- NA
reggs$conditions[reggs$conditions == "No Mention"] <- NA
reggs$conditions[reggs$conditions == "Included Variance"] <- NA
reggs <- na.omit(reggs)

regi <- spread(reggs, conditions, V1)
names(regi)[names(regi) == "region"] <- "CONTINENT"
regs <- left_join(regs, regi, by = "CONTINENT")

#for each region, add the number of studies in each category
regions <- left_join(countries, regs, by = "CONTINENT")
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
