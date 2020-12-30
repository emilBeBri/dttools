## code to prepare `DATASET` dataset goes here

setwd('/home/emil/Dropbox/Statistik_neworder/R/R-package-development/dttools')



##############  #afsnit #################
#  farveskalaer/farvepaletter  
#########################################

library(data.table)
library(purrr)

# base R colors
base_r_stampcolor <-  data.table(
	color=grDevices::colors(),hex=gplots::col2hex(grDevices::colors()),
	grp='base',
  note='base-R'
)


#iwanthue (11)
pal_iwanthue11 <-  c("#6db643","#bdad45","#7263d0","#e28337","#7183ca","#d2413f","#d14385","#5c7c37","#c66774","#45b0cf","#b15b38")
iwanthue11_stampcolor <-  data.table(
	hex=pal_iwanthue11,
	grp='iwanthue-11',
  note='iwanthue'
)



#iwanthue_long
pal_iwanthue200 <-c("#e15dad", "#26b641", "#ae4acd", "#77ce3f", "#834cd0", "#69d75d", "#9330ab", "#65c03e", "#3160e1", "#67ad1e", "#c572f3", "#a1c131", "#9e5ad5", "#3f931e", "#bf209e", "#49cc70", "#eb54c7", "#61ab38", "#c656ca", "#8fc652", "#7b74ef", "#bebd35", "#3a58c4", "#e59d1a", "#6d4bba", "#dcb838", "#6383ed", "#ef8d14", "#4c8ae7", "#edb442", "#9367d4", "#36a854", "#ea4aaf", "#42d79d", "#ce268c", "#3e9237", "#ec7eed", "#719726", "#b1379e", "#8ad172", "#9a48ae", "#6eb559", "#dc62c7", "#257326", "#f34a9a", "#36b278", "#e83377", "#6dbd73", "#c46fd8", "#9d9c28", "#9e82ec", "#ac850f", "#5b5cbb", "#d5942e", "#3295e9", "#e37021", "#3ba7e5", "#e75f29", "#41d3ec", "#ca2d22", "#33d4d1", "#e62d58", "#58d7c2", "#d0283f", "#3cb48e", "#c7317c", "#70cb92", "#a71d71", "#429757", "#bf60b8", "#517c28", "#a56cc9", "#767c16", "#bf90ed", "#445a06", "#e994e9", "#1f814d", "#f365a2", "#96c676", "#8c3b8d", "#b7bd61", "#82499f", "#859e45", "#bf4b9a", "#457636", "#d380d4", "#576c1c", "#9374c9", "#c4a642", "#3563af", "#c34112", "#4abfe8", "#ee543f", "#2da996", "#cd3758", "#2d8f6d", "#b7265f", "#7fcca4", "#e6537f", "#235e31", "#e87cbe", "#306a3c", "#f09ede", "#415a1f", "#c79ae2", "#e08334", "#4787c9", "#f07641", "#6da8ec", "#a94e0e", "#79c1ef", "#b63f30", "#39afb6", "#ee5f60", "#3fa1cc", "#f17856", "#2178a3", "#f1ab57", "#6b59a8", "#d69d4b", "#585296", "#af7422", "#6782c9", "#c56332", "#8da0e4", "#9d431d", "#4882b0", "#a92e38", "#6cae86", "#9e3a7a", "#8fb371", "#9e2c5a", "#5c9766", "#cc5d96", "#437d4e", "#c54c79", "#115e41", "#ee77a0", "#226a4d", "#f78183", "#2f7b63", "#be4762", "#6f9554", "#a76fb8", "#637b38", "#d383c2", "#4d662b", "#9d92db", "#806117", "#b7afed", "#5e5a1c", "#a95c9a", "#a5a563", "#755393", "#cfba78", "#3e5e97", "#f69b73", "#6c93c5", "#a3883e", "#7d7bb8", "#a97138", "#ca92c9", "#7a7a39", "#eaa3d2", "#844c24", "#a772a7", "#d8a46d", "#864e78", "#e5a37c", "#9a4876", "#8e7c49", "#d98db7", "#896338", "#f593ab", "#8e4229", "#eda4ba", "#9d633f", "#b77098", "#b27f56", "#98334c", "#f19c8f", "#7e3e53", "#f48671", "#925065", "#d37d59", "#a64d6c", "#ce6552", "#d17492", "#aa5b40", "#e4969a", "#8f494f", "#e46f84", "#994f45", "#e17976", "#a8484d", "#d38275", "#d05c61", "#b96f74", "#b6685d", "#c76674")
iwanthue200_stampcolor <-  data.table(
  hex=pal_iwanthue200,
  grp='iwanthue-200',
  note='iwanthue'
)

# xmen 
pal_xmen = c("#026CCBFF", "#F51E02FF" ,"#05B102FF" ,"#FB9F53FF" ,"#9B9B9BFF", "#FB82BEFF" ,"#BA6222FF"  ,    "#EEC229FF" )
xmen_stampcolor <-  data.table(
  hex=pal_xmen,
  grp='xmen'
)

#######  #subsection ######
#Nord (16)
#https://github_com/arcticicestudio/nord

pal_nord_polarnight <- c("#2E3440", "#3B4252", "#434C5E", "#4C566A")
nord_polarnight_stampcolor <-  data.table(
  hex=pal_nord_polarnight,
  grp='nord polarnight'
)

pal_nord_snowstorm <- c("#D8DEE9", "#E5E9F0", "#ECEFF4")
nord_snowstorm_stampcolor <-  data.table(
  hex=pal_nord_snowstorm,
  grp='nord snowstorm'
)

pal_nord_frost <- c("#8FBCBB", "#88C0D0", "#81A1C1", "#5E81AC")
nord_frost_stampcolor <-  data.table(
  hex=pal_nord_frost,
  grp='nord frost'
)


pal_nord_aurora = c("#BF616A", "#D08770", "#EBCB8B", "#A3BE8C", "#B48EAD", "#5E81AC")
nord_aurora_stampcolor <-  data.table(
  hex=pal_nord_aurora,
  grp='nord aurora'
)

##### subsection over #####



#######  #subsection ######
# Qualitative color schemes by Paul Tol

# FOCUS PALETTES
#The Focus palettes are colorsets designed to provide focus to the data graphed as the first element_ These palettes are best used when there is clearly an important data set for the viewer to focus on, with the remaining data being secondary, tertiary, etc_ Later elements are shown in diminishing values of gray_ These were generated with RColorBrewer, using the 8 level “grays” palette and replacing the darkest gray with the focus color    

# Red as highlight
pal_redfocus = c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
red_stampcolor <-  data.table(
  hex=pal_redfocus,
  grp='red focus',
  note='in color in focus, the rest is background'
)

# Green as highlight
pal_greenfocus = c("#41AB5D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
green_stampcolor <-  data.table(
  hex=pal_greenfocus,
  grp='green focus',
  note='in color in focus, the rest is background'
)

# Blue as highlight
pal_bluefocus = c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
blue_stampcolor <-  data.table(
  hex=pal_bluefocus,
  grp='blue focus',
  note='in color in focus, the rest is background'
)

##### subsection over #####


#######  #subsection ######
# Paul Tols own

pal_tol2=c("#4477AA", "#CC6677")
tol2_stampcolor <-  data.table(
  hex=pal_tol2,
  grp='paul tol 2',
  note='gender'
)

pal_tol3=c("#4477AA", "#DDCC77", "#CC6677")
tol3_stampcolor <-  data.table(
  hex=pal_tol3,
  grp='paul tol 3',
  note='gender, with neutral value'
)

pal_tol14=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol14_stampcolor <-  data.table(
  hex=pal_tol14,
  grp='paul tol 14'
)

##### subsection over #####

# classic easy on the eyes: roed, gul, groen, blaa
pal_classic4 <- c("#75ABDB","#E65439","#A3C587","#FCBF49")

classic4_stampcolor <-  data.table(
  hex=pal_classic4,
  grp='classic',
  note='classic easy on the eyes'
)


##############  #afsnit #################
# put into package
#########################################

a1 <- ls(pattern='stampcolor')
ecolor_data <- map_dfr(a1, get)

# hvor mange farver er der i skalaen?
ecolor_data[, no_colors := .N, by=.(grp)]

# export to package
usethis::use_data(ecolor_data, overwrite = TRUE)



