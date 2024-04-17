library(plotly)
library(umap)
library(dplyr)

load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")

umap_data = tdwg3@data
# umap_data = data.frame(umap_data[1:20,])
umap_data$LEVEL2_COD = as.numeric(umap_data$LEVEL2_COD)
umap_data$LEVEL1_COD = as.numeric(umap_data$LEVEL1_COD)
umap_data$COUNTRY = as.factor(umap_data$COUNTRY)
umap_data$LEVEL3_NAM = as.factor(umap_data$LEVEL3_NAM)
umap_data$LEVEL3_COD = as.factor(umap_data$LEVEL3_COD)

umap_data = umap_data %>%
  mutate(discovery_level = cut(discoveries ,
                               quantile(discoveries, c(0,.25,0.5, .75,1),
                                        na.rm=TRUE),
                               labels = c("Very Few", "Some", "Average","Many")))%>%
  mutate(description_level = cut(descriptions ,
                                 quantile(descriptions, c(0,.25,0.5, .75,1),
                                          na.rm=TRUE),
                                 labels = c("Very Few", "Some", "Average","Many")))

# umap.data <- umap_data#[,6:ncol(umap_data)]
umap.data <- umap_data[c( "discoveries", "descriptions",
                          "spp_left",
                          "left_spp_wcvp",
                          "tot_exp_spp_wcvp",
                          # "num_instit",
                          "Prop_6hrs",
                          "Perc_6hrs",
                          "e_peaveduc" , # education
                          "e_peedgini",  # education
                          "e_area" ,
                          "e_cow_exports",  # imports and exports
                          "e_cow_imports"  ,# imports and exports
                          "e_gdp" ,
                          "e_gdppc" ,
                          # "e_miinflat",
                          # "e_pop" ,
                          "e_total_resources_income_pc"   ,
                          "e_miferrat", # fertility rate
                          "e_mipopula", # population total
                          # "e_miurbani", # urbanisation rate
                          # "e_miurbpop", # urban population
                          "e_pelifeex", # life expectancy
                          "e_peinfmor", # infant mortality
                          "e_pematmor", # maternity mortality
                          "v2cldmovew" ,  # Freedom of domestic movement for women (C) (
                          "v2cldmovem",   # Freedom of domestic movement for men
                          "v2cafres" ,    # Freedom to research and teach
                          "e_civil_war" , # number of civil wars
                          # "e_miinteco",   # international armed conflict
                          "e_miinterc",  # internal armed conflic
                          "e_pt_coup"     # coups d'etat
)]
# umap.data <- umap.data[,4:(ncol(umap.data)-2)]
umap.data <- apply(umap.data,2,scale)
keep <- complete.cases(umap.data)
umap.data <- umap.data[keep,6:(ncol(umap.data))]

# umap.umap = umap(umap.data)
# head(umap.umap$layout, 3)

main = "left_spp_wcvp" # rate
umap.labels <- (umap_data["tot_exp_spp_wcvp"] -umap_data[c(main)])/umap_data["tot_exp_spp_wcvp"]
# umap.labels <-  apply(umap.labels,2,scale)
umap.labels <- umap.labels[keep,]


color_palette <- colorRampPalette(c("royalblue4","purple", "orange","yellow"))


# iris.data = iris[, grep("Sepal|Petal", colnames(iris))]
# iris.labels = iris[, "Species"]
umap.umap = umap(umap.data, n_components = 2)
layout <- umap.umap[["layout"]]
layout <- data.frame(layout)
final <- cbind(layout, umap.labels)

fig <- plot_ly(final, x = ~X1, y = ~X2, color = umap.labels,
               text= ~umap_data$COUNTRY[keep],
               colors = color_palette(length(umap.labels)),#c('#EF553B',"black",'#636EFA','#00CC96'),
               type = 'scatter', mode = 'markers')%>%
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='species')),
    xaxis = list(
      title = "0"),
    yaxis = list(
      title = "1"))
fig




umap.umap = umap(umap.data, n_components = 3, random_state = 15)
layout <- umap.umap[["layout"]]
layout <- data.frame(layout)
final <- cbind(layout,  umap.labels)

fig2 <- plot_ly(final, x = ~X1, y = ~X2, z = ~X3,
                text= ~umap_data$COUNTRY[keep],
                color = umap.labels,#~umap_data$discoveries[keep],
                colors = color_palette(length(umap.labels)))#c('#EF553B',"black",'#636EFA','#00CC96'))
fig2 <- fig2 %>% add_markers()
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = '0'),
                                     yaxis = list(title = '1'),
                                     zaxis = list(title = '2')))

# fig2
