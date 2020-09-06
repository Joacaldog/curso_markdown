library(dplyr)
library(tidyverse)
data("iris")


#Group By y Summarise

DF <- summarise(iris, Mean_Petal_Length = mean(Petal.Length), SD_Petal_Length = sd(Petal.Length))

By_species <- group_by(iris, Species)

DF2 <- summarise(By_species, Mean_Petal_Length = mean(Petal.Length), SD_Petal_Length = sd(Petal.Length))

# agrupar por mas de una variable

data("mtcars")
by_cyl_am <- group_by(mtcars, cyl, am)
DF3 <- summarise(by_cyl_am, mean_mpg = mean(mpg), sd_mpg = sd(mpg))

#mutate
DF4 <- mutate(mtcars, ratio_hp_wt = hp/wt)

#pipeline

DF_pipeline <- iris %>% 
  mutate(ratio_petal_sepal = Petal.Length/Sepal.Length) %>% 
  group_by(Species) %>% 
  summarise(ratio = mean(ratio_petal_sepal), desv = sd(ratio_petal_sepal))

# summarise_all
DF_summ_all <- iris %>%
  group_by(Species) %>% 
  # summarise_all(.funs = list(Mean = mean, SD = sd, Median = median))
  summarise_at(.vars = c("Sepal.Length", "Sepal.Width"), .funs = list(Mean = mean, SD = sd, Median = median))
  
  
#Filter
DF_filter <- iris %>% 
  dplyr::filter(Species == "virginica", Petal.Length >=3)

DF_filter_2 <- mtcars %>% dplyr::filter(cyl %in% c(6,8)) %>% 
  group_by(am) %>% 
  summarise(consumo = mean(mpg), N = n())

DF_filter_iris <- iris %>% dplyr::filter(Petal.Length > 4.5) %>% 
  group_by(Species) %>% 
  summarise(N = n())

#Select
data(mpg)
DF_mpg_select <- mpg %>% dplyr::filter(class == "suv") %>% 
  dplyr::select(cty, hwy, cyl)

iris2 <- iris %>% dplyr::select(starts_with("Petal"), Species)

# mpg$class %>% unique()

#filtrar en columnas especificas

iris_con_NA <- iris
iris_con_NA[2,4] <- NA
iris_con_NA[3,2] <- NA
iris_con_NA[7,1] <- NA

Iris_filtrado <- iris_con_NA %>% dplyr::filter_at(vars(Sepal.Length:Petal.Length), ~!is.na(.x))


###FIN

  
  