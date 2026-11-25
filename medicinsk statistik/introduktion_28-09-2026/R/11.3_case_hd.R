library(ggplot2)

hd_data |> 
  ggplot(
    aes(x=HD,y=Age)
  )+
  geom_boxplot()
