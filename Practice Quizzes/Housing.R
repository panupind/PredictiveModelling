housing

dim(housing)

str(housing)


names(housing)


# MEDV - DV

library(ggplot2)

housing.scale <- as.data.frame(scale(housing))

str(housing.scale)

housing$RM 

plot_RM <- ggplot(housing.scale, aes( y = housing.scale$MEDV , x = housing.scale$RM  ) ) +
  geom_point( size=2, shape=23 ) + xlab("MEDV") + ylab("RM")



plot(plot_RM)


mlm <- lm( housing$MEDV ~ . , data= housing)

summary(mlm)
