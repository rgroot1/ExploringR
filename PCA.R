head(users)

users_pca <- users %>% select (-userId, -age,-sex,-occupation, -zipcode)

users_pca <- head(users_pca,100)

cor(users_pca)

pca <- prcomp(users_pca,
                   center = TRUE,
                   scale. = TRUE) 

print(pca)

# plot method
plot(pca, type = "l")
plot(pca$x[,1],pca$x[,2])

summary(pca)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)

barplot(pca.var.per, main='Scee Plot', xlab='Principal Component',ylab='Percent Variation')

biplot(pca)


