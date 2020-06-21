library(triangle)

n <- 10000
rho <- 0.65
theta <- acos(rho)
x1 <- rnorm(n, 1, 1)
x2 <- rnorm(n, 2, 0.5)


x1 <- triangle::rtriangle(a = 1, b = 2, c = 1.5, n)
x2 <- triangle::rtriangle(a = 1.2, b = 2.2, c = 1.7, n)

pre_results <- tibble(one = x1,
                  two = x2)

cor(pre_results$one, pre_results$two)

ggplot(pre_results, aes(one, two))+
  geom_point()

x <- cbind(x1, x2)

##this subtracts the mean of each sample from every value
xctr <- scale(x, center = T, scale = F)

##create nxn identity matrix
id <-diag(n)

q <- qr.Q(qr(xctr[ , 1, drop=FALSE]))  

p <- tcrossprod(q) 

x2o <- (id-p) %*% xctr[ , 2]  

xc2  <- cbind(xctr[ , 1], x2o) 

y <- xc2 %*% diag(1/sqrt(colSums(xc2^2)))

xfinal <- y[ , 2] + (1 / tan(theta)) * y[ , 1] 

cor(x1, xfinal)

results <- tibble(one = x1,
                  two = xfinal) %>% 
  mutate(two = two + mean(x2))

cor(results$one, results$two)

ggplot(results, aes(one, two))+
  geom_point()
 
max(results$two)

mean(results$two)

min1 <- 1
ml1 <- 1.2
max1 <- 2

n <- 10000

tri1 <- rtriangle(n, min1, max1, ml1)

tri1_df <- tibble(value = tri1)

ggplot(tri1_df)+
  geom_density(aes(tri1))

