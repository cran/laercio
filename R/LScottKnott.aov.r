`LScottKnott.aov` <- function(anova,which="",conf.level=0.95) {
 variaveis <- names(anova$model)
 for (i in 2:length(variaveis)) {
 if (variaveis[i] == which) { 
 vari <- i
 stop }
 else { next } }
 medias <- sort(tapply(anova$model[[1]],anova$model[[vari]],mean),decreasing=T)
 sig <- 1 - conf.level
 nmedias <- length(medias)
 res <- rep(1,nmedias)
 time <- 1
 np <- nmedias - 1
 a <- tapply(anova$model[[1]]^2,anova$model[[2]],sum)
 b <- tapply(anova$model[[1]],anova$model[[2]],sum)
 c <- tapply(anova$model[[1]],anova$model[[2]],length)
 s2 <- sum(((a-(b^2/c))/anova$df.residual)/c)
 for (i in 1:np){
	g1 <- medias[i]
	g2 <- medias[(i+1)]
	B0 <- g1^2 + g2^2 - (g1 + g2)^2/2
	teste <- c(g1,g2)
	sigm2 <- (sum(teste^2) - sum(teste)^2/2 + anova$df.residual*s2)/(length(teste) + anova$df.residual)
	lamb <- pi*B0/(2*sigm2*(pi-2))
	v0 <- length(teste)/(pi-2)
	p <- pchisq(lamb,v0,lower.tail = F)
	if (p<sig) {
		res[i] <-  LETTERS[time]
		time <- time + 1
		res[i+1] <- LETTERS[time]}
	else {
		res[i] <-  LETTERS[time]
		res[i+1] <- LETTERS[time] } }
cat("\n","SCOTT-KNOTT TEST TO GROUP MEANS","\n","\n","Confidence level:",conf.level)
CV <- sqrt(sum(anova$residuals^2)/anova$df.residual)/mean(anova$model[[1]])
cat("\n","Variation Coefficient: ",CV*100,"%","\n","\n")
cat("\t",which,"\n")
resultado <- data.frame(medias,res)
names(resultado) <- c("Means"," ")
return(resultado)}