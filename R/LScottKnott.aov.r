`LScottKnott.aov` <- function(anova,which="")
{
 sk <- function(medias,s2,dfr)
 {
	bo <- 0
	si2 <- s2
	defr <- dfr
	parou <- 0
	np <- length(medias) - 1 #numero de particoes

	for (i in 1:np)
	{
	g1 <- medias[1:i]
	g2 <- medias[(i+1):length(medias)]
	B0 <- sum(g1)^2/length(g1) + sum(g2)^2/length(g2) - (sum(g1) + sum(g2))^2/length(c(g1,g2))
	 if (bo < B0)
		{
		 bo <- B0;
		 parou <- i;
		}
	}
	
	g1 <- medias[1:parou]
	g2 <- medias[(parou+1):length(medias)]
	teste <- c(g1,g2)

	sigm2 <- (sum(teste^2) - sum(teste)^2/length(teste) + defr*si2)/(length(teste) + defr)

	lamb <- pi*bo/(2*sigm2*(pi-2))
	
	v0 <- length(teste)/(pi-2)
	
	p <- pchisq(lamb,v0,lower.tail = F)

	cat(names(g1), " / ", names(g2)," prob.: ", p,"\n")

	if (length(g1)>1)
	{
	sk(g1,s2,dfr)
	}
	if (length(g2)>1)
	{
	sk(g2,s2,dfr)
	}
}

 variaveis <- names(anova$model)
 
 for (i in 2:length(variaveis))
 {
 if (variaveis[i] == which)
 { 
 vari <- i
 stop }
 else { next }
 }
 
 medias <- sort(tapply(anova$model[[1]],anova$model[[vari]],mean),decreasing=T)
 dfr <- anova$df.residual

	a <- tapply(anova$model[[1]]^2,anova$model[[vari]],sum)
	b <- tapply(anova$model[[1]],anova$model[[vari]],sum)
	c <- tapply(anova$model[[1]],anova$model[[vari]],length)
	s2 <- sum(((a-(b^2/c))/anova$df.residual)/c)


cat("\n","SCOTT-KNOTT test","\n","\n")
cat(which,"\n","\n")
sk(medias,s2,dfr)
cat("\n","\n")
}