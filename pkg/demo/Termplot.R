if(interactive())
  old.prompt <- devAskNewPage(TRUE)

lm0 <- lm(sr ~ pop15 + pop75,              data = LifeCycleSavings)
lm1 <- lm(sr ~                 dpi + ddpi, data = LifeCycleSavings)
lm2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

berkeley <- aggregate(Table(Admit,Freq)~.,data=UCBAdmissions)
berk0 <- glm(cbind(Admitted,Rejected)~1,data=berkeley,family="binomial")
berk1 <- glm(cbind(Admitted,Rejected)~Gender,data=berkeley,family="binomial")
berk2 <- glm(cbind(Admitted,Rejected)~Gender+Dept,data=berkeley,family="binomial")

Termplot(lm2)
Termplot(berk2)
Termplot(lm0,lm1,lm2)
Termplot(berk0,berk1,berk2)

Termplot(By(~Gender,glm(cbind(Admitted,Rejected)~Dept,family="binomial"),
                    data=berkeley))
Termplot(By(~Dept,glm(cbind(Admitted,Rejected)~Gender,family="binomial"),
                    data=berkeley))

require(splines)
xyz <- data.frame(
  x = 1:100,
  z = factor(rep(LETTERS[1:4],25))
)
xyz <- transform(xyz,
  y = rnorm(100,sin(x/10)+x/50+as.numeric(z))
)
yxz.lin <- glm(y ~ x + z, data=xyz)
yxz.bs <- glm(y ~ bs(x,6) + z, data=xyz)
yxz.ns <- glm(y ~ ns(x,6) + z, data=xyz)
yxz.poly <- glm(y ~ poly(x,6) + z, data=xyz)
yxz.sincos <- glm(y ~ sin(x/10) + cos(x/10) + x + z, data=xyz)

# Terms containing
# the same variable are not plotted
# individually but their combined effect is plotted
#
Termplot(yxz.lin,yxz.bs,yxz.ns,yxz.poly,yxz.sincos,models="columns",
  span.smth=1/3)

Termplot(yxz.lin,yxz.bs,yxz.ns,yxz.poly,yxz.sincos,variables="x",
  span.smth=1/3)


if(interactive())
  devAskNewPage(old.prompt)
  