options(width=72)
options(digits=3)
nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR",package="memisc")
nes1948 <- spss.portable.file(nes1948.por)
print(nes1948)

names(nes1948)

codebook(nes1948)


vote.48 <- subset(nes1948,
              select=c(
                  v480018,
                  v480029,
                  v480030,
                  v480045,
                  v480046,
                  v480047,
                  v480048,
                  v480049,
                  v480050
                  ))


str(vote.48)

vote.48 <- rename(vote.48,
                  v480018 = "vote",
                  v480029 = "occupation.hh",
                  v480030 = "unionized.hh",
                  v480045 = "gender",
                  v480046 = "race",
                  v480047 = "age",
                  v480048 = "education",
                  v480049 = "total.income",
                  v480050 = "religious.pref"
        )

codebook(vote.48)

vote.48 <- within(vote.48,{
  vote3 <- recode(vote,
    1 -> "Truman",
    2 -> "Dewey",
    3:4 -> "Other"
    )
  occup4 <- recode(occupation.hh,
    10:20 -> "Upper white collar",
    30 -> "Other white collar",
    40:70 -> "Blue collar",
    80 -> "Farmer"
    )
  relig3 <- recode(religious.pref,
    1 -> "Protestant",
    2 -> "Catholic",
    3:5 -> "Other,none"
    )
   race2 <- recode(race,
    1 -> "White",
    2 -> "Black"
    )
  })

xtabs(~vote3+occup4,data=vote.48)

t(genTable(percent(vote3)~occup4,data=vote.48))

t(genTable(percent(vote3)~relig3,data=vote.48))


t(genTable(percent(vote3)~race2,data=vote.48))

t(genTable(percent(vote3)~total.income,data=vote.48))

print(agg.inc <- Aggregate(percent(vote3,ci=TRUE)~total.income,data=vote.48))

if(interactive())
  old.prompt <- devAskNewPage(TRUE)

xyplot(cbind(Percentage,upper,lower)~total.income,
        data=subset(agg.inc,vote3=="Truman"),
        panel=panel.errbars,
        xlab="Household income",
        ylab="Percentage voting for Truman",
        pch=19,ewidth=0.025,
        scales=list(x=list(
          rot=90
          )),
        )

agg.occup <- Aggregate(percent(vote3,ci=TRUE)~occup4,data=vote.48)

xyplot(cbind(Percentage,upper,lower)~occup4,
        data=subset(agg.occup,vote3=="Truman"),
        panel=panel.errbars,
        xlab="Occupation head of household",
        ylab="Percentage voting for Truman",
        pch=19,ewidth=0.025,
        scales=list(x=list(
          rot=90
          )),
        )

if(interactive())
  devAskNewPage(old.prompt)


vote.48 <- within(vote.48,{
  numeric.t.income <- as.numeric(total.income)
  contrasts(occup4) <- contr("treatment",base = 3)
  contrasts(total.income) <- contr("treatment",base = 4)
  })
model1 <- glm((vote3=="Truman")~occup4,data=vote.48,
              family="binomial")
model2 <- glm((vote3=="Truman")~numeric.t.income,data=vote.48,
              family="binomial")
model3 <- glm((vote3=="Truman")~occup4+numeric.t.income,data=vote.48,
              family="binomial")
model4 <- glm((vote3=="Truman")~total.income,data=vote.48,
              family="binomial")
model5 <- glm((vote3=="Truman")~occup4+total.income,data=vote.48,
              family="binomial")
model6 <- glm((vote3=="Truman")~relig3,data=vote.48,
              family="binomial")
model7 <- glm((vote3=="Truman")~occup4+relig3,data=vote.48,
              family="binomial")
mtable(model1,model2,model3,summary.stats=c("Deviance","AIC","N"))
mtable(model1,model4,model5,summary.stats=c("Deviance","AIC","N"))
mtable(model1,model6,model7,summary.stats=c("Deviance","AIC","N"))


