#Negative binomial model for TNRAP, PVUgt1 and PVUlt1
M2_TNRAP <- glm.nb(TNRAP ~ IMG2USMD1 + `Match year`, data= df)
summary(M2_TNRAP)
# Exponentiate coefficients - incidence rate ratios
exp(coef(M2_TNRAP))
# 95% CI
exp(cbind(Estimate = coef(M2_TNRAP), confint(M2_TNRAP)))

#Hurdle model for cPVU
#pt1 logistic regression for any cPVU vs. zero
part1 <- glm(I(cPVU > 0) ~ IMG2USMD1 + `Match year`,
family = binomial, data = df)
summary(part1)
exp(cbind(OR = coef(part1), confint(part1)))  # Odds ratios
# Part 2: Linear regression on log-transformed positive values
part2 <- lm(log(cPVU) ~ IMG2USMD1 + `Match year`,
data = subset(df, cPVU > 0))
summary(part2)
exp(coef(part2))
exp(confint(part2)
)

#Hurdle model for cARCS
part1ARCS <- glm(I(cARCS > 0) ~ IMG2USMD1 + `Match year`,
family = binomial, data = df)
summary(part1ARCS)
exp(cbind(OR = coef(part1ARCS), confint(part1ARCS)))  # Odds ratios
# Part 2: Linear regression on log-transformed positive values
part2ARCS <- lm(log(cARCS) ~ IMG2USMD1 + `Match year`,
data = subset(df, cARCS > 0))
summary(part2ARCS)
exp(coef(part2ARCS))
exp(confint(part2ARCS)
)
