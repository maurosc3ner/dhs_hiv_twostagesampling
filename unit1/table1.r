M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals
----
M <- as.table(rbind(c(132, 131), c(16, 13),c(10,10)))
dimnames(M) <- list(gender = c("0", "0.5","1"),
                    party = c("trt1","trt2"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals


#test para variables categoricas
M <- as.table(rbind(c(132, 131), c(16, 13),c(10,10)))
dimnames(M) <- list(gender = c("0", "0.5","1"),
                    party = c("trt1","trt2"))
(Xsq <- chisq.test(M))  # Prints test summary

t.test(c(2015.62,1996.86))

M <- as.table(rbind(c(2015.62,1996.86)))
dimnames(M) <- list(gender = c("time"),
                    treatment = c("trt1","trt2"))
(Xsq <- chisq.test(M))  # Prints test summary
t.test(M)
t.test(M,alternative = "less")
t.test(2015.62,1996.86, alternative = "two.sided", var.equal = FALSE)
oneway.test(M,var.equal = T)

library(survival)
data(pbc)
library(dplyr)
group_by(pbc, trt) %>%
  summarise(
    count = n(),
    mean = mean(time, na.rm = TRUE),
    sd = sd(time, na.rm = TRUE)
  )
# test para variables continuas
t.test(time ~ trt, data = pbc, var.equal = TRUE)
