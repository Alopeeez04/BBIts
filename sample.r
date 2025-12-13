# Load library
library(survival)

# Example dataset
# time  = follow-up time
# status = event indicator (1 = event, 0 = censored)
# x = a single covariate
data <- data.frame(
  time   = c(5, 8, 12, 3, 10, 7, 9, 4),
  status = c(1, 1, 0, 1, 0, 1, 0, 1),
  x      = c(0, 1, 1, 0, 0, 1, 0, 1)
)

# Fit Cox proportional hazards model
cox_model <- coxph(Surv(time, status) ~ x, data = data)

# View model summary
summary(cox_model)

# Create new data for prediction
new_data <- data.frame(x = c(0, 1))

# Fit survival curves
surv_fit <- survfit(cox_model, newdata = new_data)

# Plot
plot(
  surv_fit,
  col = c("blue", "red"),
  lwd = 2,
  xlab = "Time",
  ylab = "Survival probability"
)

legend(
  "bottomleft",
  legend = c("x = 0", "x = 1"),
  col = c("blue", "red"),
  lwd = 2
)

