# demo_mytsdecomp.R
#
# Demo script for the mytsdecomp package
# Shows how to decompose two series and swap in the benchmark trend

# Install/devtools (if you haven't installed from CRAN/dev)
# install.packages("devtools")
# devtools::install_local("path/to/mytsdecomp")

library(spadjustr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Prepare two example time series
# -----------------------------------------------------------------------------

# User series: AirPassengers (monthly international airline passenger numbers)
user_ts <- AirPassengers

# Benchmark series: a synthetic trend + seasonality + noise
set.seed(123)
time_idx <- time(user_ts)
benchmark_trend <- 100 + 0.5 * (1:length(user_ts))
benchmark_seasonal <- 10 * sin(2 * pi * cycle(user_ts) / 12)
benchmark_noise <- rnorm(length(user_ts), sd = 5)
benchmark_ts <- ts(benchmark_trend + benchmark_seasonal + benchmark_noise,
  start = start(user_ts), frequency = frequency(user_ts)
)

# Plot the two raw series
plot(time(user_ts), as.numeric(user_ts), geom = "line") +
  ggtitle("User Series: AirPassengers") +
  xlab("Year") + ylab("Passengers")

qplot(time(benchmark_ts), as.numeric(benchmark_ts), geom = "line") +
  ggtitle("Benchmark Series (synthetic)") +
  xlab("Year") + ylab("Value")

# -----------------------------------------------------------------------------
# 2. Decompose and replace trend with different methods
# -----------------------------------------------------------------------------

methods <- c("ma12", "stl", "loess", "prophet")

results <- lapply(methods, function(m) {
  message("=== Method: ", m, " ===")

  # Create the TsDecomp object
  td <- ts_decomp(user_ts, benchmark_ts, method = m)

  # Print summary
  td$summary()

  # Plot the four panels
  td$plot()

  return(td)
})

names(results) <- methods

# -----------------------------------------------------------------------------
# 3. Extract and compare components for one method (e.g. STL)
# -----------------------------------------------------------------------------

td_stl <- results[["stl"]]

# User trend vs. benchmark trend
df_trends <- data.frame(
  time        = time(user_ts),
  user_trend  = as.numeric(td_stl$decomposed_user$trend),
  bench_trend = as.numeric(td_stl$decomposed_benchmark$trend)
)

ggplot(df_trends, aes(x = time)) +
  geom_line(aes(y = user_trend), linetype = "dashed") +
  geom_line(aes(y = bench_trend)) +
  ggtitle("STL: Original User Trend (dashed) vs Benchmark Trend") +
  xlab("Year") +
  ylab("Trend")

# Reconstructed user series with benchmark trend
recon_ts <- td_stl$modified_user
qplot(time(recon_ts), as.numeric(recon_ts), geom = "line") +
  ggtitle("User Series Reconstructed with Benchmark STL Trend") +
  xlab("Year") + ylab("Value")

# -----------------------------------------------------------------------------
# 4. Save one set of components for further analysis
# -----------------------------------------------------------------------------

# Save the STL-decomposed components to CSV
stl_comp <- data.frame(
  time = as.Date(time(user_ts)),
  original = as.numeric(user_ts),
  trend = as.numeric(td_stl$decomposed_user$trend),
  seasonal = as.numeric(td_stl$decomposed_user$seasonal),
  remainder = as.numeric(td_stl$decomposed_user$idiosyncratic),
  modified = as.numeric(td_stl$modified_user)
)

write.csv(stl_comp, "stl_decomposition_demo.csv", row.names = FALSE)
message("STL components saved to stl_decomposition_demo.csv")
