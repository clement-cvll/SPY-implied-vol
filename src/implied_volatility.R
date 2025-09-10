# SPY Options Price and Implied Volatility Surface

## 1. Configuration

```{r}
# Load necessary libraries
library(quantmod)
library(tidyverse)
library(janitor)
library(plotly)
library(akima)
```

```{r}
# Global parameters
TARGET_SYMBOL <- "SPY"
RISK_FREE_RATE <- 0.0425
MIN_OPTION_VOLUME <- 1
```

## 2. Data Fetching and Cleaning Function

```{r}
get_tidy_options_data <- function(symbol, min_volume) {
  # Fetch all option chains for the symbol
  options_chain <- getOptionChain(symbol, NULL)
  
  if (is.null(options_chain)) {
    stop("Could not retrieve options data for the specified symbol.")
  }
  
  # Tidy the nested list into a single data frame
  map_dfr(names(options_chain), function(expiry) {
    expiry_data <- options_chain[[expiry]]
    bind_rows(
      if (!is.null(expiry_data$calls)) mutate(expiry_data$calls, type = "call"),
      if (!is.null(expiry_data$puts)) mutate(expiry_data$puts, type = "put")
    ) %>%
      mutate(expiry_date = as.Date(expiry, format = "%b.%d.%Y"))
  }) %>%
    clean_names() %>%
    mutate(
      days_to_expiry = as.numeric(expiry_date - Sys.Date()),
      # Use the midpoint of bid/ask if the last price is zero
      price = ifelse(last == 0, (bid + ask) / 2, last)
    ) %>%
    # Filter for valid, liquid options
    filter(
      days_to_expiry > 0,
      vol >= min_volume,
      !is.na(price), price > 0,
      !is.na(bid), !is.na(ask), ask > bid
    ) %>%
    select(strike, days_to_expiry, type, price, vol) %>%
    arrange(type, days_to_expiry, strike)
}

```
 
## 3. Implied Volatility Functions

```{r}
### Black-Scholes pricing model
black_scholes <- function(S, K, T, r, sigma, type) {
  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (type == "put") {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("Option type must be 'call' or 'put'.")
  }
  return(price)
}
```

```{r}
### Calculate implied volatility
calculate_implied_vol <- function(market_price, S, K, T, r, type) {
  if (any(is.na(c(market_price, S, K, T, r, type))) || T <= 0) {
    return(NA_real_)
  }
  
  # Error function
  iv_func <- function(sigma) {
    (black_scholes(S, K, T, r, sigma, type) - market_price)
  }
  
  iv <- tryCatch(
    uniroot(iv_func, lower=0.001, upper=5)$root,
    error = function(e) NA_real_
  )
  return(iv)
}
```

## 4. Plotting Function

```{r}
create_surface_plot <- function(df, z_col, plot_title, plot_type = "scatter") {
  
  # Set the title for the z-axis based on the column
  z_title <- if (z_col == "price") "Option Price ($)" else "Implied Volatility"
  
  # Generate the base plot
  fig <- plot_ly(data = df, x = ~strike, y = ~days_to_expiry, z = as.formula(paste0("~", z_col)))
  
  if (plot_type == "surface") {
    # Interpolate data for a smooth surface plot
    interp_data <- interp(
      x = df$strike,
      y = df$days_to_expiry,
      z = df[[z_col]],
      xo = seq(min(df$strike), max(df$strike), length = 100),
      yo = seq(min(df$days_to_expiry), max(df$days_to_expiry), length = 100),
      linear = FALSE,
      extrap = FALSE
    )
    fig <- plot_ly(x = interp_data$x, y = interp_data$y, z = interp_data$z, type = "surface")
    
  } else {
    # Default to a scatter plot for prices
    fig <- fig %>%
      add_markers(
        color = as.formula(paste0("~", z_col)),
        marker = list(size = 4.0, opacity = 0.8)
      )
  }
  
  # Apply layout settings
  fig <- fig %>%
    layout(
      title = list(text = paste0("<b>", plot_title, "</b>"), y = 0.95),
      scene = list(
        xaxis = list(title = 'Strike Price ($)'),
        yaxis = list(title = 'Days to Expiry'),
        zaxis = list(title = z_title),
        camera = list(eye = list(x = 2.0, y = -2.0, z = 0.5))
      ),
      showlegend = FALSE
    )
  
  return(fig)
}
```

## 5. Prepare data

```{r}
# Fetch and process the options data
options_df <- get_tidy_options_data(symbol = TARGET_SYMBOL, min_volume = MIN_OPTION_VOLUME)

# Get the current price of the underlying asset
getSymbols(TARGET_SYMBOL, src = "yahoo", auto.assign = TRUE)
underlying_price <- as.numeric(tail(Cl(get(TARGET_SYMBOL)), 1))

# Calculate implied volatility for all options
options_df <- options_df %>%
  mutate(
    implied_vol = pmap_dbl(
      .l = list(price, strike, days_to_expiry / 365, type),
      .f = ~calculate_implied_vol(
        market_price = ..1,
        S = underlying_price,
        K = ..2,
        T = ..3,
        r = RISK_FREE_RATE,
        type = ..4
      )
    )
  ) %>%
  # Remove rows where IV calculation failed
  na.omit()

# Split into calls and puts
df_calls <- filter(options_df, type == "call")
df_puts <- filter(options_df, type == "put")
```

## 6. Generate and Save Plots

```{r}
# 1. Call Price (Scatter)
fig_call_price <- create_surface_plot(df_calls, "price", "SPY Call Option Price", "scatter")
htmlwidgets::saveWidget(fig_call_price, "SPY_call_price_scatter.html", selfcontained = TRUE)

# 2. Put Price (Scatter)
fig_put_price <- create_surface_plot(df_puts, "price", "SPY Put Option Price", "scatter")
htmlwidgets::saveWidget(fig_put_price, "SPY_put_price_scatter.html", selfcontained = TRUE)

# 3. Call Implied Volatility (Surface)
fig_call_iv <- create_surface_plot(df_calls, "implied_vol", "SPY Call Implied Volatility Surface", "surface")
htmlwidgets::saveWidget(fig_call_iv, "SPY_call_iv_surface.html", selfcontained = TRUE)

# 4. Put Implied Volatility (Surface)
fig_put_iv <- create_surface_plot(df_puts, "implied_vol", "SPY Put Implied Volatility Surface", "surface")
htmlwidgets::saveWidget(fig_put_iv, "SPY_put_iv_surface.html", selfcontained = TRUE)
```
