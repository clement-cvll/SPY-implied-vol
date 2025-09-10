# SPY Implied Volatility Analysis

This project fetches and analyzes options data for the SPDR S&P 500 ETF (SPY) to visualize implied volatility surfaces and option prices.

## Description

The goal of this project is to provide a visual representation of the implied volatility "smile" or "skew" for SPY options. It generates interactive 3D surface plots for implied volatility across different strike prices and expirations, as well as scatter plots showing the relationship between option prices, strikes, and time to maturity.

The analysis is done separately for call and put options. The resulting interactive plots are saved as HTML files in the `src/` directory.

## Visualizations

This project generates the following interactive plots:

*   **Implied Volatility Surface (3D):** A 3D surface plot showing implied volatility as a function of strike price and days to expiration. This helps in visualizing the volatility smile and term structure.
*   **Option Price Scatter (3D):** A 3D scatter plot showing the last price of options as a function of strike price and days to expiration.

*(Suggestion: You can add screenshots of your generated plots here to give a preview.)*

**Example:**
`![IV Surface](path/to/your/screenshot.png)`

## How to Use

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/your-username/SPY-implied-vol.git
    cd SPY-implied-vol
    ```

2.  **Open the project in RStudio:**
    Open the `SPY-implied-vol.Rproj` file.

3.  **Install dependencies:**
    You will need to have R installed. Run the following command in the R console to install the necessary packages (you may need to add or remove packages depending on your script).
    ```r
    # Likely packages used, please verify from your R script
    install.packages(c("plotly", "httr", "jsonlite", "RQuantLib"))
    ```

4.  **Run the analysis:**
    Source the main R script (e.g., `main.R`) that generates the data and plots.

5.  **View the output:**
    The generated interactive HTML plots can be found in the `src/` directory. Open them in any web browser.
    *   [`src/SPY_call_iv_surface.html`](src/SPY_call_iv_surface.html)
    *   [`src/SPY_put_iv_surface.html`](src/SPY_put_iv_surface.html)
    *   [`src/SPY_call_price_scatter.html`](src/SPY_call_price_scatter.html)
    *   [`src/SPY_put_price_scatter.html`](src/SPY_put_price_scatter.html)

## License

This project is licensed under the Apache License 2.0. See the [LICENSE](LICENSE) file for details.