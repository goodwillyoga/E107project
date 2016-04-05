package com.cscie107;

import yahoofinance.Stock;
import yahoofinance.YahooFinance;
import yahoofinance.quotes.stock.StockQuote;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

/**
 * Created by pulkit on 4/5/16.
 */
public class StockPriceExtractor {

    public Map<String, Stock> extractPrices(String[] symbols){
        try {
            Map<String, Stock> stocks = YahooFinance.get(symbols);
            return stocks;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
