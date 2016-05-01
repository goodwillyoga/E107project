package com.cscie107;

import com.google.common.collect.Lists;
import com.twitter.hbc.ClientBuilder;
import com.twitter.hbc.core.Client;
import com.twitter.hbc.core.Constants;
import com.twitter.hbc.core.Hosts;
import com.twitter.hbc.core.HttpHosts;
import com.twitter.hbc.core.endpoint.StatusesFilterEndpoint;
import com.twitter.hbc.core.processor.StringDelimitedProcessor;
import com.twitter.hbc.httpclient.auth.Authentication;
import com.twitter.hbc.httpclient.auth.OAuth1;

import java.util.List;

/**
 * Created by pulkit on 4/5/16.
 */
public class StockTweetExtractor extends BaseExtractor{
    private final String consumerKey;
    private final String consumerSecret;
    private final String token;
    private final String secret;
    private Client  hosebirdClient;
    public StockTweetExtractor(String consumerKey, String consumerSecret, String token, String secret, String[] monitoredStocks) {
        super(convertToStockTicks(monitoredStocks));
        this.consumerKey = consumerKey;
        this.consumerSecret = consumerSecret;
        this.token = token;
        this.secret = secret;
    }

    public void setup() {
        /** Declare the host you want to connect to, the endpoint, and authentication (basic auth or oauth) */
        Hosts hosebirdHosts = new HttpHosts(Constants.STREAM_HOST);
        StatusesFilterEndpoint hosebirdEndpoint = new StatusesFilterEndpoint();
// Optional: set up some followings and track terms
        List<String> terms = Lists.newArrayList(getMonitoredStocks());
        hosebirdEndpoint.trackTerms(terms);

// These secrets should be read from a config file
        Authentication hosebirdAuth = new OAuth1(consumerKey, consumerSecret, token, secret);
        // Create a new BasicClient. By default gzip is enabled.
        ClientBuilder builder = new ClientBuilder()
                .name("Hosebird-Client-01")                              // optional: mainly for the logs
                .hosts(hosebirdHosts)
                .authentication(hosebirdAuth)
                .endpoint(hosebirdEndpoint)
                .processor(new StringDelimitedProcessor(getMessageQueue()));

        hosebirdClient = builder.build();
// Attempts to establish a connection.
        hosebirdClient.connect();
    }

    private static String[] convertToStockTicks(String[] stocks) {
        String returnArray[] = new String[stocks.length];
        for(int i=0;i<stocks.length;i++){
            returnArray[i] = "$"+stocks[i];
        }
        return returnArray;
    }

    public void stop(){
        hosebirdClient.stop();
    }
}
