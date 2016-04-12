# To compile and make a single jar with all dependencies run
mvn clean compile assembly:single

# Always run it specifying the timezone to EST as the stocks are from NYSE
-Duser.timezone=America/New_York

# There are 4 parameters needed to run the program (String consumerKey, String consumerSecret, String token, String secret)
# Provide them on the command line as 
nohup java -jar -Duser.timezone=America/New_York extractors-1.0-jar-with-dependencies.jar consumerKey consumerSecret token secret &

