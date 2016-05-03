# To compile and make a single jar with all dependencies run
mvn clean compile assembly:single

# The jar with all dependencies get created under the target folder. The program can be run as
nohup java -jar extractors-1.0-jar-with-dependencies.jar &

# Also delink the background process from the current active terminal by running.
disown -h %1
