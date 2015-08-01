#!/bin/bash
#to be run from the src directory
javac org/json/*.java
scalac org/json/*.java Matchmaker/*.scala 
cp Matchmaker/*.class .
cp org/json/*.class .

echo Type in "scala Matchmaker.Matchmaker" from the src directory to run the program. You will need the files listings.txt and products.txt in the src directory as well. The output file will be named results.txt.
