# Clustering Words

This project presents an algorithm to automatically compute the number of clusters and shifts in a subject's list of words. The corresponding code is experimental and was applied to three datasets. 

The code performs 3 main steps, data cleaning, similarity, and clustering.

To run the algorithm with the provided example, download all codes, install the corresponding libraries, and run the code without any change. Otherwise, to apply the model to your own data, you have to change the paths of files for clustering the data, following the specifications of the CSV files (see below for more details).

## Requirements

This project runs under Rstudio 4.2.2. You can install all the libraries using install.packages("namePackage"). The original code uses the library ggplot2 and reshape2

## Uses

### Suggestion 
The R file [clusteringWord.R](/src/clusteringWord.R) has the main code and the corresponding instructions. To use this clustering model the dataset needs to be in a csv file with the two following columns: subject (the number/name of the subject, e.g. 1), and word (the raw word listed by a subject, e.g. “shark”, (please keep the order of the columns, but the labels of the head columns are not important). 

The beginning of the code contains the main variables for the model, which may be changed according to your needs 
- nameData : The path and name of the file that contains the dataset.
- simThreshold : The similarity threshold used for the clusterization process


