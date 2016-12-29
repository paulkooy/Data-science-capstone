library(scales)
library(dplyr)
library(knitr)
library(tm)
library(quanteda)
library(lattice)
sampleSize <- 0.0001
#   Read files to create samples
if (file.exists("../data/sample/sample1.txt")) {
    sample1 <- readLines(con = "../data/sample/sample1.txt", skipNul = TRUE)
} else {
    sampleTwitr <- readLines(con = "../data/final/en_US/en_US.twitter.txt", skipNul = TRUE)
    sampleNews <- readLines(con = "../data/final/en_US/en_US.news.txt", skipNul = TRUE)
    sampleBlogs <- readLines(con = "../data/final/en_US/en_US.blogs.txt", skipNul = TRUE)
    sample <- rbind(sampleTwitr, sampleNews, sampleBlogs, stringsAsFactors = FALSE)
    set.seed(111)
    sample1 <- sample[rbinom(length(sample)*sampleSize, length(sample), .5)]
    write(sample1, file = "../data/sample/sample1.txt")
    set.seed(222)
    sample2 <- sample[rbinom(length(sample)*sampleSize, length(sample), .5)]
    write(sample2, file = "../data/sample/sample2.txt")
    set.seed(333)
    sample3 <- sample[rbinom(length(sample)*sampleSize, length(sample), .5)]
    write(sample3, file = "../data/sample/sample3.txt")
    set.seed(444)
    sample4 <- sample[rbinom(length(sample)*sampleSize, length(sample), .5)]
    write(sample4, file = "../data/sample/sample4.txt")
    set.seed(555)
    sample5 <- sample[rbinom(length(sample)*sampleSize, length(sample), .5)]
    write(sample5, file = "../data/sample/sample5.txt")
}
