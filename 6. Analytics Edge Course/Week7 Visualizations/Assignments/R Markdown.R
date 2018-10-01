---
title: "Untitled"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.



WHO = read.csv("WHO.csv")
str(WHO)

#plotting
plot(WHO$GNI, WHO$FertilityRate)


#Plotting the same variables using ggplot2. 
library(ggplot2)

#ggplot has 3 arguements. We mentioned 1st two in the Scatterplot variable. 3rd is added in next
Scatterplot = ggplot(WHO, aes(x=GNI, y=FertilityRate))
