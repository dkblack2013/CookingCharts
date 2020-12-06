#install.packages("timelineS")
#library(timelineS)
library(ggplot2)
#library(gcookbook)
library(plyr)
#library(timeline)
library(shiny)
library(ggrepel)
library(tidyr)
library(dplyr)
library(xml2)
library(stringr)
library(httr)
library(rvest) # will be used to parse HTML
library(tidyverse)
library(tokenizers)
#install.packages("openNLP")
# install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
library(SnowballC)
library(NLP)
library(tm)  # make sure to load this prior to openNLP
library(openNLP)
library(openNLPmodels.en)
library(readxl)
library(DiagrammeR)



#What I've arrived at as the final working code:
df <- data.frame(task = c("Boil 6 cups water - 5 min", "Stir in pasta - 1 min", 
                          "Cook, stirring occationally - 8 min", 
                          "Drain pasta, return to pan - 1 min",
                          "Squeeze cheese sauce over hot pasta, stir until blended - 5 min", 
                          "Refrigerate leftovers - 1 min"),
                 status = c("active", "active", "active", "active", "active", "active"),
                 pos = c("first_1", "first_2", "first_3", "first_4", "first_5", "first_6"),
                 start = c("tm1","after first_1", "after first_2", "after first_3", "after first_4", "after first_5"),
                 end = c("5m", "1m", "8m", "1m", "5m", "1m"))
df2 <- data.frame(task = c("Remove frozen green beans from freezer - 4 min", "Microwave for 7 minutes - 7 min", 
                           "Let sit in Microwave for 2 minutes - 2 min", 
                           "Remove from Microwave and enjoy - 1 min"),
                  status = c("active", "active", "active", "active"),
                  pos = c("first_1_side", "first_2_side", "first_3_side", "first_4_side"),
                  start = c("after first_1","after first_1_side", "after first_2_side", "after first_3_side"),
                  end = c("4m", "7m", "2m", "1m"))

#If I could make it so that the total time of each got added up, but the shorter one started 
#at a point where they would both finish at the same time, that would be ideal
#I was thinking if I could add up the "end" vectors and subtract the shorter one from the 
#longer one, then put the difference as the start position of the shorter one

MacwSide <- mermaid(
  paste0(
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Mac & Cheese", "\n",
    paste(df %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n\n",
    "section Side", "\n",
    paste(df2 %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)

MacnCheese <- mermaid(
  paste0(
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Mac & Cheese", "\n",
    paste(df %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n\n"
  )
)

GreenBeans <- mermaid(
  paste0(
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Green Beans", "\n",
    paste(df2 %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)

#Next things to do:
#1. Figure out how to make my data into a Shiny App [COMPLETE]
#2. Figure out how to pull data out of this online recipe [MOSTLY COMPLETE]

#####Pulling a recipe from the internet#####
#2. Figure out how to pull data out of this online recipe

recipe <- GET(url = "www.acouplecooks.com/vegan-fajitas")
#print(content(recipe))

#getting the article
article <- html_nodes(content(recipe), "article")
#print(html_text(article)[[1]])


#Setting my working directory
setwd("C:/Users/dkbla/Documents/R/Cooking_Timelines")

#Taking the html and converting it to UTF-8
html_text(article)[[1]] %>% 
  iconv(to="UTF-8") %>% 
  writeLines("C:/Users/dkbla/Documents/R/Cooking_Timelines/recipe1.txt")

#reading the recipe from the converted text file (might be unnecessary)
recipe1 <- readLines("recipe1.txt")


#determining the positions within
pos1 <- grep("Ingredients", recipe1)
pos2 <- grep("Instructions", recipe1)
pos3 <- grep("Notes", recipe1)
pos4 <- grep("Author", recipe1)
pos5 <- grep("Prep Time:", recipe1)
pos6 <- grep("Cook Time:", recipe1)
pos7 <- grep("Total Time:", recipe1)


#finding the exact data I want from the url
recipe1_ingredients <- (recipe1[(pos1[2]+2):(pos2-4)])
recipe1_instructions <- (recipe1[(pos2+2):(pos3-3)])
recipe1_times <- c(1,1,1,1,1,1,1)
recipe1_names <- c("Instructions", "Time")
recipe1_data <- data.frame(recipe1_instructions, recipe1_times)
colnames(recipe1_data)=recipe1_names


#Trying to clean up recipe 1
list_of_sentence_count<- c()
for (i in 1:count(as.data.frame(recipe1_data$Instructions))[[1]]) {
  list_of_sentence_count[i] <- count(as.data.frame(strsplit(recipe1_data$Instructions[i],"\\. ")))
}
#Now list_of_sentence_count[[i]] will tell me how many sentences each imputed instruction
#contains. I can use this to break each sentence down into individual data rows.

sumSentence = 0
for (i in 1:length(list_of_sentence_count)){
  print(list_of_sentence_count[[i]])
  sumSentence = sumSentence + list_of_sentence_count[[i]]
}
#Now we have a way of counting how many sentences are in each step of the instructions.
#For our purposes, I think having each sentence be it's own "instruction" or "step" 
#will be most effective.

Instructions2 <- c()
for (i in 1:length(list_of_sentence_count)){
  Instructions2 <- append(Instructions2, strsplit(recipe1_data$Instructions[i],"\\. "))
}

Instructions3 <- c()
for (i in 1:length(Instructions2)){
  #print(Instructions2[[i]])
  Instructions3 <- append(Instructions3, Instructions2[[i]])
}

Instructions3 %>% writeLines("C:/Users/dkbla/Documents/R/Cooking_Timelines/Instructions3.txt")
#We've arrived at a vector of all the instructions broken down into single sentences!
#Now we can either attempt to impute time values for each or create them by hand.

#need to impute the time values based on the words in the instructions

#####Imputing Cooking Information#####
#I think I will have to give the function basic cooking time values, such as oven pre-heating time
#The time will vary depending on the oven; however, 
#the average time is 15 minutes to preheat an oven to 350° F (180° C). 
#Preheating to 450° F (230° C) will require an additional 5 minutes

#Thinking it through
#write a function or for loop
#for every sentence (Instructions3[i]), split it up into a corpus, then a TermDocumentMatrix
#look for particular words that would indicate each step using an if()
#for example: if the word "preheat" is in a sentence, look for a number and then 
#put it into an equation used to determine how long the oven will take to preheat


words <- tokenize_words(Instructions3[1])
for (i in 1:length(words[[1]])){
  if(words[[1]][i] == "preheat"){
    print("Yes")
  }
  else{print("No")}
}

#trying to test regexpressions to see if I could identify the temperature based on the verb "preheat"
#gregexpr("1",words[[1]])[[1]][[1]]

#trying to make a corpus and tdm so that I can tag parts of speech and pull out the verbs
#to match against my chart where I've pre-assigned time values for different verbs
#the hope is that I will be able to pull out the verbs, assign them with a time,
#and match them back to each sentence with an assigned time, that will then become a 
#data frame, and from there we can input that data into a gantt chart and PRESTO.

#On second thought, if I already know which verbs I am going to impute time for, 
#then I can just tokenize and match it up against an imported dataset I've created on the side.
instructions_string = unlist(Instructions3) %>% 
  paste(collapse=' ') %>% 
  as.String



extractPOS <- function(x, thisPOSregex) {
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  thisPOSindex <- grep(thisPOSregex, tags)
  tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
  untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
  untokenizedAndTagged
}

#This code will allow me to see all the verbs:
v <- lapply(instructions_string, extractPOS, "V")

v <- strsplit(v[[1]]," ",fixed=TRUE)
#v <- as.character(v)
v <- strsplit(v[[1]],"/",fixed=TRUE)

#not sure why I'm trying to identify if there is a verb in here, I already know there is.
for (i in 1:length(v)){
  if("VB" %in% v[i]){
    print("Yes")
  }
  else{print("No")}
}

#Reading in my excel file with time values for each verb
hard_times <- read_excel("Cooking_Tasks_Timing.xlsx")

#Need to separate the POS from the actual verbs now
verbs <- sapply(v, function(x) x[1])

#so this iterative method might not work, but we can see that the two lists can validate against each other
#What do I want it to do?
#I want to match the verbs between the list I uploaded and the list from the instructions.
#Need to have it iterate against the entire list each time

#trying to figure out how to iterate the second list so that I can compare the two precisely.
#I'm sure there's a function or package for this.

#This actually returns a list of all the verbs that intersect!
verbs <- intersect(unlist(hard_times[,1]), unlist(verbs))

#This returns all the times for those associated verbs
times <- c()
for(i in 1:length(verbs)){
  times[i] = hard_times %>% 
    select(Time) %>% 
    filter(hard_times$Activity == verbs[i])
}

#Now to put it into the Gantt Chart format
end_times <- c()
for (i in 1:length(as.character(times))){
  end_times[i] <- paste0(as.character(times)[i],"m")
}
#Problem: These end times only have 12, but there needs to be 17 to match with the sentences from Instructions3.

df3 <- data.frame(Instructions3,
                  #                 task = c("Preheat the oven to 425 degrees Fahrenheit."                                                                                                                              
                  #                            ,"Thinly slice the onion"                                                                                                                                                   
                  #                            ,"Slice the bell peppers"                                                                                                                                                   
                  #                            ,"Chop the cauliflower into small florets"                                                                                                                                  
                  #                            ,"Chop the mushroom into bite-sized pieces"                                                                                                                                 
                  #                            ,"Add the veggies to a big bowl and toss them with the olive oil, chili powder, cumin, paprika, garlic powder, onion powder, and kosher salt."                              
                  #                            ,"Line 2 baking sheets with parchment paper"                                                                                                                                
                  #                            ,"Add the vegetables in a single layer"                                                                                                                                     
                  #                            ,"Roast 15 minutes, then remove the sheets, stir the veggies, and sprinkle on another 1/2 teaspoon salt spread between the trays (1/4 teaspoon on each)"                    
                  #                            ,"Stir again, then return to the oven and roast another 10 minutes until tender."                                                                                           
                  #                            ,"Meanwhile, pit the avocados"                                                                                                                                              
                  #                            ,"Scoop out the flesh into a bowl and mash with a fork"                                                                                                                     
                  #                            ,"Add the lime juice, salt, and cilantro."                                                                                                                                  
                  #                            ,"Heat the refried beans in a small sauce pan."                                                                                                                             
                  #                            , "If desired, char the tortillas by placing them on an open gas flame on medium for a few seconds per side, flipping with tongs, until they are slightly blackened and warm"
                  #                            , "(See How to Warm Tortillas.)"                                                                                                                                             
                  #                            ,"To serve, place the refried beans and roasted veggies in tortillas, and top with guac-ish."),
                  status = c("active", "active", "active", "active", "active"
                             , "active", "active", "active", "active", "active"
                             , "active", "active", "active", "active", "active"
                             , "active", "active"),
                  pos = c("first_1_side", "first_2_side", "first_3_side", "first_4_side",
                          "first_5_side", "first_6_side", "first_7_side", "first_8_side",
                          "first_9_side", "first_10_side", "first_11_side", "first_12_side",
                          "first_13_side", "first_14_side", "first_15_side", "first_16_side",
                          "first_17_side"),
                  start = c("after first_1","after first_1_side", "after first_2_side", "after first_3_side"
                            ,"after first_4_side", "after first_5_side", "after first_6_side"
                            ,"after first_7_side", "after first_8_side", "after first_9_side"
                            ,"after first_10_side", "after first_11_side", "after first_12_side"
                            ,"after first_13_side", "after first_14_side", "after first_15_side"
                            ,"after first_16_side"),
                  end = c("2m","2m","1m","1m","1m","1m","2m","1m","1m","2m","1m","1m","1m","1m","1m","1m","1m"))

Fajitas <- mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Fajitas", "\n",
    paste(df3 %>%
            unite(i, Instructions3, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)
