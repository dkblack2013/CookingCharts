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


#####Determining what I want my Gantt Charts to look like#####
timelineS(mj_life, main = "Life of Michael Jackson")
head(timelineS::mj_life)

#Step 1: Boil 6 cups water
#Step 2: Stir in pasta
#Step 3: Cook 9 minutes (average of 8 and 10) stirring occasionally.
#Step 4: Drain pasta. Do not rinse. Return to pan. 
#Step 5: Squeeze cheese sauce over hot pasta. Stir until blended.
#Step 6: Refrigerate leftovers.

instructions <- c("Boil 6 cups water\n", "Stir in pasta\n", 
                  "Cook, stirring occationally\n", "Drain pasta, return to pan\n",
                  "Squeeze cheese sauce over hot pasta,\n stir until blended\n", 
                  "Refrigerate leftovers\n")
Timing <- c(0, 5, 5.1, 14, 14.1, 20)
Completeness <- c(1,2,3,4,5,6)

macncheese <- data.frame(instructions, Timing, Completeness, stringsAsFactors=FALSE)

ggplot(aes(x=Timing), data=macncheese) + geom_bar(width=.25, fill="black") +
  theme(axis.text.y = element_blank(), axis.title.y=element_blank(), 
        axis.ticks.y = element_blank()) + 
  annotate("rect", xmin=0, xmax=5, ymin=0, ymax=1, alpha=.5, fill="blue") +
  annotate("rect", xmin=5, xmax=14, ymin=0, ymax=1, alpha=.5, fill="red") + 
  annotate("rect", xmin=14, xmax=20, ymin=0, ymax=1, alpha=.5, fill="green") + 
  #annotate("text", x=timing, y=.5, label=macncheese$instructions) +
  geom_text(aes(y=.5, label=instructions), size=3,angle=90, vjust=1.25) +
  ggtitle("Mac & Cheese Cooking Timeline")


tm1 <- as.POSIXct("2020-10-16 00:00:00", tz = "EST")
tm2 <- as.POSIXct("2020-10-16 00:05:00", tz = "EST")
tm3 <- as.POSIXct("2020-10-16 00:05:30", tz = "EST")
tm4 <- as.POSIXct("2020-10-16 00:14:00", tz = "EST")
tm5 <- as.POSIXct("2020-10-16 00:14:30", tz = "EST")
tm6 <- as.POSIXct("2020-10-16 00:20:00", tz = "EST")
Minutes <- c(tm1, tm2, tm3, tm4, tm5, tm6)

#doesn't work
# tm7 <- as.POSIXct("00:00:00")
# tm8 <- as.POSIXct("00:05:00")
# tm9 <- as.POSIXct("00:05:30")
# tm10 <- as.POSIXct("00:14:00")
# tm11 <- as.POSIXct("00:14:30")
# tm12 <- as.POSIXct("00:20:00")
# Minutes2 <- c(tm7, tm8, tm9, tm10, tm11, tm12)

macncheesetime <- data.frame(instructions, Minutes, stringsAsFactors=FALSE)
timelineS(macncheesetime, main="Mac & Cheese Cooking Timeline", label.direction = "up", 
          label.length = c(0.2,0.8,0.4,1.2), label.position = 3, 
          line.color = "red", label.color = "red", point.color = "red", pch = "-")



#timeline looks good, but it's missing labels underneath 
#and I would rather not include the dates

#instead, I'm going to try making a proportional stacked area graph
uspopage_prop <- ddply(uspopage, "Year", transform,
                       Percent=Thousands/sum(Thousands)*100)
ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) + 
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

#Line plot
ggplot(aes(x=Timing, y=Completeness), data=macncheese) + geom_point() +
  theme() + 
  annotate("rect", xmin=macncheese$Timing[1], xmax=macncheese$Timing[2], 
           ymin=0, ymax=1, alpha=.5, fill="blue") +
  annotate("rect", xmin=5, xmax=14, ymin=0, ymax=1, alpha=.5, fill="red") + 
  annotate("rect", xmin=14, xmax=20, ymin=0, ymax=1, alpha=.5, fill="green") + 
  #annotate("text", x=timing, y=.5, label=macncheese$instructions) +
  geom_text(aes(y=.5, label=instructions), size=3,angle=90, vjust=1.25) +
  ggtitle("Mac & Cheese Cooking Timeline")

#Proportional Stacked Area Graph
macncheese_prop <- ddply(macncheese, "Timing", transform,
                         Percent_Complete = Completeness/sum(Completeness)*100)

ggplot(aes(x=Timing, y=Percent_Complete), data=macncheese_prop) + 
  geom_line() +
  scale_x_continuous(breaks=c(0:20,1))+
  geom_area(colour="black", size=.2, alpha=.4) +
  annotate("rect", xmin=macncheese$Timing[1], xmax=macncheese$Timing[2], 
           ymin=0, ymax=100, alpha=.5, fill="blue") +
  annotate("rect", xmin=5, xmax=14, ymin=0, ymax=100, alpha=.5, fill="red") + 
  annotate("rect", xmin=14, xmax=20, ymin=0, ymax=100, alpha=.5, fill="green") + 
  #annotate("text", x=timing, y=.5, label=macncheese$instructions) +
  #geom_text(aes(y=50, label=instructions), size=3,angle=90, vjust=1.25) +
  ggtitle("Mac & Cheese Cooking Timeline") +
  geom_text_repel(aes(y=50, label=instructions), size=3, angle=90)


#Trying a new package
data(ww2)
timeline(ww2, ww2.events)
timeline(ww2, ww2.events, event.spots=2, event.label='', event.above=FALSE)
timelineShinyDemo()
#not what I wanted.

#struggling to get it to work for me
timeline(macncheesetime, macncheesetime$instructions, event.spots=2, event.labels='', event.above=FALSE)
        

#ok, so now I have a graph that does what I want it to.

#Create Data
instructions <- c("Boil 6 cups water\n", "Stir in pasta\n", 
                  "Cook, stirring occationally\n", "Drain pasta, return to pan\n",
                  "Squeeze cheese sauce over hot pasta,\n stir until blended\n", 
                  "Refrigerate leftovers\n")
Timing <- c(0, 5, 5.1, 14, 14.1, 20)
Completeness <- c(1,2,3,4,5,6)

macncheese <- data.frame(instructions, Timing, Completeness, stringsAsFactors=FALSE)

#Proportional Stacked Area Graph
macncheese_prop <- ddply(macncheese, "Timing", transform,
                         Percent_Complete = Completeness/sum(Completeness)*100)

ggplot(aes(x=Timing, y=Percent_Complete), data=macncheese_prop) + 
  #geom_area(colour="black", size=.2, alpha=.4) +
  geom_line()+
  scale_x_continuous(breaks=c(0:20,1))+
  geom_area(colour="black", size=.2, alpha=.4) +
  annotate("rect", xmin=macncheese$Timing[1], xmax=macncheese$Timing[2], 
           ymin=0, ymax=100, alpha=.5, fill="blue") +
  annotate("rect", xmin=5, xmax=14, ymin=0, ymax=100, alpha=.5, fill="red") + 
  annotate("rect", xmin=14, xmax=20, ymin=0, ymax=100, alpha=.5, fill="green") + 
  #annotate("text", x=timing, y=.5, label=macncheese$instructions) +
  #geom_text(aes(y=50, label=instructions), size=3,angle=90, vjust=1.25) +
  ggtitle("Mac & Cheese Cooking Timeline") +
  geom_text_repel(aes(y=50, label=instructions), size=3, angle=90)

#Eventually want to make it so that I have a model that 
#can read in new instructions and graph it automatically

#To get to that point, I need to:
#1. Write out the instructions and get them to parse into the data format needed
#2. Do that many times. Enough times to train a model.
#3. Probably assign mandatory food prep times, or otherwise figure out how to impute that data.


#let's try Gantt charts
library(timevis)

data <- data.frame(
  id      = 1:4,
  content = c("Item one"  , "Item two"  ,"Ranged item", "Item four"),
  start   = c("2016-01-10", "2016-01-11", "2016-01-20", "2016-02-14 15:00:00"),
  end     = c(NA          ,           NA, "2016-02-04", NA)
)

timevis(data)

#A different solution
library(DiagrammeR)

mermaid("
        gantt
        dateFormat  YYYY-MM-DD
        title A Very Nice Gantt Diagram
        
        section Basic Tasks
        This is completed             :done,          first_1,    2014-01-06, 2014-01-08
        This is active                :active,        first_2,    2014-01-09, 3d
        Do this later                 :               first_3,    after first_2, 5d
        Do this after that            :               first_4,    after first_3, 5d
        
        section Important Things
        Completed, critical task      :crit, done,    import_1,   2014-01-06,24h
        Also done, also critical      :crit, done,    import_2,   after import_1, 2d
        Doing this important task now :crit, active,  import_3,   after import_2, 3d
        Next critical task            :crit,          import_4,   after import_3, 5d
        
        section The Extras
        First extras                  :active,        extras_1,   after import_4,  3d
        Second helping                :               extras_2,   after extras_1, 20h
        More of the extras            :               extras_3,   after extras_1, 48h
        ")

#But my data is a data frame, so
df <- data.frame(task = c("task1", "task2", "task3"),
                 status = c("done", "active", "crit"),
                 pos = c("first_1", "first_2", "first_3"),
                 start = c("2014-01-06", "2014-01-09", "after first_2"),
                 end = c("2014-01-08", "3d", "5d"))

#   task status     pos         start        end
#1 task1   done first_1    2014-01-06 2014-01-08
#2 task2 active first_2    2014-01-09         3d
#3 task3   crit first_3 after first_2         5d



mermaid(
  paste0(
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  YYYY-MM-DD", "\n", 
    "title Making Dinner", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
    paste(df %>%
            unite(i, task, status, sep = ":") %>%
            unite(j, i, pos, start, end, sep = ",") %>%
            .$j, 
          collapse = "\n"
    ), "\n"
  )
)

#trying it with my data
df <- data.frame(task = c("Boil 6 cups water", "Stir in pasta", 
                                  "Cook, stirring occationally", "Drain pasta, return to pan",
                                  "Squeeze cheese sauce over hot pasta, stir until blended", 
                                  "Refrigerate leftovers"),
                 status = c("active", "active", "active", "active", "active", "active"),
                 pos = c("first_1", "first_2", "first_3", "first_4", "first_5", "first_6"),
                 start = c("2014-01-06","after first_1", "after first_2", "after first_3", "after first_4", "after first_5"),
                 end = c("2014-01-08", "3d", "5d", "1d", "1d", "1d"))
#trying to get the x axis to where I want it using the time values I created above
df <- data.frame(task = c("Boil 6 cups water - 5 min", "Stir in pasta - 1 min", 
                          "Cook, stirring occationally - 8 min", 
                          "Drain pasta, return to pan - 1 min",
                          "Squeeze cheese sauce over hot pasta, stir until blended - 5 min", 
                          "Refrigerate leftovers - 1 min"),
                 status = c("active", "active", "active", "active", "active", "active"),
                 pos = c("first_1", "first_2", "first_3", "first_4", "first_5", "first_6"),
                 start = c("tm1","after first_1", "after first_2", "after first_3", "after first_4", "after first_5"),
                 end = c("5m", "1m", "8m", "1m", "5m", "1m"))
Timing <- c(0, 5, 5.1, 14, 14.1, 20)
tm1 <- as.POSIXct("2020-10-25 00:00:00")
tm2 <- as.POSIXct("2020-10-16 00:05:00", tz = "EST")
tm3 <- as.POSIXct("2020-10-16 00:05:30", tz = "EST")
tm4 <- as.POSIXct("2020-10-16 00:14:00", tz = "EST")
tm5 <- as.POSIXct("2020-10-16 00:14:30", tz = "EST")
tm6 <- as.POSIXct("2020-10-16 00:20:00", tz = "EST")





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
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Mac & Cheese", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
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
    # mermaid "header", each component separated with "\n" (line break)
    "gantt", "\n", 
    "dateFormat  HH:MM:SS", "\n", 
    "title Making Dinner", "\n\n",
    "section Mac & Cheese", "\n",
    # unite the first two columns (task & status) and separate them with ":"
    # then, unite the other columns and separate them with ","
    # this will create the required mermaid "body"
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
    # mermaid "header", each component separated with "\n" (line break)
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
#1. Figure out how to make my data into a Shiny App
#2. Figure out how to pull data out of this online recipe
#3. If Step 2 fails, just pull it by hand and start building a catalog.


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
  
#recipe1_collapsed <- paste(readLines("recipe1.txt"), collapse = " ")

#str_split(recipe1_collapsed, " ")
#substr(recipe1_collapsed, start=3, stop=6)

#determining the positions within
pos1 <- grep("Ingredients", recipe1)
pos2 <- grep("Instructions", recipe1)
pos3 <- grep("Notes", recipe1)
pos4 <- grep("Author", recipe1)
pos5 <- grep("Prep Time:", recipe1)
pos6 <- grep("Cook Time:", recipe1)
pos7 <- grep("Total Time:", recipe1)

#this prints the ingredients and instructions
#print(recipe1[1729:1762])

#finding the exact data I want from the url
recipe1_ingredients <- (recipe1[(pos1[2]+2):(pos2-4)])
recipe1_instructions <- (recipe1[(pos2+2):(pos3-3)])
recipe1_times <- c(1,1,1,1,1,1,1)
recipe1_names <- c("Instructions", "Time")
recipe1_data <- data.frame(recipe1_instructions, recipe1_times)
colnames(recipe1_data)=recipe1_names

#We have to clean this data and make sure it's conducive for a Gantt Chart.
#Need to split up the second and possibly other instructions.
#https://statisticsglobe.com/find-position-of-character-in-string-in-r
grep("Preheat", recipe1_data[1])
substr(recipe1_data$Instructions[2], start=1, stop=gregexpr("\\. ",recipe1_data$Instructions[2])[[1]][1])
str_locate_all(pattern = "\\. ", recipe1_data$Instructions[2])
which(strsplit(recipe1_data$Instructions[2], "x")[[1]] == ".")
strsplit(recipe1_data$Instructions[2],"\\. ")

#Rob helped provide an example so I could figure out how to split strings by sentences.
#I was missing the \\ stop character. 
s = "Hey David. This is Rob. I think this R function will be helpful for you. 
If you want to allow decimals like 1.0 without splitting, 
you can use period+space as your search string."
strsplit(s,"\\.")
strsplit(s,"\\. ")
strsplit(s,". ",fixed=TRUE) #match literal string, not regex (less flexible but easier)
##

#Trying to clean up recipe 1
strsplit(recipe1_data$Instructions[2],"\\. ")
recipe1_data <- data.frame(recipe1_instructions, recipe1_times)

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


for (i in 1:(recipe1_data$Instructions)) {
  Instructions2 <- strsplit(recipe1_data$Instructions[i],"\\. ")
  # Instructions2[i] <- i
}

Instructions2 <- c()
for (i in 1:length(list_of_sentence_count)){
  Instructions2 <- append(Instructions2, strsplit(recipe1_data$Instructions[i],"\\. "))
}

Instructions3 <- c()
for (i in 1:length(Instructions2)){
  #print(Instructions2[[i]])
  Instructions3 <- append(Instructions3, Instructions2[[i]])
}
#We've arrived at a vector of all the instructions broken down into single sentences!
#Now we can either attempt to impute time values for each or create them by hand.


###

#need to impute the time values based on the words in the instructions
#this is an idea, but it needs a lot of work
# imputeTimes <- function(x) {
#   df %>% 
#     select(x) %>% 
#     summarize(missing = sum(is.na(.))) %>%
#     mutate(var =x)
#   
# }
# 
# collectedPercent <- map_dfr(colnames(df), getPercent) %>% 
#   mutate(n=nrow(df)) %>% 
#   mutate(percentage = (missing/n)*100) %>% 
#   select(var, percentage)

#####Imputing Cooking Information#####
#I think I will have to give the function basic cooking time values, such as oven pre-heating time
#The time will vary depending on the oven; however, 
#the average time is 15 minutes to preheat an oven to 350° F (180° C). 
#Preheating to 450° F (230° C) will require an additional 5 minutes



#####Ignore for Now#####
##copied from another NLP project
opinions <- lapply(files[1:3], article)
opinions2 <- lapply(files[5], article)


# install.packages("tm")
# install.packages("SnowballC")
library(tm)
library(SnowballC)
corp <- Corpus(URISource(files[1:3]),
               readerControl = list(reader = readPDF))

opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf)))) 

inspect(opinions.tdm[1:10,]) 

corp <- tm_map(corp, removePunctuation, ucp = TRUE)

opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf))))

inspect(opinions.tdm[1:10,]) 

findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)

ft <- findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)
as.matrix(opinions.tdm[ft,]) 

ft.tdm <- as.matrix(opinions.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)

#applying the lesson to gentlemen bastards series
corp_gb <- Corpus(URISource(files[5]),
                  readerControl = list(reader = readPDF))

lamora.tdm <- TermDocumentMatrix(corp_gb, 
                                 control = 
                                   list(removePunctuation = TRUE,
                                        stopwords = TRUE,
                                        tolower = TRUE,
                                        stemming = FALSE,
                                        removeNumbers = TRUE)) 

inspect(lamora.tdm)

findFreqTerms(lamora.tdm, lowfreq = 100, highfreq = Inf)

acqTag <- tagPOS(corp_gb[["the-gentleman-bastard-series-3-book-bundle-scott-lynch.pdf"]][["content"]])

#install.packages("openNLP")
library("openNLP")

acqTag <- tagPOS(opinions2, language = "en") 
acqdf <- read.table(textConnection(gsub(" ", "\n", acqTag)), sep="/", stringsAsFactors=FALSE)
acqdf$nnadj <- grepl("NN|JJ", acqdf$V2)
acqdf$nnadj 

acqdf$nnadj[1:(nrow(acqdf)-1)] & acqdf$nnadj[2:nrow(acqdf)]

acqdf$pair <- c(NA, acqdf$nnadj[1:(nrow(acqdf)-1)] & acqdf$nnadj[2:nrow(acqdf)])
acqdf[1:7, ]
