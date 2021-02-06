
# packages needed
library(pacman)
p_load(dplyr, stringr, magrittr, readxl)

# loading datafile for verbs
dd<-read_excel("verbs.xlsx") %>% data.frame() %>% na.omit()
colnames(dd)<-c("word","singular","present_paticiple","simple_past","past_participle")
View(dd)


for (i in 1:ncol(dd))
{ # removing hyphens which meant NA, as well as NA
  dd[,i]<-ifelse(dd[,i]=="-", "", dd[,i])
}

# the answer function
#######################################################################
answer<-function(q, word_count)
{
  # splitting the story to sentences
  fun_storysentence<-str_split(bulkstory, "[.]") 
  
  # splitting the question to words
  splitquiz<-str_split(q, " ") %>% unlist

  # a dataframe to hold the story sentences
  storytibble<-data.frame()

  for (i in 1:length(fun_storysentence[[1]]))
  {
    storytibble[i, 1]<-fun_storysentence[[1]][i]
    
    # counting no of chars in the string
    storytibble[i, 2]<-str_count(storytibble[i, 1], " ")
  }
  colnames(storytibble)<-c("storylines", "length")

  for (i in 1:length(fun_storysentence[[1]]))
  {
    logic_answer<-str_detect(storytibble[i,1], splitquiz)
    truthcount<-sum(str_count(logic_answer, "^T"))
    if (truthcount>=word_count)
    {
      return(paste(q ,":", storytibble[i,1]))
    }
    else{}
  }
}

###########################################################


qanalyze<-function(q)
{ ## this function analyzes a question
  interrogates<-c("how", "when", "why", "which", "what", "where", "whom", 
                  "who", "whose", "")
  helping_verbs<-c()

  # what - asking specific info from a general range of possible answers
  # which - asking sepcific info from a restricted range of possible answers

  # splitting the question to words
  splitq<-str_split(q, " ") %>% unlist()

  # locating the verb in the question
  verb_locator<-function(q, df)
  {
    v<-vector() # vector for holding verbs
    s<-str_split(q, " ") %>% unlist()
    for (i in seq_along(s))
    {
      for (j in 1:nrow(df))
      {
        for (k in 1:ncol(df))
        {
          if (s[i]==df[j, k])
          {
            #print(df[j, ] %>% unname())
            v[i]<-s[i]
          }
          else{}
        }
      }
    }
    return(v) %>% na.exclude() %>% as.character()
  }

  # locating the interrogate.
  for (i in seq_along(interrogates))
  {
    for (j in seq_along(splitq))
    {
      if (splitq[j]==interrogates[i])
      {
        message(paste("The interrogative word is: ",splitq[j]))
      }
    }
  }

  message("The verbs are: ")
  v<-verb_locator(q, df=dd)
  return(v)
}

qanalyze(q1)
answer(q1, 4)

# a sample story together with questions to be answered by model
bulkstory<-"Wacu is a very successful businesswoman. She started her business in a small stall in Lando, her home town. In the stallshe sold second hand clothes, watches and small radios. With the profit she made, and with a lot of determination, Wacu expanded her business and started a clothes and electronics shop. Her ever increasing number of customers wanted a wide variety of goods. It was difficult to satisfy their demands for she had started supplying goods to other traders. One day Wacu heard that an international trade exhibition - organized by the ministry of trade and industry - was going to be held in Nairobi at the Kenyatta International Conference centre. She travelled to the city to attend the exhibition. Exhibitiors from Europe, Asia and the rest of Africa had brought many kinds of goods. At each stand, Wacu was given a lot of information that she later used to expand her business. She learnt how she could order goods from other countries using the fax or email, make payments using modern banking technology and have them delivered within a short time. Afterwards, Wacu applied for a big loanfrom a bank, which she used together with the profits she had made to expand her business. In six months, Wacu opened a big supermarket in Lando where she sold all kinds of goods. Her goods came from other countries as well as her own country."
q1<-"what made it possible for wacu to expand her business" # word_count 4
q2<-"why do you think it is the ministry of trade and exhibition that organized the international trade exhibition" # word_count ?
q3<-"How did wacu order goods from other countries" # word_count 3
q4<-"wacu started a big business by using" # word_count ?
q5<-"Wacu's business progressed from"
q6<-"The story of wacu shows that"

# common interrogative words
interrogates<-c("how", "when", "why", "which", "what", "where", "is", "can",
"what made it possible", "what makes it possible", "will")
