## UNDERSDADING COMMUNITY
library("igraph")

train_label = paste(HOME,"users/coded_ids_labels_train.csv", sep="")

labels  <- read.csv(train_label, header=T, as.is=T)
labels  <- data.frame(labels)

#1.Community:
## All members of this community are Spam
# $`1`
c1 <- c(1,18,25,34,48,60,91,122, 149,168,170,183,185,196,198,229,232,233,236,237,238,239,244,251,260)
outputs = labels[labels$coded_id %in%  c1, ]
print(outputs)

#Some instances might not be in the train set
#But they might still be in the graph data and in the test data to be predicted
print(length(outputs$coded_id)  - length(c1)) #1

 

# $`2`
## All members of this community are Spam
# [1] "8"   "26"  "169" "192" "197" "255"
c2 <- c(8,26,169,192,197,255)
outputs_2 = labels[labels$coded_id %in%  c2, ]
print(outputs_2)
print(length(outputs_2$coded_id)  - length(c2)) #-2

# $`3`
## All members of this community are Spam
# [1] "86"  "92"  "108" "109" "110"
c3 <- c(8,26,169,192,197,255)
outputs_3 = labels[labels$coded_id %in%  c3, ]
print(outputs_3)
print(length(outputs_3$coded_id)  - length(c3)) #-2

# $`4`
## All members of this community are Spam
c4 <- c(8,26,169,192,197,255)
outputs_4 = labels[labels$coded_id %in%  c4, ]
print(outputs_4)
print(length(outputs_4$coded_id)  - length(c4)) #-2


## All members of this community are LEGITIME
c5 <- c(3,   9,   10,  17,  20,  33,  35,  84,  89,  98,  102, 136, 137, 138, 144, 147,
         160, 166, 171, 179, 180, 187, 188, 201, 205, 207, 214, 218, 220, 223, 227, 240,
         241, 250, 257, 262, 264, 265, 267, 309, 315, 463)

outputs_5 = labels[labels$coded_id %in%  c5, ]
print(outputs_5)
print(length(outputs_5$coded_id)  - length(c5)) #-2



# $`6`
## All members of this community are Spam
c6<- c(142, 163, 164, 182, 200, 245, 252, 254)
outputs_6 = labels[labels$coded_id %in%  c6, ]
print(outputs_6)
print(length(outputs_6$coded_id)  - length(c6)) #0


# $`7`
## All members of this community are Spam
c7<- c(4,56,194,226)
outputs_7 = labels[labels$coded_id %in%  c7, ]
print(outputs_7)
print(length(outputs_7$coded_id)  - length(c7)) #-1


# $`8`
## All members of this community are Spam
c8<- c(74,75,76)
outputs_8 = labels[labels$coded_id %in%  c8, ]
print(outputs_8)
print(length(outputs_8$coded_id)  - length(c8)) #0



# $`9`
## All members of this community are Spam
c9<- c(124,125,126)
outputs_9 = labels[labels$coded_id %in%  c9, ]
print(outputs_9)
print(length(outputs_9$coded_id)  - length(c9)) #0


# $`10`
## SPAM and LEGITIME instances
c10<- c(64,67,71)
outputs_10 = labels[labels$coded_id %in%  c10, ]
print(outputs_10)
print(length(outputs_10$coded_id)  - length(c10)) #-1



# $`11`
## All members of this community are Spam
c11<- c(59,61,231)
outputs_11 = labels[labels$coded_id %in%  c11, ]
print(outputs_11)
print(length(outputs_11$coded_id)  - length(c11)) #0


# $`12`
## SPAM and LEGITIME instances
c12<- c(80,  81,  119)
outputs_12 = labels[labels$coded_id %in%  c12, ]
print(outputs_12)
print(length(outputs_12$coded_id)  - length(c12)) #0


all <- c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12) #Observations
outputs_all = labels[labels$coded_id %in%  all, ]


length(labels[labels$label == 1, ]$coded_id) #118 Spammers
#The train set has 118, which means that our communities are not enough to identify the other spammers.
#Specially the ones that mich have degree 0 and small clustering coefficients, which we discarded from our analysis


# Let's try to understand the characteristics of Communities 1 and 5
#I chose this communities because they have more nodes which might bring more insight
# when trying to understand Spammers and Legitimates



