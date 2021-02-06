## Building Features Based on the relationships between the users and/or their similarities.


#row i userid i
similarity_matrix <- read.csv((paste(HOME,"graphs/app_based_similarity/sim_matrix.txt", sep="")), header=F, as.is=T)
dim(similarity_matrix) #766 767


NODES  <- read.csv((paste(HOME,"users/coded_ids.csv", sep="")), header=T, as.is=T)
dim(NODES)

NODES$app_sim_row_mean <- rowMeans(similarity_matrix)
NODES$app_sim_col_mean <- colMeans(similarity_matrix)

normalized_app_based_users_vectors <- read.csv((paste(HOME,"graphs/app_based_similarity/app_based_users_vectors/normalized_app_based_users_vectors.txt", sep="")), header=FALSE, stringsAsFactors=FALSE, sep=",")

#Removing "index:" from all elements of column V1
for(i in seq(dim(normalized_app_based_users_vectors)[1])){
  normalized_app_based_users_vectors$V1[i]<-as.double(unlist(strsplit(normalized_app_based_users_vectors$V1[i], ":"))[2])
}


#convert list to matrix to compute vector norms
m <- as.matrix(normalized_app_based_users_vectors) 

#compute norm for each vector
norms_1 = c()
for(i in seq(dim(m)[1])){
  norms_1 = c(norms_1,norm(as.matrix(m[i,]),  type = c("O")))
}

#l2 norm of a vector
norm_2_vec <- function(x) sqrt(sum(x^2))

norms_2 = c()
for(i in seq(dim(m)[1])){
  norms_2 = c(norms_2, norm_2_vec(as.numeric(as.matrix(m[i,]))))
}



normalized_app_based_users_vectors_matrix <- read.csv((paste(HOME,"graphs/app_based_similarity/app_based_users_vectors/normalized_app_based_users_vectors.txt", sep="")), header=F, as.is=T)
#Removing "index:" from all elements of column V1
for(i in seq(dim(normalized_app_based_users_vectors_matrix)[1])){
  normalized_app_based_users_vectors_matrix$V1[i]<-as.double(unlist(strsplit(normalized_app_based_users_vectors_matrix$V1[i], ":"))[2])
}

m_2 <- as.matrix(normalized_app_based_users_vectors_matrix) 

#compute norm for each vector
norms_1_m2 = c()
for(i in seq(dim(m_2)[1])){
  norms_1_m2 = c(norms_1_m2,norm(as.matrix(m_2[i,]),  type = c("O")))
}

norms_2_m2 = c()
for(i in seq(dim(m_2)[1])){
  norms_2_m2 = c(norms_2_m2, norm_2_vec(as.numeric(as.matrix(m_2[i,]))))
}

norms_1_m2 == norms_1 #SAME
norms_2_m2 == norms_2 #SAME
