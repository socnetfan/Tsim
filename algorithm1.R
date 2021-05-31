
rm(list=ls())

#1. Read and symmetrize E_t1
E_t1 <- read.table("edgelist1.csv",header=F,sep=",")
names(E_t1) <- c("i","j")
tmp <- data.frame(cbind(E_t1[,2],E_t1[,1]))
names(tmp) <- names(E_t1)
E_t1 <- merge(E_t1,tmp,by=c("i","j"),all=TRUE)
nrow(E_t1)
#2. Extract V_t1
V_t1 <- data.frame(unique(E_t1[,1]))
nrow(V_t1)
rm(tmp)

#3. Read and symmetrize E_t2
E_t2 <- read.table("D:/Dropbox/smartphone/polview/w3.csv",header=F,sep=",")
names(E_t2) <- c("i","j")
tmp <- data.frame(cbind(E_t2[,2],E_t2[,1]))
names(tmp) <- names(E_t2)
E_t2 <- merge(E_t2,tmp,by=c("i","j"),all=TRUE)
nrow(E_t2)
#4. Extract V_t2
V_t2 <- data.frame(unique(E_t2[,1]))
nrow(V_t2)
rm(tmp)
names(V_t1) <- names(V_t2) <- "i"

#5. Extract V_persist, V_lost, and V_new
V_persist <- merge(V_t1,V_t2,by="i")
nrow(V_persist)
V_lost <- anti_join(V_t1, V_persist, "i") 
nrow(V_lost)
V_new <- anti_join(V_t2, V_persist, "i") 
nrow(V_new)

#6. Extract E_persist, E_lost, and E_new
E_persist <- merge(E_t1,E_t2, by=c("i","j"))
nrow(E_persist)
E_lost <- anti_join(E_t1, E_persist, c("i","j")) 
nrow(E_lost)
E_new <- anti_join(E_t2, E_persist, c("i","j")) 
nrow(E_new)

#7. Randomly remove nodes by survival rate of each degree group
deg_t1 <- data.frame(table(E_t1$i))
names(deg_t1) <- c("i","degt1")
deg_t1$id <- deg_t1$i %in% V_persist$i
tmp <-as.data.frame.matrix(table(deg_t1$degt1,deg_t1$id))
tmp$d <- as.numeric(c(rownames(tmp)))
V_lost_random <- c()
for (z in 1:nrow(tmp)) {
  V_lost_random <- c(V_lost_random,sample(which(deg_t1$degt1==tmp$d[z]),tmp[z,1]))
}
rm(tmp)

#8. Randomly remove edges between persisting nodes
E_t2_random <- E_t1[-which(E_t1$i %in% V_lost_random | E_t1$j %in% V_lost_random),]
E_persist_num <- nrow(E_t2_random)
while (E_persist_num > nrow(E_persist)) {
  tmp <- sample(nrow(E_t2_random),1)
  e1 <- E_t2_random[tmp,]
  e2 <- E_t2_random[-tmp,]
  e2 <- e2[-which(e2$i==e1$j & e2$j==e1$i),]
  deg_d2 <- data.frame(table(e2$i))
  if (e1$i %in% e2$i & e1$j %in% e2$j) {
    E_t2_random <- e2
    E_persist_num <- nrow(E_t2_random)
  }
}

#9. Add new nodes and randomly link them to persisting nodes
V_t2_random <- unique(c(E_t2_random$i))
V_new_random <- c((max(c(E_t2_random$i))+1):(max(c(E_t2_random$i))+nrow(V_new)))
E_new_random <- data.frame(cbind(sample(V_t2_random,nrow(V_new)),V_new_random))
names(E_new_random) <- c("i","j")
tmp <- data.frame(cbind(E_new_random[,2],E_new_random[,1]))
names(tmp) <- names(E_new_random)
E_new_random <- merge(E_new_random,tmp,by=c("i","j"),all=TRUE)
rm(tmp)
E_t2_random <- merge(E_t2_random,E_new_random,by=c("i","j"),all=TRUE)
V_t2_random <- unique(c(E_t2_random$i))

#10. Randomly add edges between nodes
while (E_t2_num < nrow(E_t2)) {
  tmp <- sample(V_t2_random,2)
  e1 <- data.frame(cbind(tmp[1],tmp[2]))
  names(e1) <- c("i","j")
  e2 <- data.frame(cbind(tmp[2],tmp[1]))
  names(e2) <- c("i","j")
  if (nrow(merge(E_t2_random,e1,by=c("i","j")))==0) {
    E_t2_random <- rbind(E_t2_random,e1,e2)
    E_t2_num <- nrow(E_t2_random)
  }
}

#11. Output the simulated edgelist
save(E_t2_random,file="algorithm1.Rdata")
