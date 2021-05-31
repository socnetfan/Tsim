
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
#3. Extract T_t1
tmp1 <- tmp2 <- E_t1
names(tmp1) <- c("i","k")
names(tmp2) <- c("j","k")
T_t1 <- merge(E_t1,tmp1,by="i",all=TRUE)
T_t1 <- T_t1[which(T_t1$j!=T_t1$k),]
T_t1 <- merge(T_t1,tmp2,by=c("j","k"))
T_t1 <- T_t1[T_t1$i<T_t1$j & T_t1$j<T_t1$k,]
nrow(T_t1)
rm(tmp, tmp1,tmp2)

#4. Read and symmetrize E_t2
E_t2 <- read.table("edgelist2.csv",header=F,sep=",")
names(E_t2) <- c("i","j")
tmp <- data.frame(cbind(E_t2[,2],E_t2[,1]))
names(tmp) <- names(E_t2)
E_t2 <- merge(E_t2,tmp,by=c("i","j"),all=TRUE)
nrow(E_t2)
#5. Extract V_t2
V_t2 <- data.frame(unique(E_t2[,1]))
nrow(V_t2)
#6. Extract T_t2
tmp1 <- tmp2 <- E_t2
names(tmp1) <- c("i","k")
names(tmp2) <- c("j","k")
T_t2 <- merge(E_t2,tmp1,by="i",all=TRUE)
T_t2 <- T_t2[which(T_t2$j!=T_t2$k),]
T_t2 <- merge(T_t2,tmp2,by=c("j","k"))
T_t2 <- T_t2[T_t2$i<T_t2$j & T_t2$j<T_t2$k,]
nrow(T_t2)
rm(tmp, tmp1,tmp2)
names(V_t1) <- names(V_t2) <- "i"

#7. Extract V_persist, V_lost, and V_new
V_persist <- merge(V_t1,V_t2,by="i")
nrow(V_persist)
V_lost <- anti_join(V_t1, V_persist, "i") 
nrow(V_lost)
V_new <- anti_join(V_t2, V_persist, "i") 
nrow(V_new)

#8. Extract E_persist, E_lost, and E_new
E_persist <- merge(E_t1,E_t2, by=c("i","j"))
nrow(E_persist)
E_lost <- anti_join(E_t1, E_persist, c("i","j")) 
nrow(E_lost)
E_new <- anti_join(E_t2, E_persist, c("i","j")) 
nrow(E_new)

#9. Extract T_persist, T_lost, and T_new
T_persist <- merge(T_t1,T_t2,by=c("i","j","k"))
nrow(T_persist)
T_lost <- anti_join(T_t1, T_persist, c("i","j","k")) 
nrow(T_lost)
T_new <- anti_join(T_t2, T_persist, c("i","j","k")) 
nrow(T_new)

#10. Extract the number of triangles losing one edge, two edges, and all three edges in T_lost
tmp1 <- tmp2 <- tmp3 <- E_persist
tmp1$ij <- tmp2$ik <- tmp3$jk <- 1
names(tmp1) <- c("i","j","ij")
names(tmp2) <- c("i","k","ik")
names(tmp3) <- c("j","k","jk")
T_lost <- merge(T_lost,tmp1,by=c("i","j"),all=TRUE)
T_lost <- T_lost[is.na(T_lost$k)==FALSE,]
T_lost <- merge(T_lost,tmp2,by=c("i","k"),all=TRUE)
T_lost <- T_lost[is.na(T_lost$j)==FALSE,]
T_lost <- merge(T_lost,tmp3,by=c("j","k"),all=TRUE)
T_lost <- T_lost[is.na(T_lost$i)==FALSE,]
T_lost[is.na(T_lost)] <- 0
T_lost$lostnum <- 3-(T_lost$ij + T_lost$ik + T_lost$jk)
T_lostnum <- as.numeric(c(table(T_lost$lostnum)))

#11. Extract the number of triangles gaining one edge, two edges, and all three edges in T_new
T_new <- merge(T_new,tmp1,by=c("i","j"),all=TRUE)
T_new <- T_new[is.na(T_new$k)==FALSE,]
T_new <- merge(T_new,tmp2,by=c("i","k"),all=TRUE)
T_new <- T_new[is.na(T_new$j)==FALSE,]
T_new <- merge(T_new,tmp3,by=c("j","k"),all=TRUE)
T_new <- T_new[is.na(T_new$i)==FALSE,]
T_new[is.na(T_new)] <- 0
T_new$newnum <- 3-(T_new$ij + T_new$ik + T_new$jk)
T_newnum <- as.numeric(c(table(T_new$newnum)))
rm(tmp1,tmp2,tmp3)

#12. Randomly select triangles to be removed by their survive rates in each degree group
deg_t1 <- data.frame(table(E_t1$i))
names(deg_t1) <- c("i","degt1")
tmp <- deg_t1[deg_t1$degt1>=2,]
library(dplyr)
tmp$group <- ntile(tmp$degt1, 3) 
tmp <- tmp[,-2]
tmp1 <- tmp2 <- tmp
names(tmp) <- c("i","groupi")
names(tmp1) <- c("j","groupj")
names(tmp2) <- c("k","groupk")

T_t2_random <- inner_join(T_t1, T_persist, c("i","j","k")) %>%
              mutate(merge = "both") 
T_t2_random <- rbind(T_t2_random, anti_join(T_t1, T_persist, c("i","j","k")) %>% 
              mutate(merge = 'left_only'))
T_t2_random <- merge(T_t2_random,tmp,by="i")
T_t2_random <- merge(T_t2_random,tmp1,by="j")
T_t2_random <- merge(T_t2_random,tmp2,by="k")
rm(tmp,tmp1,tmp2)
T_t2_random[,5:7] <- t(apply(T_t2_random[,5:7],1,sort))
T_t2_random$group <- T_t2_random$groupi*100+T_t2_random$groupj*10+T_t2_random$groupk
tmp <- as.data.frame.matrix(table(T_t2_random$group,T_t2_random$merge))
tmp$g <- as.numeric(c(rownames(tmp)))
T_lost_random <- data.frame(i = numeric(),      
                            j = numeric(),
                            k = numeric(),
                            stringsAsFactors = FALSE)
for (z in 1:nrow(tmp)) {
  T_lost_random <- rbind(T_lost_random,T_t2_random[sample(which(T_t2_random$group==tmp$g[z]),tmp[z,2]),1:3])
}
rm(tmp)

#13. Randomly remove edges in the selected triangles by the triadic evolution pattern in the real transition
E_lost_random <- data.frame(i = numeric(),      
                            j = numeric(),
                            stringsAsFactors = FALSE)
tmp1 <- T_lost_random[sample(nrow(T_lost_random),T_lostnum[1]),]
for (z in 1:nrow(tmp1))  {
  tmp2 <- sample(c("ij","jk","ik"),1)
  if (tmp2=="ij") {
    tmp3 <- tmp1[z,-1]
  } 
  if (tmp2=="jk") {
    tmp3 <- tmp1[z,-3]
  } 
  if (tmp2=="ik") {
    tmp3 <- tmp1[z,-2]
  } 
  names(tmp3) <- c("i","j")  
  E_lost_random <- rbind(E_lost_random,tmp3)
}
rm(tmp2,tmp3)
tmp2 <- anti_join(T_lost_random, tmp1, c("i","j","k")) 
tmp3 <- tmp2[sample(nrow(tmp2),T_lostnum[2]),]
for (z in 1:nrow(tmp3)) {
  tmp4 <- sample(c("ij","jk","ik"),1)
  if (tmp4=="ij") {
    tmp5 <- tmp3[z,-3]
    tmp6 <- tmp3[z,-2]
  } 
  if (tmp4=="jk") {
    tmp5 <- tmp3[z,-2]
    tmp6 <- tmp3[z,-1]
  } 
  if (tmp4=="ik") {
    tmp5 <- tmp3[z,-3]
    tmp6 <- tmp3[z,-1]
  } 
  names(tmp5) <- names(tmp6) <- c("i","j")  
  E_lost_random <- rbind(E_lost_random,tmp5,tmp6)
}
rm(tmp4,tmp5,tmp6)
tmp4 <- anti_join(tmp2, tmp3, c("i","j","k")) 
for (z in 1:nrow(tmp4)) {
  tmp5 <- tmp4[z,-1]
  tmp6 <- tmp4[z,-2]
  tmp7 <- tmp4[z,-3]
  names(tmp5) <- names(tmp6) <- names(tmp7) <- c("i","j")  
  E_lost_random <- rbind(E_lost_random,tmp5,tmp6,tmp7)
}  
rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7)
E_lost_random <- unique(E_lost_random)
tmp <- data.frame(cbind(E_lost_random[,2],E_lost_random[,1]))
names(tmp) <- c("i","j")
E_lost_random <- unique(rbind(E_lost_random,tmp))
E_t2_random <- anti_join(E_t1, E_lost_random, c("i","j")) 

#14.Keep removing nodes randomly by survival rate of each degree group
tmp1 <- tmp2 <- E_t2_random
names(tmp1) <- c("i","k")
names(tmp2) <- c("j","k")
T_persist_random <- merge(E_t2_random,tmp1,by="i",all=TRUE)
T_persist_random <- T_persist_random[which(T_persist_random$j!=T_persist_random$k),]
T_persist_random <- merge(T_persist_random,tmp2,by=c("j","k"))
T_persist_random <- T_persist_random[T_persist_random$i<T_persist_random$j & T_persist_random$j<T_persist_random$k,]
rm(tmp, tmp1,tmp2)

deg_t1 <- data.frame(table(E_t1$i))
names(deg_t1) <- c("i","degt1")
deg_tmp <- data.frame(table(E_t2_random$i))
names(deg_tmp) <- c("i","degt1")
deg_t1$persist <- deg_t1$i %in% deg_tmp$i
V_lost_random <- as.numeric(deg_t1[deg_t1$persist==FALSE,1])
deg_t1$id <- deg_t1$i %in% V_persist$i
V_in_T_persist_random <- data.frame(unique(c(T_persist_random$i,T_persist_random$j,T_persist_random$k)))
V_in_T_persist_random$inT <- 1
names(V_in_T_persist_random) <- c("i","inT")
deg_t1 <- merge(deg_t1,V_in_T_persist_random,by="i",all=TRUE)
deg_t1[is.na(deg_t1)] <- 0
tmp <- as.data.frame.matrix(table(deg_t1$degt1,deg_t1$id))
tmp$d <- as.numeric(c(rownames(tmp)))
tmp1 <- data.frame(table(deg_t1[deg_t1$persist==FALSE,2]))
names(tmp1)[1] <- "d"
tmp <- merge(tmp,tmp1,by="d",all=TRUE)
tmp[is.na(tmp)] <- 0
tmp[,2] <- tmp[,2]-tmp[,4]
for (z in 1:nrow(tmp)) {
  V_lost_random <- c(V_lost_random,sample(which(deg_t1$degt1==tmp$d[z] & deg_t1$persist==TRUE & deg_t1$inT==0),tmp[z,2]))
}

#15. Keep removing edges randomly between persisting nodes
E_t2_random <- E_t2_random[-which(E_t2_random$i %in% V_lost_random | E_t2_random$j %in% V_lost_random),]
rm(tmp,tmp1)
E_persist_num <- nrow(E_t2_random)
tmp1 <- T_persist_random[,-1]
tmp2 <- T_persist_random[,-2]
tmp3 <- T_persist_random[,-3]
names(tmp1) <- names(tmp2) <- names(tmp3) <- c("i","j")
tmp4 <- unique(rbind(tmp1,tmp2,tmp3))
tmp5 <- data.frame(cbind(tmp4[,2],tmp4[,1]))
names(tmp5) <- c("i","j")
E_in_T_persist_random <- merge(tmp4,tmp5,by=c("i","j"),all=TRUE)
rm(tmp1,tmp2,tmp3,tmp4,tmp5)
E_in_T_persist_random$inT <- 1
E_t2_random <- merge(E_t2_random,E_in_T_persist_random,by=c("i","j"),all=TRUE)
E_t2_random[is.na(E_t2_random)] <- 0
while (E_persist_num > nrow(E_persist)) {
  tmp <- sample(c(1:nrow(E_t2_random)),1)
  if (E_t2_random[tmp,3]==0) {
    e1 <- E_t2_random[tmp,]
    e2 <- E_t2_random[-tmp,]
    e2 <- e2[-which(e2$i==e1$j & e2$j==e1$i),]
    if (e1$i %in% e2$i & e1$j %in% e2$j) {
      E_t2_random <- e2
      E_persist_num <- nrow(E_t2_random)
    }
  }
}
rm(tmp)
E_t2_random <- E_t2_random[,-3]

#16. Add new nodes and randomly link them to persisting nodes
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

#17. Randomly add edges to form new triangles by the triadic evolution pattern in the real transition
T_new3num <- 0
while (T_new3num < T_newnum[3]) {
  tmp <- sample(V_t2_random,3)
  tmp1 <- data.frame(rbind(c(tmp[1],tmp[2])))
  tmp2 <- data.frame(rbind(c(tmp[1],tmp[3])))
  tmp3 <- data.frame(rbind(c(tmp[2],tmp[3])))
  names(tmp1) <- names(tmp2) <- names(tmp3) <- c("i","j")
  if (nrow(merge(tmp1,E_t2_random,by=c("i","j")))==0 & nrow(merge(tmp2,E_t2_random,by=c("i","j")))==0 & nrow(merge(tmp3,E_t2_random,by=c("i","j")))==0) {
    tmp4 <- rbind(tmp1,tmp2,tmp3)
    tmp5 <- data.frame(cbind(tmp4[,2],tmp4[,1]))
    names(tmp5) <- c("i","j")
    tmp4 <- merge(tmp4,tmp5,by=c("i","j"),all=TRUE)
    E_t2_random <- rbind(E_t2_random,tmp4)
    T_new3num <- T_new3num +1
  }
}
rm(tmp,tmp1,tmp2,tmp3,tmp4,tmp5)

T_new2num <- 0
while (T_new2num < T_newnum[2]) {
  tmp <- sample(V_t2_random,3)
  tmp1 <- data.frame(rbind(c(tmp[1],tmp[2])))
  tmp2 <- data.frame(rbind(c(tmp[1],tmp[3])))
  tmp3 <- data.frame(rbind(c(tmp[2],tmp[3])))
  names(tmp1) <- names(tmp2) <- names(tmp3) <- c("i","j")
  cond1 <- nrow(merge(tmp1,E_t2_random,by=c("i","j")))==1 & nrow(merge(tmp2,E_t2_random,by=c("i","j")))==0 & nrow(merge(tmp3,E_t2_random,by=c("i","j")))==0
  cond2 <- nrow(merge(tmp1,E_t2_random,by=c("i","j")))==0 & nrow(merge(tmp2,E_t2_random,by=c("i","j")))==1 & nrow(merge(tmp3,E_t2_random,by=c("i","j")))==0
  cond3 <- nrow(merge(tmp1,E_t2_random,by=c("i","j")))==0 & nrow(merge(tmp2,E_t2_random,by=c("i","j")))==0 & nrow(merge(tmp3,E_t2_random,by=c("i","j")))==1
  if (cond1 | cond2 | cond3) {
    tmp4 <- rbind(tmp1,tmp2,tmp3)
    tmp5 <- data.frame(cbind(tmp4[,2],tmp4[,1]))
    names(tmp5) <- c("i","j")
    tmp4 <- merge(tmp4,tmp5,by=c("i","j"),all=TRUE)
    E_t2_random <- merge(E_t2_random,tmp4,by=c("i","j"),all=TRUE)
    T_new2num <- T_new2num+1
  }
}
rm(tmp,tmp1,tmp2,tmp3,tmp4,tmp5)

tmp1 <- tmp2 <- E_t2_random
names(tmp1) <- c("i","k")
names(tmp2) <- c("j","k")
T_t2_random <- merge(E_t2_random,tmp1,by="i",all=TRUE)
T_t2_random <- tmp3 <- T_t2_random[which(T_t2_random$j!=T_t2_random$k),]
T_t2_random <- merge(T_t2_random,tmp2,by=c("j","k"))
tmp4 <- anti_join(tmp3, T_t2_random, c("i","j", "k")) 
T_t2_random <- T_t2_random[T_t2_random$i<T_t2_random$j & T_t2_random$j<T_t2_random$k,]
tmp4 <- tmp4[tmp4$i<tmp4$j & tmp4$j<tmp4$k,]
tmp5 <- sample(c(1:nrow(tmp4)), nrow(T_t2)-nrow(T_t2_random))
tmp6 <- unique(tmp4[tmp5,-1])
tmp7 <- data.frame(cbind(tmp6[,2],tmp6[,1]))
names(tmp6) <- names(tmp7) <- c("i","j")
tmp6 <- merge(tmp6,tmp7,by=c("i","j"),all=TRUE)
E_t2_random <- merge(E_t2_random,tmp6,by=c("i","j"),all=TRUE)
rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6)

#18. Keep adding edges randomly between nodes
E_t2_num <- nrow(E_t2_random)
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

#19. Output the simulated edgelist
save(E_t2_random,file="algorithm2.Rdata")
