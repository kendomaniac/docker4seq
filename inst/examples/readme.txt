
#this is the script used to create two dataset with the same names 4t1 is mouse triple negative cell line and #mb231 is human triple negative cell line those are use as example for deseq2 wrapper

tmp1 <- read.table("4t1_counts.txt", sep="\t", header=T, row.names=1)
tmp2 <- read.table("mb231_counts.txt", sep="\t", header=T, row.names=1)
tmp1.1 <- strsplit(rownames(tmp1), ":")
tmp1.1 <- sapply(tmp1.1, function(x)x[1])
tmp1 <- tmp1[!duplicated(tmp1.1),]
tmp1.1 <- strsplit(rownames(tmp1), ":")
tmp1.1 <- sapply(tmp1.1, function(x)x[1])
rownames(tmp1) <- toupper(tmp1.1)

tmp2.1 <- strsplit(rownames(tmp2), ":")
tmp2.1 <- sapply(tmp2.1, function(x)x[1])
tmp2 <- tmp2[!duplicated(tmp2.1),]
tmp2.1 <- strsplit(rownames(tmp2), ":")
tmp2.1 <- sapply(tmp2.1, function(x)x[1])
rownames(tmp2) <- tmp2.1
common <- intersect(rownames(tmp1), rownames(tmp2))

tmp1 <- tmp1[which(rownames(tmp1)%in%common),]
tmp1 <- tmp1[order(rownames(tmp1)),]
tmp2 <- tmp2[which(rownames(tmp2)%in%common),]
tmp2 <- tmp2[order(rownames(tmp2)),]

write.table(tmp1, "4t1_counts.txt", sep="\t", col.names = NA, quote=FALSE)
write.table(tmp2, "mb231_counts.txt", sep="\t",col.names = NA, quote=FALSE)

