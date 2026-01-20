# Create some fake data in a data frame
# For the homework, you won't use fake data; instead, you will use real data
d <- data.frame(chr=paste0("chr",rep(22,10)),
                chromstart=1000+1:10,
                chromend=1000+2:11,
                x=round(runif(n=10), digits=2))

# See what the fake data looks like
# It's just 10 variants on chromosome 22
d

# We need some metadata lines
# Notice the quotes within quotes
line1 <- "browser hide all"
line2 <- "track type=bedGraph name=fake_data description='simulated data' color=255,0,0 altColor=0,0,255 visibility=3 graphType=bar"

# Now write metadata lines to a file, line by line, using cat with sep="\n"
# Use append=TRUE so that the file doesn't get overwritten!
# Then append 
cat(line1, file="~/Desktop/example.bedgraph", sep="\n")
cat(line2, file="~/Desktop/example.bedgraph", sep="\n", append=TRUE)
write.table(d, file="~/Desktop/example.bedgraph",
            quote=FALSE,
            row.names=FALSE,
            col.names=FALSE,
            sep="\t",
            append=TRUE)

# Now you should open example.bedgraph with a text editor to see what it looks like!