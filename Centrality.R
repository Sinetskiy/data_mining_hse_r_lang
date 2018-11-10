z <- graph.famous("Zachary")
plot(z,layout = layout.fruchterman.reingold)


deg=degree(z)
lay <- layout.fruchterman.reingold(z)
lay


fine = 500 # this will adjust the resolving power.
palette = colorRampPalette(c('blue','red'))
degCol = palette(fine)[as.numeric(cut(deg,breaks = fine))]
plot(z, layout=lay, vertex.color=degCol, vertex.size=deg*1.5, vertex.label.cex=0.6, main="Degree centrality")
#number of nearest neighbours


clos=closeness(z)
#Look at the values:
clos
#N/sum(d)

# Plot the graph:
closCol = palette(fine)[as.numeric(cut(clos,breaks = fine))]
plot(z,layout = lay, vertex.color=closCol, vertex.size=clos*1500, vertex.label.cex=0.6, main="Closeness centrality")



betw <- betweenness(z)
#Look at the values:
betw
betwCol = palette(fine)[as.numeric(cut(betw,breaks = fine))]
plot(z,layout = lay, vertex.color=betwCol, vertex.size=betw*0.2, vertex.label.cex=0.6, main="Betwenness centrality")


ev <- evcent(z)
# See what's in the output:
ev

ev <- evcent(z)$vector
# See what's in the output:
ev
evCol = palette(fine)[as.numeric(cut(ev,breaks = fine))]
plot(z,layout = lay, vertex.size=ev*40, vertex.color=evCol, vertex.label.cex=0.6, main="Eigenvector centrality")


bon <- bonpow(z, rescale=TRUE)
bonCol = palette(fine)[as.numeric(cut(bon,breaks = fine))]
plot(z,layout = lay, vertex.size=bon*400, vertex.color=bonCol, vertex.label.cex=0.6, main="Bonachrich centrality")



# We will plot 6 graphs in 2 rows and 3 columns:
op <- par(mfrow = c(2, 3))
#Remember we assigned a name to each graph?
plot(z, layout=lay, vertex.color=degCol, vertex.size=deg*1.5, vertex.label.cex=0.6, main="Degree centrality")
plot(z,layout = lay, vertex.color=closCol, vertex.size=clos*1500, vertex.label.cex=0.6, main="Closeness centrality")
plot(z,layout = lay, vertex.color=betwCol, vertex.size=betw*0.2, vertex.label.cex=0.6, main="Betwenness centrality")
plot(z,layout = lay, vertex.size=ev*40, vertex.color=evCol, vertex.label.cex=0.6, main="Eigenvector centrality")
plot(z,layout = lay, vertex.size=bon*500, vertex.color=bonCol, vertex.label.cex=0.6, main="Bonachich power centrality")
#plot(z,layout = lay, vertex.size=alpha*0.2, vertex.color=alphaCol, vertex.label.cex=0.6, main="Alpha centrality")