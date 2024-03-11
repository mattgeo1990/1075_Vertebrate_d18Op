# WIS should have had d18O surface water that was a few per mille higher than mean global sea surface water AT THAT SAME LATITUDE
lowEM <- -2

# conservative ipsometric mean from upstream highland catchment 
hiEM <- -20

fA <- seq(0.01,1,by=0.01)

d18Oriver <- ((fA)*hiEM) + ((1-fA)*lowEM)

plot(d18Oriver)
