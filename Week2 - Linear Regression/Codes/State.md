State datasets, Analytics Edge, MITx: 15.073x
========================================================

### Tarek Dib

## *Intoduction*




```r
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center, 
    state.division, state.name, state.region)
# Region with the highest average highschool graduation
which.max(tapply(HS.Grad, state.region, mean))
```

```
## Error: object 'HS.Grad' not found
```

```r
# State with highest murder in the northeast region
NortheastData = statedata[state.region == "Northeast", ]
levels(factor(NortheastData$state.abb[which.max(NortheastData$Murder)]))
```

```
## [1] "NY"
```



```r
plot(statedata$x, statedata$y, pch = 19, col = "blue", xlab = "Longitude", ylab = "Latitude")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
boxplot(Murder ~ state.region, col = "blue")
```

```
## Error: object 'Murder' not found
```

