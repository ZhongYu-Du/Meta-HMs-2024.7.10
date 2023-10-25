# Meta-HMs
## The code for plant responses to heavy metals

### Author: Zhongyu Du

### Time: 2023-10-25

# Code
```
install.packages("geodata")
library(geodata)
library(raster)
library(sp)
library(rgdal)
 
# Get soil grid layers (tif files)
gph <- soil_world(var="phh2o", depth=5, path=tempdir())
 
#Image visualization
plot(gph, ylim = c(-60,90), xlim = c(-180,180),
col = colorRampPalette(c("#3288BD","#66C2A5","#FEE08B","#D53E4F"))(250))
 
# create RasterLayer
ph <- raster(gph)
plot(ph)
 
# Get soil grid points  
data = read.csv("point.csv")
coordinates(data) = c("longitude", "latitude")
phd <- extract(x = ph, y = data)
datapoint <-data.frame (data,phd)
write.csv(datapoint,"E:/Seoyeol/Paper/4ENG2/Data/ph.csv")#生成csv文件
```


# Figure 1
![image](https://github.com/Byonone/Meta-HMs/blob/main/Final.jpg)


