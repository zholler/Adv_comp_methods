genData = function(N, saveData=TRUE, savePlot=TRUE){
  #Generates random points in a map (e.g. location of houses for sale) and adds a label whether
  #the point is in an urban or in a suburban area. Location and size of suburban areas are also 
  #randomly generated.
  
  set.seed(78696)
  if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  #Max value of latitude and longitude - defines size of the whole area
  latitude_max = 1
  longitude_max = 1
  #Number ofsuburban areas
  n = 5
  
  genAreas = function(latitude_max = 1, longitude_max = 1, center_size=0.4 , n = 5, ratio = 0.2) {
    #Generates the suburban areas' center and radius
    #input =  size of the whole area latitude, longitude, size of the center area, 
    #number of suburban areas, overall ratio of size of suburban area
    
    #center of the whole area
    center = c(latitude_max/2, longitude_max/2)
    #overall size of suburban area
    area = latitude_max * longitude_max *ratio
    #size and radius of suburbain areas
    areas = sort(runif(n,min=0, max=area))
    areas = sort(areas - c(0,areas[1:(n-1)]), decreasing=T)
    areas_r = (areas / pi)^(1/2)
    # location of suburban areas
    k=0
    #Initialize location matrix
    l = matrix(rep(NA,1*n),2,n)
    while (k<5) {
      l[1,k+1] = runif(1,min=0,max=latitude_max)
      l[2,k+1] = runif(1,min=0,max=longitude_max)
      #Check if suburban area center is outside the center of the city
      if ( (sum((l[,k+1] - center)^2))^(1/2) > center_size ) {
        #Check if suburban areas do not overlap each other
        distance = c()
        if (k!=0) {
          for (i in 1:k) {
            distance = c( distance , sum((l[,k+1] - l[,i])^2)^(1/2) )
          }
          if ( sum(distance > areas_r[1:k] + areas_r[k+1]) == k ) {
            k=k+1
          }
        }
        else{
          k=k+1
        }
      }
    }
    return(list(center_suburban = l, r_suburban = areas_r))
  }
  suburban_areas = genAreas()
  
  #Function to compute distace of some points from a center
  dist <- function(points,center) {
    apply(points , 1 , function(x) sum((x - center)^2)^(1/2) )
  }
  
  #Generating random location data (e.g.: location of houses for sale)
  sample_latitude = runif(N,min=0,max=latitude_max)
  sample_longitude = runif(N, min=0, max=longitude_max)
  data = as.data.frame(cbind(sample_latitude, sample_longitude))
  
  #Initialize catgory variable - False if house is in urban area, True if in suburban
  suburban = rep(FALSE, dim(data)[1])
  #Check if house is in suburban area
  for (i in 1:n) {
    suburban = suburban | dist(data,suburban_areas$center_suburban[,i]) < suburban_areas$r_suburban[i]
  }
  
  #Add category
  data=cbind(data,suburban)
  #Add category labels
  data$suburban_label = rep("urban",nrow(data))
  data$suburban_label[data$suburban==TRUE] = "suburban"
  
  #Save data
  if (saveData) {
    write.csv(data,file = "dataset.csv")
  }
  
  #Saving plot
  if (savePlot) {
    
    ggplot(data = data,
           aes(x = sample_latitude, y = sample_longitude, colour=suburban_label)) +
      geom_point() +
      xlab("latitude") +
      ylab("longitude")  + theme_bw() +
      scale_color_manual("Category", 
                         values = c("urban" = "black", "suburban" = "green"))
    ggsave("dataPlot.pdf")
  }
  return(data)
}

N = 1000
data = genData(N)
