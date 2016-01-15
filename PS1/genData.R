N = 1000
genData = function(N){
  
  set.seed(9866)
  latitude_max = 1
  longitude_max = 1
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
  
  dist <- function(points,center) {
    apply(points , 1 , function(x) sum((x - center)^2)^(1/2) )
  }
  
  sample_latitude = runif(N,min=0,max=latitude_max)
  sample_longitude = runif(N, min=0, max=longitude_max)
  data = cbind(sample_latitude, sample_longitude)
  
  suburban = rep(FALSE, dim(data)[1])
  for (i in 1:n) {
    suburban = suburban | dist(data,suburban_areas$center_suburban[,i]) < suburban_areas$r_suburban[i]
  }
  
  plot(data[suburban == F,], pch = 4)
  points(data[suburban == T,], col="green", pch = 4)
  points(t(suburban_areas$center_suburban) , col="red", pch = 4)
  
}