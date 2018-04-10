parabolic = function(alpha,v,h,detail = 0.05) {
  alphaRad = alpha*pi/180
  g = 9.8
  t = (v*sin(alphaRad)+sqrt( (v*sin(alphaRad))^2 + 2*g*h ))/g
  timeParameter = seq(0,t,detail)
  x = v*cos(alphaRad)*timeParameter
  y = y0 + v*sin(alphaRad)*timeParameter - 0.5*g*timeParameter^2
  
  for (i in 1:length(y)) {
    if (y[i] < 0) {
      y[i] = 0
    }
  }
  plot(x,y,type = "b",xlab = "jarak", ylab = "ketinggian")
  return(data.frame(time = timeParameter, distance = x, height = y))
}

deathDistance = function(alpha,v,h) {
  alphaRad = alpha*pi/180
  g = 9.8
  x = ((v^2*sin(2*alphaRad))/(2*g))*( 1 + sqrt( 1 + ((2*g*h)/(v^2*(sin(alphaRad))^2))))
  return(x)
}
