#function to calculate the area of a rectangle
areaRect <- function(rect)
{
	lengthx <- abs(diff(rect[1:2]))
	lengthy <- abs(diff(rect[3:4]))
	return(lengthx * lengthy)
}
