#function to check whether one rectangle is contained within another
containsBounds <- function(tile, bounds)
{
	if(tile[2] < bounds[1]) return(0)
	else
	{
		if(tile[1] > bounds[2]) return(0)
		else
		{
			if(tile[4] < bounds[3]) return(0)
			else
			{
				if(tile[3] > bounds[4]) return(0)
				else
				{
					if(tile[1] < bounds[1] & tile[2] > bounds[2] & tile[3] < bounds[3] & tile[4] > bounds[4]) return(1)
					else return(0.5)
				}
			}
		}
	}
}

