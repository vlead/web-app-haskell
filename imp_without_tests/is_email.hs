firstString :: String -> String -> String
firstString "" acc = acc
firstString str acc = if ((head str) /= '@') 
						then firstString (tail str) ((head str) : acc)
					else acc

