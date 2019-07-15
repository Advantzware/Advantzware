if xef.lam-dscr = "R" or (xef.lam-dscr ne "R" and xef.xgrain = "S")
then assign sh-wid = xef.gsh-wid
	    sh-len = xef.gsh-len.
else assign sh-wid = xef.gsh-len
	    sh-len = xef.gsh-wid.

