/*  rc/depunctA.i - return only ALPHA CHARS in string field */
    str_buffa = {1}.
    str_buffb = "".
    str_xx = 1.
    str_yy = 1.
    do str_xx = 1 to length(str_buffa):
	if {rc/isalpha.i substring(str_buffa,str_xx,1)} then do:
	    substring(str_buffb,str_yy,1) = substring(str_buffa,str_xx,1).
	    str_yy = str_yy + 1.
	end.
    end.
    {1} = str_buffb.
