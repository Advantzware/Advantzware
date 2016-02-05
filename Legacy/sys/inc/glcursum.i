/* glcursum.i  same as gldstsum.i but use-cur-act field */

release wkdistrib.
/*
      if {1} = xar-acct then
      do:
	if {2} <= 0 then
          find first wkdistrib where wkdistrib.actnum = {1} and 
				wkdistrib.amount < 0 and
				wkdistrib.ar-type-credit = no no-lock no-error.
	else
          find first wkdistrib where wkdistrib.actnum = {1} and 
				wkdistrib.amount < 0 and
				wkdistrib.ar-type-credit = yes no-lock no-error.
      	if not avail wkdistrib then do:
	  create wkdistrib.
	  assign wkdistrib.actnum = {1}.
	  if {2} <= 0 then
	    assign wkdistrib.ar-type-credit = no.
	  else
	    assign wkdistrib.ar-type-credit = yes.
      	end.
      end.
      else
        find first wkdistrib
        where wkdistrib.actnum = {1} no-lock no-error.
      if not avail wkdistrib then do:
*/
	create wkdistrib.
	assign wkdistrib.actnum = {1}.
/*
      end.
if wkdistrib.ar-type-credit = no then
*/
      assign wkdistrib.amount = wkdistrib.amount + {2}
             wkdistrib.use-cur-act = YES   /* for currency code variance */
	         wkdistrib.recs = wkdistrib.recs + 1.
/*
else
      assign wkdistrib.amount = wkdistrib.amount + (-1 * {2}
	wkdistrib.recs = wkdistrib.recs + 1.
*/
