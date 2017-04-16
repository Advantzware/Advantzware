/* ------------------------------------------------------- ce/prep.v 5/92 cd  */
/*                                                                            */
/* prep file  - display                                                       */
/*                                                                            */
/* -------------------------------------------------------------------------- */

find first account where account.company = cocode and
			 account.actnum = prep.actnum
			 no-lock no-error.

display
     prep.code FORMAT "x(15)"
     prep.dscr
     prep.mkup
     prep.cost
     prep.ml
     prep.amtz
     prep.mat-type
     prep.dfault
     prep.uom
     prep.simon
     prep.actnum + " " + account.dscr format "x(35)"
		  when available account @ prep.actnum.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

