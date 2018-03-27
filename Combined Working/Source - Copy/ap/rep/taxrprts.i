/* ---------------------------------------------- ap/rep/taxrprts.i 10/96 JLF */
/* Tax Reports Include                                                        */
/* -------------------------------------------------------------------------- */

/* JLF rmv'd 05/02/97
    BUILD-WORK:
    for each ar-inv
	where ar-inv.company        eq cocode

	  and ar-inv.inv-date       ge v-date[1]
	  and ar-inv.inv-date       le v-date[2]

	  and ar-inv.tax-code       ne ""
	  and ar-inv.posted

	use-index inv-date no-lock.

      create w-tax.
      assign
       w-tax.tax-gr = ar-inv.tax-code
       w-tax.rec-id = recid(ar-inv).

    end.  /* BUILD-WORK */
   JLF rmv'd 05/02/97 */

    assign
     str-tit  = coname
     str-tit2 = "{1}"
     str-tit3 = "(" + string(v-date[1]) + "-" + string(v-date[2]) + ")"
     x = (112 - length(str-tit)) / 2
     str-tit  = fill(" ",x) + str-tit
     x = (115 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3
     v-tax-gl = "".

    {sys/msg/print.i print}
    {sys/inc/outprint.i 58}

    view frame r-top.
    view frame f-top.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
