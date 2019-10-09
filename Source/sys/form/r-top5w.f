/* ---------------------------------------------- sys/form/r-top3w.f 12/92 cd */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 112}.

form header 
     skip(1)
     day_str
     str-tit format "x(112)"
     "Page" at 123
     page-number format ">>9"
     skip
     tim_str
     str-tit2 format "x(112)"
     skip
     str-tit3 format "x(112)"
     skip
     str-tit4 format "x(112)"
     skip
     str-tit5 format "x(112)"
     skip
     str-tit6 format "x(112)"
     skip
     str-tit7 format "x(112)"
     skip(1)
     
    with frame r-top row 1 column 1 stream-io width 132
	  no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

