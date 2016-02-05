/* ---------------------------------------------- sys/form/r-top4w.f 12/92 cd */
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
     time_stamp
     str-tit2 format "x(90)" at 10
     skip
     str-tit3 at 51 format "x(25)"
     skip
     "WHSE:" at 51
     v-whse
     
    with frame r-top row 1 column 1 stream-io width 132
	  no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

