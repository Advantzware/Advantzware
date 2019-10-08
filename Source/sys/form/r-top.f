/* ------------------------------------------------- sys/form/r-top.f 9/92 cd */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 56}.
 
form header
     skip(1)
     day_str
     str-tit format "x(56)"
     "Page" at 68 page-number format ">>>"
     skip
     tim_str
     str-tit2 format "x(56)"
     skip(1)
      
    with frame r-top no-labels no-box width 80 stream-io no-underline page-top.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

