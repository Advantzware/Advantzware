/* ---------------------------------------------- sys/form/r-topl.f 03/04 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 160}.

form  header
      skip(1)
      day_str
      str-tit  format "x(160)"
      "Page" at 171
      page-number format ">>9"
      skip
      tim_str
      str-tit2 format "x(160)"
      skip(1)
      
     with frame r-top row 1 column 1 stream-io width 180
	   no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 2004  advanced software, inc. */

