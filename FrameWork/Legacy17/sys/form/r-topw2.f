/* ---------------------------------------------- sys/form/r-topw.f 03/04 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 172}.

form  header
      skip(1)
      day_str
      str-tit  format "x(172)"
      "Page" at 123
      page-number format ">>9"
      skip
      tim_str
      str-tit2 format "x(172)"   "{1}" at 123
      skip(1)
      
     with frame r-top row 1 column 1 stream-io width 210
	   no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 2004  advanced software, inc. */

