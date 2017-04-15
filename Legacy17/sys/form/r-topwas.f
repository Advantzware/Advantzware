/* ---------------------------------------------- sys/form/r-topw.f 03/04 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 81}.

def var as-of-day_str like day_str format "x(15)" init "As of: " no-undo.

form  header
      skip(1)
      day_str
      as-of-day_str
      str-tit format "x(82)"
      "Page" at 123
      page-number format ">>9"
      skip
      tim_str
      str-tit2 format "x(112)"
      skip(1)
      
     with frame r-top row 1 column 1 stream-io width 132
	   no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 2004  advanced software, inc. */

