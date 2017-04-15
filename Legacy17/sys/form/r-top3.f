/* ----------------------------------------------- sys/form/r-top3.f 12/92 cd */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 56}.

form  header skip(1)
      day_str str-tit format "x(56)" "Page" at 68 page-number format ">>9"
      skip
      tim_str str-tit2 format "x(56)" skip(1)
      str-tit3 format "x(78)" skip
      with frame r-top row 1 column 1 stream-io width 80
	   no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

