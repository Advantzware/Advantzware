/* ---------------------------------------------- sys/form/r-top3w.f 12/92 cd */
/*                                                                            */
/* -------------------------------------------------------------------------- */

form  header skip(1)
      day_str str-tit format "x(112)" "Page" at 123 page-number ({1}) format ">>9"
      skip
      tim_str str-tit2 format "x(112)" "{2}" at 123 
      str-tit3 format "x(130)"
      with frame r-top-{1} row 1 column 1 stream-io width 150
	   no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

