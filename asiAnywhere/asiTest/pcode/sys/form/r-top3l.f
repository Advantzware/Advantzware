/* --------------------------------------------- sys/form/r-top3l.f 8/94 gb */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 112}.

def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.

form header
     skip(1)
     day_str
     str-tit format "x(58)"
     "Page" at 72
     page-number format ">>9"
     skip
     tim_str
     str-tit2 format "x(58)"
     skip
     str-tit3 format "x(80)"
     skip(1)
     hdr-tit format "x(80)"
     skip
     hdr-tit2 format "x(80)"
     skip
     hdr-tit3 format "x(80)"
      
    with frame r-top row 1 column 1 width 80
         no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

