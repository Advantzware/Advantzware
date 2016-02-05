/* ------------------------------------------- sys/form/r-top3ll.f  03/04 JLF */
/*   Landscape or wide carriage printers only                                                                         */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 160}.

def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.

form header
     skip(1)
     day_str
     str-tit format "x(160)"
     "Page" at 171
     page-number format ">>9"
     skip
     tim_str
     str-tit2 format "x(160)"
     skip
     str-tit3 format "x(180)"
     skip(1)
     hdr-tit format "x(180)"
     skip
     hdr-tit2 format "x(180)"
     skip
     hdr-tit3 format "x(180)"
     
    with frame r-top row 1 column 1 stream-io width 180
         no-labels no-box no-underline page-top.

/* end ---------------------------------- copr. 2004  advanced software, inc. */

