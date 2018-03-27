/* -------------------------------------------------- sys/ref/mmtx.f 1/92 cd  */
/* RUN MATRIX - FORM                                                          */
/* -------------------------------------------------------------------------- */

def var v-page-across as char format "x(9)".

form header "                 M A C H I N E   S T A N D A R D S   M A T R I X"
     with frame top2 no-labels no-box no-underline width 81 row 2 overlay.

form
  mmtx.mr-run
  " MACHINE NO:" mmtx.m-code   mach.m-dscr   format "x(15)"
    " STYLE:"    mmtx.style    format "x(6)"
		 style.dscr    format "x(15)"
/* " MR/RUN :" mmtx.mr-run  */ skip
  mmtx.c-title at 25 format "x(50)"  skip
  v-page-across space(0) mmtx.col-value[1 for 10]  skip
  
  space(0) mmtx.rtit[1]  space(0) mmtx.row-value[1] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[11 for 10] skip
  space(0) mmtx.rtit[2]  space(0) mmtx.row-value[2] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[21 for 10] skip
  space(0) mmtx.rtit[3]  space(0) mmtx.row-value[3] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[31 for 10] skip
  space(0) mmtx.rtit[4]  space(0) mmtx.row-value[4] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[41 for 10] skip
  space(0) mmtx.rtit[5]  space(0) mmtx.row-value[5] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[51 for 10] skip
  space(0) mmtx.rtit[6]  space(0) mmtx.row-value[6] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[61 for 10] skip
  space(0) mmtx.rtit[7]  space(0) mmtx.row-value[7] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[71 for 10] skip
  space(0) mmtx.rtit[8]  space(0) mmtx.row-value[8] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[81 for 10] skip
  space(0) mmtx.rtit[9]  space(0) mmtx.row-value[9] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[91 for 10] skip
  space(0) mmtx.rtit[10] space(0) mmtx.row-value[10] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[101 for 10] skip
  space(0) mmtx.rtit[11] space(0) mmtx.row-value[11] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[111 for 10] skip
  space(0) mmtx.rtit[12] space(0) mmtx.row-value[12] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[121 for 10] skip
  space(0) mmtx.rtit[13] space(0) mmtx.row-value[13] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[131 for 10] skip
  space(0) mmtx.rtit[14] space(0) mmtx.row-value[14] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[141 for 10] skip
  space(0) mmtx.rtit[15] space(0) mmtx.row-value[15] format ">>>>>>>9.<<<<<"
     space(0) mmtx.vals[151 for 10]

  with frame mmtx no-labels no-box no-underline stream-io width 81 overlay.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
