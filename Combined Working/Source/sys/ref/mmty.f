/* -------------------------------------------------- sys/ref/mmty.f 1/92 cd  */
/* Make Ready Matrix - FORM                                                   */
/* -------------------------------------------------------------------------- */

form header "                 M A C H I N E   S T A N D A R D S   M A T R I X"
     with frame top2 no-labels no-box no-underline width 81 row 2 overlay.
form
"MR" " MACHINE NO:" mmty.m-code   mach.m-dscr   format "x(15)"
    " STYLE:"    mmty.style    format "x(6)"
		 style.dscr    format "x(15)"
     /* " MR/RUN :  MR"    */          skip

  mmty.c-title at 25 format "x(50)"  skip
  v-page-across space(0) mmty.col-value[1 for 10]  skip

  space(0) mmty.rtit[1]  space(0) mmty.row-value[1] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[11  for 10] skip
  space(0) mmty.rtit[2]  space(0) mmty.row-value[2] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[21  for 10] skip
  space(0) mmty.rtit[3]  space(0) mmty.row-value[3] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[31  for 10] skip
  space(0) mmty.rtit[4]  space(0) mmty.row-value[4] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[41  for 10] skip
  space(0) mmty.rtit[5]  space(0) mmty.row-value[5] format ">>>>>>>9.<<<<<"
    space(0) mmty.vals[51  for 10] skip
  space(0) mmty.rtit[6]  space(0) mmty.row-value[6] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[61  for 10] skip
  space(0) mmty.rtit[7]  space(0) mmty.row-value[7] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[71  for 10] skip
  space(0) mmty.rtit[8]  space(0) mmty.row-value[8] format ">>>>>>>9.<<<<<"
      space(0) mmty.vals[81  for 10] skip
  space(0) mmty.rtit[9]  space(0) mmty.row-value[9] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[91  for 10] skip
  space(0) mmty.rtit[10] space(0) mmty.row-value[10] format ">>>>>>>9.<<<<<"
    space(0) mmty.vals[101 for 10] skip
  space(0) mmty.rtit[11] space(0) mmty.row-value[11] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[111 for 10] skip
  space(0) mmty.rtit[12] space(0) mmty.row-value[12] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[121 for 10] skip
  space(0) mmty.rtit[13] space(0) mmty.row-value[13] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[131 for 10] skip
  space(0) mmty.rtit[14] space(0) mmty.row-value[14] format ">>>>>>>9.<<<<<"
    space(0) mmty.vals[141 for 10] skip
  space(0) mmty.rtit[15] space(0) mmty.row-value[15] format ">>>>>>>9.<<<<<"
     space(0) mmty.vals[151 for 10]
  with frame mmty no-labels no-box no-underline stream-io width 81 row 3 overlay.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
