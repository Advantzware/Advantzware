/* -------------------------------------------------- sys/ref/mstd.f 1/92 cd  */
/* Machine Standards File Maintenance - FORM                                  */
/* -------------------------------------------------------------------------- */

form header "                 M A C H I N E   S T A N D A R D S   F I L E "
     with frame top2 no-labels no-box no-underline row 2 overlay width 81.

form
  "  MACHINE NO: " mstd.m-code   space(1) mach.m-dscr   skip
  "  DEPARTMENT: " mstd.dept  space(5) dept.dscr skip
  "  STYLE CODE: " mstd.style format "x(6)" space(3) style.dscr skip
  head1 skip
  "  MAKE READY:" mr-x mx-dscr[1] space(2) mr-y mx-dscr[2] skip
  "   RUN SPEED:" rs-x mx-dscr[3] space(2) rs-y mx-dscr[4] skip
  "   RUN SPOIL:" sp-x sp-dscr[1] space(2) sp-y sp-dscr[2] skip
  /*************** CTS moved to menu option under Run ***********
  head      " Extra %       Board     Run Speed        "
  skip
  "     Over  Per Hr      Caliper   % Reduction        "
  skip
  mstd.run-qty[1] to 9 x-sheets[1] to 17 board-cal[1] to 30
    spd-reduc[1] to 41 skip
  mstd.run-qty[2] to 9 x-sheets[2] to 17 board-cal[2] to 30
    spd-reduc[2] to 41 skip
  mstd.run-qty[3] to 9 x-sheets[3] to 17 board-cal[3] to 30
    spd-reduc[3] to 41 skip
  mstd.run-qty[4] to 9 x-sheets[4] to 17 board-cal[4] to 30
    spd-reduc[4] to 41  skip
  mstd.run-qty[5] to 9 x-sheets[5] to 17 board-cal[5] to 30
    spd-reduc[5] to 41 skip
  mstd.run-qty[6] to 9 x-sheets[6] to 17 board-cal[6] to 30
    spd-reduc[6] to 41 skip
  mstd.run-qty[7] to 9 x-sheets[7] to 17 board-cal[7] to 30
    spd-reduc[7] to 41 skip
  mstd.run-qty[8] to 9 x-sheets[8] to 17 board-cal[8] to 30
    spd-reduc[8] to 41 skip
  mstd.run-qty[9] to 9 x-sheets[9] to 17 board-cal[9] to 30
    spd-reduc[9] to 41 skip
  ********************************************************/
  skip(1) /* was skip(11) CTS end */
  with frame mstd no-labels no-box no-underline row 3 overlay stream-io width 81.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

