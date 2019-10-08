/* ----------------------------------------------- sys/ref/mach-1.p 11/99 fwk */
/* MACHINE FILE PRINTOUT                                                      */
/* -------------------------------------------------------------------------- */

def input param v-recid as recid no-undo.

find first mstd where recid(mstd) eq v-recid no-lock no-error.

  DISPLAY
  SKIP(1)
  "   BLANKS   EXTRA %      BOARD    RUN SPEED       BOX       SPEED" skip
  "     OVER    PER HR    CALIPER  % REDUCTION     DEPTH   REDUCTION" skip
  mstd.run-qty[1] to 9 when mstd.run-qty[1] ne 0
  x-sheets[1] to 19 when x-sheets[1] ne 0
  board-cal[1] to 30 when board-cal[1] ne 0
  spd-reduc[1] to 43 when spd-reduc[1] ne 0
  mstd.board-depth[1] to 53 when mstd.board-depth[1] ne 0
  mstd.depth-reduc[1] to 65 when mstd.depth-reduc[1] ne 0 skip
  mstd.run-qty[2] to 9 when mstd.run-qty[2] ne 0
  x-sheets[2] to 19 when x-sheets[2] ne 0
  board-cal[2] to 30 when board-cal[2] ne 0
  spd-reduc[2] to 43 when spd-reduc[2] ne 0
  mstd.board-depth[2] to 53 when mstd.board-depth[2] ne 0
  mstd.depth-reduc[2] to 65 when mstd.depth-reduc[2] ne 0 skip
  mstd.run-qty[3] to 9 when mstd.run-qty[3] ne 0
  x-sheets[3] to 19 when x-sheets[3] ne 0
  board-cal[3] to 30 when board-cal[3] ne 0
  spd-reduc[3] to 43 when spd-reduc[3] ne 0
  mstd.board-depth[3] to 53 when mstd.board-depth[3] ne 0
  mstd.depth-reduc[3] to 65 when mstd.depth-reduc[3] ne 0 skip
  mstd.run-qty[4] to 9 when mstd.run-qty[4] ne 0
  x-sheets[4] to 19 when x-sheets[4] ne 0
  board-cal[4] to 30 when board-cal[4] ne 0
  spd-reduc[4] to 43 when spd-reduc[4] ne 0
  mstd.board-depth[4] to 53 when mstd.board-depth[4] ne 0
  mstd.depth-reduc[4] to 65 when mstd.depth-reduc[4] ne 0 skip
  mstd.run-qty[5] to 9 when mstd.run-qty[5] ne 0
  x-sheets[5] to 19 when x-sheets[5] ne 0
  board-cal[5] to 30 when board-cal[5] ne 0
  spd-reduc[5] to 43 when spd-reduc[5] ne 0
  mstd.board-depth[5] to 53 when mstd.board-depth[5] ne 0
  mstd.depth-reduc[5] to 65 when mstd.depth-reduc[5] ne 0 skip
  mstd.run-qty[6] to 9 when mstd.run-qty[6] ne 0
  x-sheets[6] to 19 when x-sheets[6] ne 0
  board-cal[6] to 30 when board-cal[6] ne 0
  spd-reduc[6] to 43 when spd-reduc[6] ne 0
  mstd.board-depth[6] to 53 when mstd.board-depth[6] ne 0
  mstd.depth-reduc[6] to 65 when mstd.depth-reduc[6] ne 0 skip
  mstd.run-qty[7] to 9 when mstd.run-qty[7] ne 0
  x-sheets[7] to 19 when x-sheets[7] ne 0
  board-cal[7] to 30 when board-cal[7] ne 0
  spd-reduc[7] to 43 when spd-reduc[7] ne 0
  mstd.board-depth[7] to 53 when mstd.board-depth[7] ne 0
  mstd.depth-reduc[7] to 65 when mstd.depth-reduc[7] ne 0 skip
  mstd.run-qty[8] to 9 when mstd.run-qty[8] ne 0
  x-sheets[8] to 19 when x-sheets[8] ne 0
  board-cal[8] to 30 when board-cal[8] ne 0
  spd-reduc[8] to 43 when spd-reduc[8] ne 0
  mstd.board-depth[8] to 53 when mstd.board-depth[8] ne 0
  mstd.depth-reduc[8] to 65 when mstd.depth-reduc[8] ne 0 skip
  mstd.run-qty[9] to 9 when mstd.run-qty[9] ne 0
  x-sheets[9] to 19 when x-sheets[9] ne 0
  board-cal[9] to 30 when board-cal[9] ne 0
  spd-reduc[9] to 43 when spd-reduc[9] ne 0
  mstd.board-depth[9] to 53 when mstd.board-depth[9] ne 0
  mstd.depth-reduc[9] to 65 when mstd.depth-reduc[9] ne 0 skip
  with frame mstd-frm no-labels stream-io no-box.

  DISPLAY
  mstd.board-depth[10] to 53 when mstd.board-depth[10] ne 0
  mstd.depth-reduc[10] to 65 when mstd.depth-reduc[10] ne 0 skip
  mstd.board-depth[11] to 53 when mstd.board-depth[11] ne 0
  mstd.depth-reduc[11] to 65 when mstd.depth-reduc[11] ne 0 skip
  mstd.board-depth[12] to 53 when mstd.board-depth[12] ne 0
  mstd.depth-reduc[12] to 65 when mstd.depth-reduc[12] ne 0 skip
  mstd.board-depth[13] to 53 when mstd.board-depth[13] ne 0
  mstd.depth-reduc[13] to 65 when mstd.depth-reduc[13] ne 0 skip
  mstd.board-depth[14] to 53 when mstd.board-depth[14] ne 0
  mstd.depth-reduc[14] to 65 when mstd.depth-reduc[14] ne 0 skip
  mstd.board-depth[15] to 53 when mstd.board-depth[15] ne 0
  mstd.depth-reduc[15] to 65 when mstd.depth-reduc[15] ne 0 skip
  mstd.board-depth[16] to 53 when mstd.board-depth[16] ne 0
  mstd.depth-reduc[16] to 65 when mstd.depth-reduc[16] ne 0 skip
  mstd.board-depth[17] to 53 when mstd.board-depth[17] ne 0
  mstd.depth-reduc[17] to 65 when mstd.depth-reduc[17] ne 0 skip
  with frame mstd-frm1 no-labels stream-io no-box.


/* END --------------------------------- copr. 1992  Advanced Software, Inc. */



