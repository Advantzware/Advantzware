/* ------------------------------------------------------- oe/menu.f  7/94 RM */
/* Order Processing Sub-Menu Program Form Statement - O/E Module              */
/* -------------------------------------------------------------------------- */

form skip(1)
     cmnd[1] skip
     cmnd[2] skip
     cmnd[3] skip
     cmnd[4] skip
     cmnd[5] skip
     cmnd[6] skip
     cmnd[7] skip
     cmnd[8] skip
     cmnd[9] skip(1)
     with title color value(col-warn) "    EDI PROCESSING MENU   "
     frame f-cmd2 row 3 no-labels  width 32 overlay centered
     color value(col-norm) prompt value(col-mess).
