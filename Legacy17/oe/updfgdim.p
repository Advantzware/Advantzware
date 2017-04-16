/* oe/updfgdim.p                                                       */
/* 2/23/2012 - wfk                                                     */
/* Includes updfgdim.i so as to avoid transaction scoping errors in    */
/* the calling program                                                 */

DEF INPUT PARAMETER ipr-eb AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-itemfg AS ROWID NO-UNDO.

DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEF BUFFER xeb FOR eb.
DEF BUFFER xitemfg FOR itemfg.

{custom/globdefs.i}
{sys/inc/var.i shared}


{sys/inc/f16to32.i} 
{ce/msfcalc.i}

FIND xeb WHERE rowid(xeb) = ipr-eb EXCLUSIVE-LOCK NO-ERROR.
FIND xitemfg WHERE ROWID(xitemfg) = ipr-itemfg EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAIL xeb THEN
     RETURN.

IF NOT AVAIL xitemfg THEN
  RETURN.
    
{sys/inc/updfgdim.i "xeb" "x"} 

