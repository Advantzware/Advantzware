/* -------------------------------------------------- fg/ce-addfg.p 08/98 JLF */
/* Add FG thru estimating                                                     */
/* -------------------------------------------------------------------------- */

def input parameter v-item like itemfg.i-no.

{sys/inc/var.i shared}

def shared buffer xest    for est.
def shared buffer xef     for ef.
def shared buffer xeb     for eb.

DEF VAR li AS INT NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGMasterLoc AS CHARACTER NO-UNDO.

{ce/msfcalc.i}
{oe/fgfreight.i}    
{sys/inc/f16to32.i}
{sys/inc/fgmaster.i}
       
DO TRANSACTION:
   {sys/inc/graphic.i}
END.
RUN sys\ref\nk1look.p (cocode,
        "FGMasterLoc",
        "C",
        NO,
        NO,
        "",
        "", 
        OUTPUT cReturn,
        OUTPUT lFound).
 IF lFound THEN      
 cFGMasterLoc = cReturn .

IF NOT AVAIL xeb THEN
    RETURN.
IF NOT AVAIL xest THEN
    RETURN.
IF v-item EQ "" THEN
    RETURN.

FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ v-item
                  NO-LOCK NO-ERROR.
IF AVAIL itemfg THEN
    RETURN.
find first cust  where cust.company eq cocode
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.

create itemfg.
assign
 itemfg.company           = cocode
 itemfg.loc               = locode
 itemfg.i-no              = v-item
 itemfg.i-name            = xeb.part-dscr1
 itemfg.part-dscr1        = xeb.part-dscr2
 itemfg.part-no           = xeb.part-no
 itemfg.cust-no           = xeb.cust-no
 itemfg.cust-name         = if avail cust then cust.name else ""
 itemfg.die-no            = xeb.die-no
 itemfg.plate-no          = xeb.plate-no
 itemfg.style             = xeb.style
 itemfg.procat            = xeb.procat
 itemfg.cad-no            = xeb.cad-no
 itemfg.upc-no            = xeb.upc-no
 itemfg.spc-no            = xeb.spc-no
 itemfg.isaset            = (xest.est-type eq 2 or xest.est-type eq 6) and
                            xeb.form-no eq 0 
 itemfg.pur-man           = xeb.pur-man     
 itemfg.alloc             = NOT xeb.set-is-assembled
 itemfg.setupDate         = TODAY
 itemfg.receiveAsRMItemID = xeb.receiveAsRMItemID
 .
  
 RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").

 {oe/fgfreighta.i xeb}
 /* gdm - 11190901 */
 IF xeb.ship-id NE "" THEN DO:
  FIND FIRST shipto no-lock
    WHERE shipto.company EQ xeb.company
      AND shipto.cust-no EQ xeb.cust-no
      AND shipto.ship-id EQ xeb.ship-id NO-ERROR.
  IF AVAIL shipto THEN ASSIGN itemfg.ship-meth = shipto.ship-meth.
  IF cFGMasterLoc EQ "Estimate Shipto" AND AVAIL shipto THEN
   ASSIGN
       itemfg.def-loc     = shipto.loc
       itemfg.def-loc-bin = shipto.loc-bin.   
 END.
 /* gdm - 11190901 end */

IF xeb.form-no EQ 0 THEN
  itemfg.pur-man = NOT CAN-FIND(FIRST eb
                                WHERE eb.company EQ xeb.company 
                                  AND eb.est-no  EQ xeb.est-no
                                  AND eb.form-no NE 0
                                  AND eb.pur-man EQ NO).

IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.

{fg/set-inks1.i itemfg xeb}
 
{sys/inc/fgcascnt.i itemfg xeb}

{sys/inc/updfgdim.i "xeb"}
