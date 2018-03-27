/* Tests if order should be closed given a bol number */
DEF INPUT PARAMETER ipcCompany AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipiBolNo LIKE fg-rctd.bol-no NO-UNDO.
DEF INPUT PARAMETER ipcI-no  LIKE fg-rctd.i-no  NO-UNDO.
DEF VAR v-close-line AS LOG NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cReason AS CHARACTER   NO-UNDO.

{oe/closchk.i new}

FIND FIRST oe-boll 
 WHERE oe-boll.company EQ ipcCompany
   AND oe-boll.bol-no  EQ ipiBolNo
   AND oe-boll.i-no    EQ ipcI-no
 NO-LOCK NO-ERROR.
IF AVAIL oe-boll THEN
   FIND FIRST oe-ordl 
     WHERE oe-ordl.company EQ oe-boll.company
       AND oe-ordl.ord-no  EQ oe-boll.ord-no
       AND oe-ordl.i-no    EQ oe-boll.i-no
       AND oe-ordl.LINE    EQ oe-boll.LINE
   NO-LOCK NO-ERROR.
IF AVAIL oe-ordl THEN DO:
  
   RUN oe/CloseOrder(INPUT ROWID(oe-ordl),
                     INPUT NO,
                     OUTPUT cStatus,
                     OUTPUT cReason).
/*                RUN oe/clslnchkinv.p (BUFFER oe-ordl, OUTPUT v-close-line). */
/*                IF v-close-line THEN */
    IF cStatus EQ 'C' THEN     
        RUN oe/closelin.p (INPUT ROWID(oe-ordl),YES).
   FIND oe-ord 
     WHERE oe-ord.company EQ oe-ordl.company
       AND oe-ord.ord-no  EQ oe-ordl.ord-no
     NO-LOCK NO-ERROR.
   IF AVAIL oe-ord THEN DO:
      IF NOT CAN-FIND(FIRST oe-ordl WHERE 
         oe-ordl.company = oe-ord.company AND
         oe-ordl.ord-no = oe-ord.ord-no AND 
         oe-ordl.stat NE "C") THEN
        RUN oe\close.p(RECID(oe-ord), YES).
   END.

END.

