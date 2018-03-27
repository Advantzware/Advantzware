DEF PARAM BUFFER io-po-ordl FOR po-ordl.
DEF PARAM BUFFER io-ap-invl FOR ap-invl.

DEF BUFFER b-po-ord FOR po-ord.
DEF VAR v-negative-receipt AS LOG NO-UNDO.
DEF VAR v-po-no AS CHAR NO-UNDO.

IF AVAIL io-po-ordl AND io-po-ordl.t-rec-qty EQ 0 AND
   NOT(io-po-ordl.item-type AND
       CAN-FIND(FIRST item
                WHERE item.company EQ io-po-ordl.company
                  AND item.i-no    EQ io-po-ordl.i-no
                  AND item.i-code  EQ "R"
                  AND item.stocked EQ NO)) THEN
DO:
   v-po-no = STRING(io-po-ordl.po-no).

   FOR EACH rm-rcpth WHERE
       rm-rcpth.company EQ io-po-ordl.company AND
       rm-rcpth.po-no EQ v-po-no AND
       rm-rcpth.rita-code EQ "R"
       NO-LOCK,
       EACH rm-rdtlh WHERE
            rm-rdtlh.r-no EQ rm-rcpth.r-no AND
            rm-rdtlh.rita-code EQ rm-rcpth.rita-code AND
            rm-rdtlh.qty LT 0
            NO-LOCK:
   
       v-negative-receipt = YES.
       LEAVE.
   END.
   
   IF v-negative-receipt = NO THEN
   DO:
      FIND FIRST b-po-ord WHERE
           b-po-ord.company EQ io-po-ordl.company AND
           b-po-ord.po-no EQ io-po-ordl.po-no
           NO-LOCK NO-ERROR.
     
      IF AVAIL b-po-ord THEN
         FOR EACH fg-rcpth WHERE
             fg-rcpth.company EQ io-po-ordl.company AND
             fg-rcpth.vend-no EQ b-po-ord.vend-no AND
             fg-rcpth.po-no EQ v-po-no AND
             LOOKUP(fg-rcpth.rita-code,"R,E") > 0
             NO-LOCK,
             EACH fg-rdtlh WHERE
                  fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                  fg-rdtlh.rita-code EQ fg-rcpth.rita-code AND
                  fg-rdtlh.qty LT 0
                  NO-LOCK:
            
             v-negative-receipt = YES.
             LEAVE.
         END.
      END.
END.

IF AVAIL io-po-ordl                                             AND
   (NOT AVAIL io-ap-invl OR
    NOT CAN-FIND(FIRST ap-invl
                 WHERE ap-invl.i-no       EQ io-ap-invl.i-no
                   AND ap-invl.po-no      EQ io-po-ordl.po-no
                   AND {ap/invlline.i -1} EQ io-po-ordl.line
                   AND ROWID(ap-invl)     NE ROWID(io-ap-invl)
                 USE-INDEX i-no))                               AND
                               
  io-po-ordl.stat      NE "X"   /* not deleted or cancelled */  AND            
  io-po-ordl.stat      NE "F"   /* not deleted or cancelled */  AND            
  (io-po-ordl.t-rec-qty NE 0 OR v-negative-receipt OR
   (io-po-ordl.item-type AND
    CAN-FIND(FIRST item
             WHERE item.company EQ io-po-ordl.company
               AND item.i-no    EQ io-po-ordl.i-no
               AND item.i-code  EQ "R"
               AND item.stocked EQ NO
             USE-INDEX i-no)))                                  THEN.
ELSE RELEASE io-po-ordl.
