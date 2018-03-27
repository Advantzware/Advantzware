DEFINE INPUT  PARAMETER iprRell AS ROWID       NO-UNDO.

DEF VAR v-tot-blank-bins AS DEC NO-UNDO.
DEF BUFFER b-fg-bin FOR fg-bin.


FIND FIRST oe-rell WHERE ROWID(oe-rell) EQ iprRell NO-LOCK NO-ERROR.

  IF avail(oe-rell) AND oe-rell.s-code = "S" THEN DO:
    FIND FIRST oe-boll WHERE oe-boll.company EQ oe-rell.company
      AND oe-boll.ord-no EQ oe-rell.ord-no
      AND oe-boll.i-no   EQ oe-rell.i-no
      AND oe-boll.tag    EQ oe-rell.tag
      AND oe-boll.r-no   EQ oe-rell.r-no
      AND oe-boll.job-no EQ oe-rell.job-no
      AND oe-boll.job-no2 EQ oe-rell.job-no2
      AND oe-boll.po-no  EQ oe-rell.po-no
      NO-LOCK NO-ERROR.
    
    /* If the release is ship-only and we're posting the BOL,
       clean up the fg-bin records that are placeholders, total the + and - */
    IF AVAIL oe-rell AND AVAIL oe-boll THEN DO:

        v-tot-blank-bins = 0.
        FOR EACH fg-bin WHERE fg-bin.company EQ oe-rell.company
            AND fg-bin.i-no EQ oe-rell.i-no
            /* AND fg-bin.loc  EQ oe-rell.loc */
            AND fg-bin.loc-bin EQ ""
            AND fg-bin.tag EQ "" NO-LOCK.
            v-tot-blank-bins = v-tot-blank-bins + fg-bin.qty.
            
        END.
        
        IF v-tot-blank-bins EQ 0 THEN DO:
          
            FOR EACH fg-bin WHERE fg-bin.company EQ oe-rell.company
                AND fg-bin.i-no EQ oe-rell.i-no
                /* AND fg-bin.loc  EQ oe-rell.loc */
                AND fg-bin.loc-bin EQ ""
                AND fg-bin.tag EQ "" EXCLUSIVE-LOCK.
                
                DELETE fg-bin.
            END. /* each fg-bin */
    
        END. /* if total blank bins = 0 */
        ELSE DO:
          /* May be shipping less than the cust owned qty so reduce cust owned */
            FIND FIRST fg-bin WHERE fg-bin.company EQ oe-rell.company
                AND fg-bin.i-no EQ oe-rell.i-no
                AND fg-bin.loc-bin EQ ""
                AND fg-bin.tag EQ ""
                AND fg-bin.cust-no GT ""
                /* AND fg-bin.qty GT 0 */ EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL fg-bin THEN
              FIND FIRST b-fg-bin WHERE b-fg-bin.company EQ oe-rell.company
                  AND b-fg-bin.i-no EQ oe-rell.i-no
                  AND b-fg-bin.loc-bin EQ ""
                  AND b-fg-bin.tag EQ ""
                  AND b-fg-bin.cust-no EQ ""
                  AND b-fg-bin.qty LE 0
                  /* AND ABS(b-fg-bin.qty) LT abs(fg-bin.qty) */
              EXCLUSIVE-LOCK NO-ERROR.
            

            IF AVAIL b-fg-bin AND AVAIL(fg-bin) THEN DO:
              
              IF b-fg-bin.qty NE 0 THEN
                 b-fg-bin.qty = b-fg-bin.qty + oe-boll.qty.

              IF b-fg-bin.qty EQ 0 THEN
               DELETE b-fg-bin.
              ELSE DO:
                IF b-fg-bin.qty + oe-boll.qty EQ 0 THEN 
                  DELETE b-fg-bin.
              END.

              IF fg-bin.qty EQ 0 THEN
                 DELETE fg-bin.
            END. /* If bins are found to fix */

        END. /* If total is not zero */
    END. /* avail oe-rell */

END. /* if s-code is 'S' */
