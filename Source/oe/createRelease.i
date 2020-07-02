/* createRelease.i - shared by oe/impord.w & cXML/monitor.w */

PROCEDURE createRelease:
    DEFINE INPUT PARAMETER ipcShipTo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipFrom AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dtDateRule AS DATE    NO-UNDO.
    DEFINE VARIABLE iNextRelNo AS INTEGER NO-UNDO.

    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ cocode
           AND sys-ctrl.name    EQ "OECARIER"
         NO-ERROR.

    FIND FIRST shipto NO-LOCK
         WHERE shipto.company EQ oe-ord.company
           AND shipto.cust-no EQ oe-ord.cust-no
           AND shipto.ship-id EQ ipcShipTo
         NO-ERROR.
    IF NOT AVAILABLE shipto THEN
    FIND FIRST shipto NO-LOCK
         WHERE shipto.company EQ cocode
           AND shipto.cust-no EQ oe-ord.cust-no
         NO-ERROR.

    FIND FIRST bf-oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.
    /* 10051225 */
    /* iNextRelNo = (if avail bf-oe-rel then bf-oe-rel.r-no else 0) + 1. */
    RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT iNextRelNo).
    CREATE oe-rel.
    ASSIGN 
        oe-rel.company   = cocode
        oe-rel.loc       = locode
        oe-rel.ord-no    = oe-ordl.ord-no
        oe-rel.i-no      = oe-ordl.i-no
        oe-rel.cust-no   = oe-ord.cust-no
        oe-rel.po-no     = IF oe-ordl.po-no NE "" THEN oe-ordl.po-no ELSE oe-ord.po-no
        oe-rel.qty       = oe-ordl.qty /*- v-qty-sum */
        oe-rel.tot-qty   = oe-ordl.qty
        oe-rel.line      = oe-ordl.line
        oe-rel.s-comm[1] = oe-ord.s-comm[1]
        oe-rel.s-comm[2] = oe-ord.s-comm[2]
        oe-rel.s-comm[3] = oe-ord.s-comm[3]
        oe-rel.s-name[1] = oe-ord.sname[1]
        oe-rel.s-name[2] = oe-ord.sname[2]
        oe-rel.s-name[3] = oe-ord.sname[3]
        oe-rel.s-pct[1]  = oe-ord.s-pct[1]
        oe-rel.s-pct[2]  = oe-ord.s-pct[2]
        oe-rel.s-pct[3]  = oe-ord.s-pct[3]
        oe-rel.sman[1]   = oe-ord.sman[1]
        oe-rel.sman[2]   = oe-ord.sman[2]
        oe-rel.sman[3]   = oe-ord.sman[3]
        oe-rel.sold-no   = oe-ord.sold-no
        oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" AND AVAILABLE shipto THEN shipto.carrier
                             ELSE oe-ord.carrier
        oe-rel.r-no      = iNextRelNo
        oe-rel.frt-pay   = SUBSTRING(oe-ord.frt-pay,1,1)
        oe-rel.fob-code  = oe-ord.fob-code
        .

    IF oereleas-cha EQ "LastShip" THEN
        oe-rel.rel-date = oe-ord.last-date.
    ELSE IF oereleas-cha EQ "Due Date" THEN
            oe-rel.rel-date = oe-ordl.req-date.
        ELSE /*DueDate+1Day*/ DO:
            RUN spCommon_DateRule (
                ?,
                "_ANY_",
                "_ANY_",
                ROWID(oe-ordl),
                ?,
                OUTPUT dtDateRule
                ).
            oe-rel.rel-date = dtDateRule.
/*            oe-rel.rel-date = oe-ordl.req-date + 1.       */
/*            IF WEEKDAY(oe-rel.rel-date) EQ 7 THEN         */
/*                oe-rel.rel-date = oe-rel.rel-date + 2.    */
/*            ELSE IF WEEKDAY(oe-rel.rel-date) EQ 1 THEN    */
/*                    oe-rel.rel-date = oe-rel.rel-date + 1.*/
        END.
    IF AVAILABLE shipto THEN DO:
        ASSIGN 
            oe-rel.ship-addr[1] = shipto.ship-addr[1]
            oe-rel.ship-city    = shipto.ship-city
            oe-rel.ship-state   = shipto.ship-state
            oe-rel.ship-zip     = shipto.ship-zip
            oe-rel.ship-no      = shipto.ship-no
            oe-rel.ship-id      = shipto.ship-id
            oe-rel.ship-i[1]    = shipto.notes[1]
            oe-rel.ship-i[2]    = shipto.notes[2]
            oe-rel.ship-i[3]    = shipto.notes[3]
            oe-rel.ship-i[4]    = shipto.notes[4]
            oe-rel.spare-char-1 = IF ipcShipFrom NE "" THEN ipcShipFrom
                                  ELSE shipto.loc
            .
        RUN CopyShipNote (shipto.rec_key, oe-rel.rec_key).
    END.
    ELSE
    ASSIGN
        oe-rel.ship-no      = oe-ord.sold-no
        oe-rel.ship-id      = oe-ord.sold-id
        oe-rel.ship-i[1]    = oe-ord.ship-i[1]
        oe-rel.ship-i[2]    = oe-ord.ship-i[2]
        oe-rel.ship-i[3]    = oe-ord.ship-i[3]
        oe-rel.ship-i[4]    = oe-ord.ship-i[4]
        oe-rel.spare-char-1 = IF ipcShipFrom NE "" THEN ipcShipFrom
                              ELSE oe-ord.loc
        .
    /* Assign itemfg-loc values */
    RUN fg/fgitmloc.p (oe-rel.i-no, INPUT ROWID(oe-rel)).

END PROCEDURE.

PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyTo   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.
    


