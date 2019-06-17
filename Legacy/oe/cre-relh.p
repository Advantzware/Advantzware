/* ---------------------------------------------------- oe/cre-relh 01/98 JLF */
/* Order Entry - Create actual releases from planned release line (header)    */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-recid AS RECID.

DEFINE SHARED VARIABLE out-recid     AS RECID     NO-UNDO.
DEFINE SHARED VARIABLE relh-recid    AS RECID     NO-UNDO.

DEFINE VARIABLE iNextRNo        AS INTEGER   INIT 1 NO-UNDO.
DEFINE VARIABLE iNextReleaseNum AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOrigProgram    AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE v-chkflg        AS LOG       NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

{sys/inc/var.i shared}

DO TRANSACTION:
    {sys/inc/addxfer.i}
END.

ASSIGN 
    cOrigProgram = TRIM(PROGRAM-NAME(2))
    v-chkflg = NO
    .

FIND oe-rel NO-LOCK WHERE RECID(oe-rel) EQ v-recid.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "RELCREDT" 
    NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld 
    THEN 
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ oe-rel.cust-no 
        NO-ERROR.
IF AVAILABLE cust THEN      
    IF cOrigProgram NE "fg/invrecpt.p" 
        THEN RUN oe/CRcheck.p (INPUT ROWID(cust),
            INPUT YES,
            OUTPUT v-chkflg).

RUN oe/getNextRelNo.p (INPUT "oe-relh", OUTPUT iNextRNo).

RUN oe/release#.p (cocode, OUTPUT iNextReleaseNum).

CREATE oe-relh.

ASSIGN 
    oe-relh.cust-no = oe-rel.cust-no
    .

IF addxfer-log = YES  AND oe-rel.s-code EQ 'T' 
    THEN 
DO:
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cocode 
          AND cust.active EQ 'X'  
        NO-ERROR.
    IF AVAILABLE cust THEN 
    DO:
        IF CAN-FIND(FIRST shipto 
            WHERE shipto.company EQ cocode 
              AND shipto.cust-no EQ cust.cust-no 
              AND shipto.ship-no EQ oe-rel.ship-no 
              AND shipto.ship-id EQ oe-rel.ship-id) 
            THEN ASSIGN oe-relh.cust-no = cust.cust-no.

        RELEASE cust.
    END.
END.

ASSIGN 
    out-recid         = RECID(oe-relh)
    relh-recid        = RECID(oe-relh)
    oe-relh.r-no      = iNextRNo
    oe-relh.company   = oe-rel.company
    oe-relh.ship-no   = oe-rel.ship-no
    oe-relh.ship-id   = oe-rel.ship-id
    oe-relh.ship-i[1] = oe-rel.ship-i[1]
    oe-relh.ship-i[2] = oe-rel.ship-i[2]
    oe-relh.ship-i[3] = oe-rel.ship-i[3]
    oe-relh.ship-i[4] = oe-rel.ship-i[4]
    oe-relh.carrier   = oe-rel.carrier
    oe-relh.printed   = NO
    oe-relh.posted    = NO
    oe-relh.deleted   = NO
    oe-relh.rel-date  = oe-rel.rel-date
    oe-relh.release#  = iNextReleaseNum
    oe-relh.user-id   = USERID("nosweat")
    oe-relh.upd-time  = TIME
    oe-relh.upd-date  = TODAY
    oe-relh.w-ord     = v-chkflg
    .
       
RUN CopyShipNote (oe-rel.rec_key, oe-relh.rec_key).

IF v-chkflg THEN
DO:
    FIND FIRST bf-cust EXCLUSIVE-LOCK
        WHERE bf-cust.company EQ cocode
          AND bf-cust.cust-no EQ oe-rel.cust-no USE-INDEX cust 
         NO-WAIT NO-ERROR.
    IF AVAILABLE bf-cust THEN
        ASSIGN bf-cust.cr-hold = YES
            .
END.

PROCEDURE CopyShipNote PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Copies Ship Note from rec_key to rec_key
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.


/* end ---------------------------------- copr. 1998  advanced software, inc. */
