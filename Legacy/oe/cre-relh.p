/* ---------------------------------------------------- oe/cre-relh 01/98 JLF */
/* Order Entry - Create actual releases from planned release line (header)    */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-recid AS RECID.

/* gdm - 02020902 */
DEF VAR v-chkflg AS LOG NO-UNDO.
DEF BUFFER bf-cust FOR cust.

{sys/inc/var.i shared}

DO TRANSACTION:
   {sys/inc/addxfer.i}
END.

DEF SHARED VAR out-recid  AS RECID NO-UNDO.
DEF SHARED VAR relh-recid AS RECID NO-UNDO.

DEF VAR v-nxt-r-no    AS INT INIT 1 NO-UNDO.
DEF VAR v-nxt-release AS INT        NO-UNDO.

/* gdm - 10260903*/
DEF VAR v-origprgm    AS CHAR FORMAT "x(50)" NO-UNDO.
ASSIGN v-origprgm = TRIM(PROGRAM-NAME(2)).

FIND oe-rel WHERE RECID(oe-rel) EQ v-recid NO-LOCK.

/* gdm - 02020902 */
ASSIGN v-chkflg = NO.

FIND FIRST sys-ctrl NO-LOCK
  WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld 
  THEN 
   FIND FIRST cust NO-LOCK
     WHERE cust.company EQ cocode
       AND cust.cust-no EQ oe-rel.cust-no NO-ERROR.
   IF AVAIL cust THEN 
     /* gdm - 10260903*/
     IF v-origprgm NE "fg/invrecpt.p" 
       THEN RUN oe/CRcheck.p (INPUT ROWID(cust),
                              INPUT YES,
                              OUTPUT v-chkflg).

/* FIND LAST oe-relh USE-INDEX r-no NO-LOCK NO-ERROR.      */
/* v-nxt-r-no = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0. */
RUN oe/getNextRelNo.p (INPUT "oe-relh", OUTPUT v-nxt-r-no).

RUN oe/release#.p (cocode, OUTPUT v-nxt-release).

/* gdm - 06220907 
RUN oe/reltckt#.p(INPUT cocode,
                  OUTPUT v-nxt-r-no,
                  OUTPUT v-nxt-release).
 gdm - 06220907 */                  


CREATE oe-relh.

ASSIGN oe-relh.cust-no = oe-rel.cust-no.


IF addxfer-log = YES  AND oe-rel.s-code EQ 'T' 
  THEN DO:

   FIND FIRST cust 
     WHERE cust.company EQ cocode 
       AND cust.active EQ 'X' NO-LOCK NO-ERROR.
   IF AVAIL cust THEN DO:

     IF CAN-FIND(FIRST shipto 
                   WHERE shipto.company EQ cocode 
                     AND shipto.cust-no EQ cust.cust-no 
                     AND shipto.ship-no EQ oe-rel.ship-no 
                     AND shipto.ship-id EQ oe-rel.ship-id) 
       THEN ASSIGN oe-relh.cust-no = cust.cust-no.

     RELEASE cust.
   END.
END.

ASSIGN out-recid         = recid(oe-relh)
       relh-recid        = recid(oe-relh)
       oe-relh.r-no      = v-nxt-r-no
       oe-relh.company   = oe-rel.company
       oe-relh.ship-no   = oe-rel.ship-no
       oe-relh.ship-id   = oe-rel.ship-id
       oe-relh.ship-i[1] = oe-rel.ship-i[1]
       oe-relh.ship-i[2] = oe-rel.ship-i[2]
       oe-relh.ship-i[3] = oe-rel.ship-i[3]
       oe-relh.ship-i[4] = oe-rel.ship-i[4]
       oe-relh.carrier   = oe-rel.carrier
       oe-relh.printed   = no
       oe-relh.posted    = no
       oe-relh.deleted   = no
       oe-relh.rel-date  = oe-rel.rel-date
       oe-relh.release#  = v-nxt-release
       oe-relh.user-id   = USERID("nosweat")
       oe-relh.upd-time  = TIME
       oe-relh.upd-date  = TODAY
       oe-relh.w-ord = v-chkflg.
       
RUN CopyShipNote (oe-rel.rec_key, oe-relh.rec_key).

IF v-chkflg THEN
DO:
   FIND FIRST bf-cust 
     WHERE bf-cust.company EQ cocode
       AND bf-cust.cust-no EQ oe-rel.cust-no USE-INDEX cust 
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF AVAIL bf-cust THEN
      ASSIGN bf-cust.cr-hold = YES.
END.
/* gdm - 06220907 end */

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
