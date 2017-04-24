/* {1} is oe-rel */
/* {2} is cust-no */
/* Include file dependencies:
      oe-rel record
   Outputs:
    oe-relh record
    lr-oe-relh-rowid
    ll-by-po
    l-rno
    ll-rell-found
*/
DEF BUFFER b-reft-findrelh FOR reftable.
DEF BUFFER b-reft-fob FOR reftable.
DEF BUFFER b2-oe-rell FOR oe-rell.
DEF BUFFER b2-reftable FOR reftable.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF VAR ll-rell-found AS LOG NO-UNDO.
DEF VAR ll-by-po      AS LOG NO-UNDO.
DEF VAR l-rno LIKE oe-rell.r-no NO-UNDO.
DEF VAR lr-oe-relh-rowid AS ROWID NO-UNDO.

DO TRANSACTION:
  {sys/inc/relmerge.i}
END.

RELEASE oe-relh.

/* Get reftable record for this r-no for an s-code to match against oe-rell.s-code */
/* compare the oe-rel.s-code with the oe-rell.s-code                               */
FIND FIRST b-reft-findrelh
    WHERE b-reft-findrelh.reftable EQ "oe-rel.s-code"
      AND b-reft-findrelh.company  EQ STRING({1}.r-no,"9999999999")
    NO-LOCK NO-ERROR.

/* Get reftable record for this r-no for an dscr to match against another  */
/* reftable record related to lot# (compare the oe-rel.lot with oe-rell   */
FIND FIRST b-reft-fob WHERE
     b-reft-fob.reftable EQ "oe-rel.lot-no" AND
     b-reft-fob.company  EQ STRING({1}.r-no,"9999999999")
     NO-LOCK NO-ERROR.

/* Set ll-by-po */
ll-by-po = NO.
IF can-find(FIRST sys-ctrl-shipto
                    WHERE sys-ctrl-shipto.company = {1}.company
                      AND sys-ctrl-shipto.NAME = "RelMerge"
                      AND sys-ctrl-shipto.cust-vend = YES
                      AND sys-ctrl-shipto.char-fld  BEGINS "SamePo#Only"
                      AND sys-ctrl-shipto.cust-vend-no = {2}) THEN
  ll-by-po = YES.
                
/* set ll-rec-found based on whether able to match to existing oe-rell */
/* l-rno is the existing oe-rell.r-no                                  */
ll-rell-found = NO.
FOR EACH b-oe-rel WHERE b-oe-rel.company = oe-rel.company
                     AND b-oe-rel.po-no EQ oe-rel.po-no
                     AND b-oe-rel.r-no NE oe-rel.r-no
                     AND b-oe-rel.rel-date EQ oe-rel.rel-date
                     AND b-oe-rel.cust-no EQ oe-rel.cust-no
                  NO-LOCK,
    EACH b2-oe-rell WHERE b2-oe-rell.company = b-oe-rel.company
                       AND b2-oe-rell.ord-no = b-oe-rel.ord-no
                       AND b2-oe-rell.i-no   = b-oe-rel.i-no
                     NO-LOCK,
    EACH oe-relh WHERE  oe-relh.company  EQ {1}.company
	                AND oe-relh.rel-date EQ {1}.rel-date
                    AND oe-relh.cust-no  EQ {2}
	                AND oe-relh.ship-id  EQ {1}.ship-id                    
	                AND oe-relh.posted   EQ NO
	                AND oe-relh.deleted  EQ NO
	                AND (oe-relh.printed EQ NO OR relmerge-log) 
                    AND oe-relh.r-no = b2-oe-rell.r-no
                 NO-LOCK:
    ASSIGN ll-rell-found = TRUE
           l-rno = b2-oe-rell.r-no.
    LEAVE.
END.

lr-oe-relh-rowid = ?.
oe-relh-loop:
FOR EACH oe-relh
    WHERE  oe-relh.company  EQ {1}.company
	  AND oe-relh.rel-date EQ {1}.rel-date
      AND oe-relh.cust-no  EQ {2}
	  AND oe-relh.ship-id  EQ {1}.ship-id
    
	  AND oe-relh.posted   EQ NO
	  AND oe-relh.deleted  EQ NO
	  AND (oe-relh.printed EQ NO OR relmerge-log) 
      /* Check against reftables s-code if one is found */
      AND (NOT AVAIL b-reft-findrelh OR
           CAN-FIND(FIRST oe-rell
                    WHERE oe-rell.r-no   EQ oe-relh.r-no
                      AND oe-rell.s-code EQ b-reft-findrelh.code))
      /* same order only logic, ll-by-po must pass this check also */
      AND ((relmerge-chr NE "SameOrderOnly" OR ll-by-po) OR
           CAN-FIND(FIRST oe-rell
                    WHERE oe-rell.r-no   EQ oe-relh.r-no
                      AND oe-rell.ord-no EQ {1}.ord-no))
      /* separate check for samePO#Only, special case where */
      /* samePO#only is only used for specific customers */
      /* see task 08261102 */
      AND ((NOT CAN-FIND(FIRST sys-ctrl-shipto
                    WHERE sys-ctrl-shipto.company = {1}.company
                      AND sys-ctrl-shipto.NAME = "RelMerge"
                      AND sys-ctrl-shipto.cust-vend = YES
                      AND sys-ctrl-shipto.char-fld  BEGINS "SamePo#Only"
                      AND sys-ctrl-shipto.cust-vend-no = oe-relh.cust-no))
                      
            OR /* sys-ctrl-shipto not found or oe-relh.r-no same as */ 
               /* found oe-rell.r-no                                */
            (ll-rell-found AND oe-relh.r-no = l-rno))  

    USE-INDEX rel-date NO-LOCK
    BY oe-relh.printed
    BY oe-relh.r-no:
    
    /* Reject this oe-relh if the lot number check is not passed */
    IF AVAIL b-reft-fob THEN
    DO:
       FOR EACH b2-oe-rell FIELDS(rec_key) WHERE
           b2-oe-rell.r-no EQ oe-relh.r-no
           NO-LOCK:
           
         /* Match of lot-no between oe-rel and oe-rell */
           FIND FIRST b2-reftable WHERE
                b2-reftable.reftable EQ "oe-rell.lot-no" AND
                b2-reftable.rec_key  EQ b2-oe-rell.rec_key
                USE-INDEX rec_key
                NO-LOCK NO-ERROR.
           
           IF (AVAIL b2-reftable AND b2-reftable.dscr NE b-reft-fob.dscr) OR
              (NOT AVAIL b2-reftable AND b-reft-fob.dscr NE "") THEN
              NEXT oe-relh-loop.
       END.
    END.

    lr-oe-relh-rowid = ROWID(oe-relh).
    
    LEAVE.
END.

/* Return correct buffer if found */
IF lr-oe-relh-rowid <> ?  THEN
    FIND oe-relh WHERE rowid(oe-relh) = lr-oe-relh-rowid NO-ERROR.
