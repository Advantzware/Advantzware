/* ---------------------------------------------------- oe/actrel.i 01/98 JLF */
/* order entry - Create actual releases from planned release line             */
/* -------------------------------------------------------------------------- */

DEFINE VARIABLE v-nxt-r-no AS INTEGER   INIT 1 NO-UNDO.
DEFINE VARIABLE v-dlg-sel  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-cust-no  AS CHARACTER NO-UNDO.


DO TRANSACTION:
  {sys\inc\addxfer.i}
END.

choice = YES.

IF NOT v-auto THEN
MESSAGE " This will create an Actual release from this Planned release. "
        SKIP
        " Would you like to continue? "
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.

IF choice THEN
rel-block:
REPEAT:
  /** Find the most recent actual release with the same #order, shipto,
  release date, not deleted, and not printed. **/
  REPEAT PRESELECT
    EACH oe-relh NO-LOCK
      WHERE oe-relh.company  EQ oe-rel.company
        /*             and oe-relh.ord-no   eq oe-rel.ord-no */
        AND oe-relh.rel-date EQ oe-rel.rel-date
        AND oe-relh.cust-no  EQ oe-rel.cust-no
        AND oe-relh.ship-id  EQ oe-rel.ship-id
        AND oe-relh.posted   EQ NO
        /*             and oe-relh.po-no    eq oe-rel.po-no */
        AND oe-relh.deleted  EQ NO
    USE-INDEX rel-date,
    
    EACH oe-rell NO-LOCK
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
        AND oe-rell.ord-no  EQ oe-rel.ord-no
        AND oe-rell.i-no    EQ oe-rel.i-no
        AND oe-rell.rel-no  EQ oe-rel.rel-no /* YSK added 01/20/03 - TASK 01170303*/
    :
    
    FIND NEXT oe-rell NO-ERROR.
    IF NOT avail oe-rell THEN LEAVE.
    IF avail oe-rell THEN DO:
      IF NOT v-auto /*or program-name(2) begins "oe/oe-relx." */ THEN DO:
        MESSAGE "This has been already been released. Can not release."
        VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END. /* Not V-auto */
    END. /* Avail Oe-rell */
  END. /* repeat preselect oe-relh */
  
  v-cust-no = oe-rel.cust-no.
  
  IF addxfer-log THEN
  DO:
    
      IF oe-rel.s-code EQ 'T' THEN
      DO:

        FIND FIRST cust WHERE
        cust.company EQ cocode AND
        cust.active EQ 'X'
        NO-LOCK NO-ERROR.
        
        IF AVAIL cust THEN
        DO:
          IF CAN-FIND(FIRST shipto WHERE
            shipto.company EQ cocode AND
            shipto.cust-no EQ cust.cust-no AND
            shipto.ship-no EQ oe-rel.ship-no AND
            shipto.ship-id EQ oe-rel.ship-id) THEN
          v-cust-no = cust.cust-no.
          
          RELEASE cust.
        END. /* Avail Cust */
      END. /* S-code = 'T' */
  END. /* If addxfer-log */
  
  
  /* Set v-merge-prompt value */
  v-merge-prompt = ?.
  /* Finds oe-relh based on merge criteria */
  {oe/findrelh.i oe-rel v-cust-no}
  IF AVAIL oe-relh THEN DO:
      
    IF ((NOT CAN-FIND(FIRST sys-ctrl-shipto
      WHERE sys-ctrl-shipto.company      = oe-relh.company
        AND sys-ctrl-shipto.NAME         = "RelMerge"
        AND sys-ctrl-shipto.cust-vend    = YES
        AND sys-ctrl-shipto.char-fld     = "SamePo#Only"
        AND sys-ctrl-shipto.cust-vend-no = oe-relh.cust-no))    
    OR
    (ll-rell-found AND oe-relh.r-no = l-rno)) THEN
      FIND FIRST sys-ctrl-shipto
        WHERE sys-ctrl-shipto.company        = oe-relh.company
          AND sys-ctrl-shipto.NAME           = "RelMerge"
          AND sys-ctrl-shipto.cust-vend      = YES
          AND sys-ctrl-shipto.char-fld  BEGINS "SamePo#Only"
          AND sys-ctrl-shipto.cust-vend-no   = oe-relh.cust-no
      NO-LOCK NO-ERROR.
    
    IF AVAIL(sys-ctrl-shipto)
      AND INDEX(sys-ctrl-shipto.char-fld, "WithPrompt") > 0 THEN
        v-merge-prompt = YES.
    ELSE
      IF AVAIL(sys-ctrl-shipto)
        AND INDEX(sys-ctrl-shipto.char-fld, "WithoutPrompt") > 0 THEN
      v-merge-prompt = NO.
    
  END. /* If Avail Oe-relh based on include file */
  
  
  /* if oe-relh exists more than one for same cust-no, ship-no,rel-date - pick first oe-relh - wrong*/
  IF relh-recid NE ? THEN
  FIND oe-relh NO-LOCK
    WHERE RECID(oe-relh)  EQ relh-recid
      AND oe-relh.cust-no EQ v-cust-no
      AND oe-relh.ship-id EQ oe-rel.ship-id
    NO-ERROR.
  
  IF avail oe-relh THEN DO:
    
    
    out-recid = RECID(oe-relh).
    
    IF (v-auto AND v-merge-prompt NE YES) OR v-merge-prompt = NO THEN  DO:
      v-dlg-sel = 1.
    END. /* If v-auto */
    ELSE DO:
    
      MESSAGE "A previous release exists for Customer/Ship-To/Date: " +
        TRIM(oe-relh.cust-no) + "/" +
        TRIM(oe-relh.ship-id) + "/" +
        STRING(oe-relh.rel-date,"99-99-99") +
        ", Choose YES to print multiple items on one Release/Pick ticket.,Choose NO to create a separate Release/Pick ticket for each item."
          VIEW-AS ALERT-BOX BUTTON YES-NO-CANCEL UPDATE ll-ans AS LOG.
          
          IF ll-ans THEN v-dlg-sel = 1.
          ELSE IF NOT ll-ans THEN v-dlg-sel = 2.
          ELSE v-dlg-sel = 3.
          
        END. /* Not v-auto, ask user whether to merge */
      END. /* If avail oe-relh */
      
      IF v-dlg-sel EQ 3 THEN RETURN.
      
      IF v-dlg-sel EQ 2    OR
      NOT avail oe-relh THEN RUN oe/cre-relh.p (RECID(oe-rel)).
      
      IF v-email THEN
      DO:
       RELEASE oe-relh.        
        
        FOR EACH oe-relh
          WHERE oe-relh.company  EQ oe-rel.company
            AND oe-relh.rel-date EQ oe-rel.rel-date
            AND oe-relh.cust-no  EQ v-cust-no
            AND oe-relh.ship-id  EQ oe-rel.ship-id
            AND oe-relh.posted   EQ NO
            AND oe-relh.deleted  EQ NO
            AND (oe-relh.printed EQ NO OR relmerge-log)
            AND (IF oe-rel.s-code = "" OR
                CAN-FIND(FIRST oe-rell
                WHERE oe-rell.r-no   EQ oe-relh.r-no
                AND oe-rell.s-code EQ oe-rel.s-code))


                AND (relmerge-chr NE "SameOrderOnly" OR
                CAN-FIND(FIRST oe-rell
                  WHERE oe-rell.r-no   EQ oe-relh.r-no
                    AND oe-rell.ord-no EQ oe-rel.ord-no))
                    AND (relmerge-chr NE "SamePO#Only" OR
                        (CAN-FIND(FIRST sys-ctrl-shipto
                        WHERE sys-ctrl-shipto.company = oe-rel.company
                          AND sys-ctrl-shipto.NAME = "RelMerge"
                          AND sys-ctrl-shipto.cust-vend = YES
                          AND sys-ctrl-shipto.cust-vend-no = oe-relh.cust-no)
                          AND oe-relh.po-no = oe-rel.po-no
                          AND oe-relh.rel-date = oe-rel.rel-date
                          AND oe-relh.ship-id  = oe-rel.ship-id))
                  USE-INDEX delpost NO-LOCK
          BY oe-relh.printed
          BY oe-relh.r-no:
          LEAVE.
        END. /* For Each oe-relh */
        
        CREATE tt-email.
        ASSIGN
          tt-email.cust-no = oe-rel.cust-no
          tt-email.ord-no  = oe-rel.ord-no
          tt-email.i-no    = oe-rel.i-no
          tt-email.rel-qty = oe-rel.qty
          tt-email.rel-date = IF AVAIL oe-relh THEN oe-relh.rel-date
                              ELSE oe-rel.rel-date
          tt-email.po-no    = oe-rel.po-no.
        
        RELEASE tt-email.
        
      END. /* If v-email */
      
      RUN oe/cre-rell.p (RECID(oe-rel)).
/* end rel-block - last end is in the enclosing procedure (oe/actrel.p) */      

      
/* end ---------------------------------- copr. 1998  advanced software, inc. */
      
      
