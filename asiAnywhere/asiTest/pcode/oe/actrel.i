/* ---------------------------------------------------- oe/actrel.i 01/98 JLF */
/* order entry - Create actual releases from planned release line             */
/* -------------------------------------------------------------------------- */

DEF VAR v-nxt-r-no AS INT INIT 1 NO-UNDO.
DEF VAR v-dlg-sel AS INT NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.

DEF VAR v-email AS LOG INIT YES NO-UNDO.
DEF VAR ll-ans AS LOG NO-UNDO.
DEF BUFFER s-code FOR reftable.

/*DO TRANSACTION:
  {sys\inc\addxfer.i}
END.
*/

DEF VAR addxfer-log AS LOG NO-UNDO.

    FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ oe-rel.company
      AND sys-ctrl.name    EQ "ADDXFER"
    NO-LOCK NO-ERROR.
    
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = oe-rel.company
   sys-ctrl.name    = "ADDXFER"
   sys-ctrl.module  = "OU1"
   sys-ctrl.descrip = "When creating actual transfer releases, inhouse customer?"
   sys-ctrl.int-fld = 0.
end.
assign
 addxfer-log = sys-ctrl.log-fld.

choice = YES.

/*IF NOT v-auto THEN */
    
/*  MESSAGE " This will create an Actual release from this Planned release. "
          SKIP
          " Would you like to continue? "
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE choice.*/

if choice then rel-block: repeat.
    
       /** Find the most recent actual release with the same #order, shipto,
           release date, not deleted, and not printed. **/
     
    repeat preselect
         
           each oe-relh no-lock
           where oe-relh.company  eq oe-rel.company
/*             and oe-relh.ord-no   eq oe-rel.ord-no */
             and oe-relh.rel-date eq oe-rel.rel-date
             and oe-relh.cust-no  eq oe-rel.cust-no
             and oe-relh.ship-id  eq oe-rel.ship-id
             and oe-relh.posted   eq no
/*             and oe-relh.po-no    eq oe-rel.po-no */
             and oe-relh.deleted  eq no
           use-index rel-date,
               
           each oe-rell no-lock
           where oe-rell.company eq oe-relh.company
             and oe-rell.r-no    eq oe-relh.r-no
             AND oe-rell.ord-no  EQ oe-rel.ord-no
             and oe-rell.i-no    eq oe-rel.i-no                                  
             AND oe-rell.rel-no  eq oe-rel.rel-no /* YSK added 01/20/03 - TASK 01170303*/
           :

         find next oe-rell no-error.
         if not avail oe-rell then leave.
         if avail oe-rell then do:
           if not v-auto /*or program-name(2) begins "oe/oe-relx." */ then do:
              /*  message "This has been already been released. Can not release."
                         view-as alert-box error.
                return error.*/
             
             RETURN.
           end.      
         end.
       end. /* repeat preselect oe-relh */
       
       v-cust-no = oe-rel.cust-no.

       IF addxfer-log THEN
       DO:
          FIND FIRST s-code WHERE
               s-code.reftable EQ "oe-rel.s-code" AND
               s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
               NO-LOCK NO-ERROR.
         
          IF AVAIL s-code THEN
          DO:
             IF s-code.CODE EQ 'T' THEN
             DO:
                FIND FIRST cust WHERE
                     cust.company EQ oe-rel.company AND
                     cust.active EQ 'X'
                     NO-LOCK NO-ERROR.
         
                IF AVAIL cust THEN
                DO:
                   IF CAN-FIND(FIRST shipto WHERE
                      shipto.company EQ oe-rel.company AND
                      shipto.cust-no EQ cust.cust-no AND
                      shipto.ship-no EQ oe-rel.ship-no AND
                      shipto.ship-id EQ oe-rel.ship-id) THEN
                      v-cust-no = cust.cust-no.

                   RELEASE cust.
                END.
             END.
             RELEASE s-code.
          END.
       END.   
        v-merge-prompt = ?.
       {oe/findrelh.i oe-rel v-cust-no} 
      if avail oe-relh then do:
        /* out-recid = recid(oe-relh).*/
         IF ((NOT CAN-FIND(FIRST sys-ctrl-shipto
                    WHERE sys-ctrl-shipto.company = oe-relh.company
                      AND sys-ctrl-shipto.NAME = "RelMerge"
                      AND sys-ctrl-shipto.cust-vend = YES
                      AND sys-ctrl-shipto.char-fld  = "SamePo#Only"
                      AND sys-ctrl-shipto.cust-vend-no = oe-relh.cust-no))
                      
            OR
            (ll-rell-found AND oe-relh.r-no = l-rno)) THEN
            FIND FIRST sys-ctrl-shipto
                   WHERE sys-ctrl-shipto.company = oe-relh.company
                     AND sys-ctrl-shipto.NAME = "RelMerge"
                     AND sys-ctrl-shipto.cust-vend = YES
                     AND sys-ctrl-shipto.char-fld  BEGINS "SamePo#Only"
                     AND sys-ctrl-shipto.cust-vend-no = oe-relh.cust-no
                  NO-LOCK NO-ERROR.
            IF AVAIL(sys-ctrl-shipto) 
                AND INDEX(sys-ctrl-shipto.char-fld, "WithPrompt") > 0 THEN
                v-merge-prompt = YES.
            ELSE
              IF AVAIL(sys-ctrl-shipto) 
                AND INDEX(sys-ctrl-shipto.char-fld, "WithoutPrompt") > 0 THEN
                  v-merge-prompt = NO.
       END.
       /* if oe-relh exists more than one for same cust-no, ship-no,rel-date - pick first oe-relh - wrong*/
       IF relh-recid NE ? THEN
       FIND oe-relh NO-LOCK
           WHERE RECID(oe-relh)  EQ relh-recid
             AND oe-relh.cust-no EQ v-cust-no
             AND oe-relh.ship-id EQ oe-rel.ship-id
           NO-ERROR.
       if avail oe-relh then do:
         out-recid = recid(oe-relh).

       if (v-auto AND v-merge-prompt NE YES) OR v-merge-prompt = NO then v-dlg-sel = 1.
         else  do:
           /* message "A previous release exists for Customer/Ship-To/Date: " +
                    TRIM(oe-relh.cust-no) + "/" +
                    TRIM(oe-relh.ship-id) + "/" +
                    STRING(oe-relh.rel-date,"99-99-99") +
                    ", Choose YES to print multiple items on one Release/Pick ticket.,Choose NO to create a separate Release/Pick ticket for each item."
                    view-as alert-box button yes-no-cancel update ll-ans as log.*/
             ASSIGN  ll-ans = NO.
            if ll-ans then v-dlg-sel = 1.
            else if not ll-ans then v-dlg-sel = 2.
            else v-dlg-sel = 3.
         end.                      
       end.    

        
      
       if v-dlg-sel eq 3 then return.

       if v-dlg-sel eq 2    or
          not avail oe-relh then run oe/cre-relh.p (recid(oe-rel)).
       
        IF v-email  THEN
       DO:
            
          DEF BUFFER b2-reft-findrelh FOR reftable.

          RELEASE oe-relh.

          FIND FIRST b2-reft-findrelh WHERE
               b2-reft-findrelh.reftable EQ "oe-rel.s-code" AND
               b2-reft-findrelh.company  EQ STRING(oe-rel.r-no,"9999999999")
               NO-LOCK NO-ERROR.

          FOR EACH oe-relh
            WHERE oe-relh.company  EQ oe-rel.company
              AND oe-relh.rel-date EQ oe-rel.rel-date
              AND oe-relh.cust-no  EQ v-cust-no
              AND oe-relh.ship-id  EQ oe-rel.ship-id
              AND oe-relh.posted   EQ NO
              AND oe-relh.deleted  EQ NO
              AND (oe-relh.printed EQ NO OR relmerge-log) 
              AND (NOT AVAIL b2-reft-findrelh OR
                   CAN-FIND(FIRST oe-rell
                            WHERE oe-rell.r-no   EQ oe-relh.r-no
                              AND oe-rell.s-code EQ b2-reft-findrelh.code))
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
                    AND oe-relh.ship-id  = oe-rel.ship-id)
                   )
            USE-INDEX delpost NO-LOCK
            BY oe-relh.printed
            BY oe-relh.r-no:
            LEAVE.
          END. 

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
          
       END.



       run oe/cre-rell.p (recid(oe-rel)).
/* end ---------------------------------- copr. 1998  advanced software, inc. */

