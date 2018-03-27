/* getPrice.i - shared by oe/impord.w & cXML/monitor.w */

PROCEDURE getPrice:
  DEF INPUT PARAMETER ip-ordl-rowid AS ROWID NO-UNDO.

  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR lv-save-xoe-ordl AS ROWID NO-UNDO.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  
  def var lv-est-no as CHAR NO-UNDO.
  def var lv-price as dec no-undo.
  def var lv-pr-uom as cha no-undo.
  def var v-tmp-part as cha no-undo.
  def var v-qty as int no-undo.
  DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.

  FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ip-ordl-rowid NO-ERROR.  
  IF NOT AVAIL bf-oe-ordl THEN RETURN NO-APPLY.

  FIND FIRST sys-ctrl-shipto WHERE sys-ctrl-shipto.company = cocode
                               AND sys-ctrl-shipto.NAME = "OEImport"
                               AND sys-ctrl-shipto.char-fld = bf-oe-ordl.cust NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl-shipto THEN oeimport-int = sys-ctrl-shipto.int-fld.
  
  DO:
    IF AVAIL bf-oe-ordl AND TRIM(bf-oe-ordl.est-no) <> "" THEN DO:

      IF NOT AVAIL xoe-ord THEN
      FIND FIRST xoe-ord WHERE xoe-ord.company EQ g_company
                           AND xoe-ord.ord-no  EQ bf-oe-ordl.ord-no
                           NO-LOCK NO-ERROR.

      ASSIGN
       save_id   = RECID(bf-oe-ordl)
       lv-rowid  = ROWID(bf-oe-ordl)
       v-i-item  = bf-oe-ordl.i-no
       v-i-qty   = INT(bf-oe-ordl.qty)
       v-qty-mod = YES. /* new record, so will have been modified */

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ v-i-item
          NO-LOCK NO-ERROR.
      
      IF AVAIL itemfg THEN DO:
        /*IF AVAIL xoe-ordl THEN lv-save-xoe-ordl= ROWID(xoe-ordl). */
        /* Depends on xoeitem */        
        IF /*itemfg.i-code = "C" AND*/ bf-oe-ordl.est-no <> "" THEN DO:        
            lv-est-no = FILL(" ",8 - LENGTH(TRIM(bf-oe-ordl.est-no))) +
                 TRIM(bf-oe-ordl.est-no).
             find first xest where xest.company eq cocode and
                   xest.est-no eq lv-est-no no-lock no-error.
             assign lv-price = bf-oe-ordl.price
                 lv-pr-uom = bf-oe-ordl.pr-uom
                 lv-qty    = bf-oe-ordl.qty
                 v-tmp-part = bf-oe-ordl.i-no.

             run oe/getqprdt.p (recid(xest), bf-oe-ordl.part-no,
                                v-tmp-part, oeimport-int,
                                input-output lv-price,
                                input-output lv-pr-uom,
                                OUTPUT lv-q-no,
                                INPUT-OUTPUT lv-qty).
             
             ASSIGN bf-oe-ordl.price  = lv-price
                    bf-oe-ordl.pr-uom = lv-pr-uom
                    .

        END.
       
        ELSE IF itemfg.i-code = "S" THEN DO:            
            /* Task 04151301  */            
            RUN oe/oe-price.p.
        END.
      

        /*FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ lv-rowid NO-ERROR. */
        /*{oe/ordltot3.i bf-oe-ordl qty bf-oe-ordl} move to bottom*/
/*         IF lv-save-xoe-ordl NE ? THEN                              */
/*             FIND xoe-ordl WHERE ROWID(xoe-ordl) = lv-save-xoe-ordl */
/*                EXCLUSIVE-LOCK NO-ERROR.                            */

/* MESSAGE "get-price: " bf-oe-ordl.i-no bf-oe-ordl.pr-uom */
/*             bf-oe-ordl.price                            */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                  */
      END.      
    END. /* est-no <> "" */
    ELSE DO:
                
        IF AVAIL bf-oe-ordl AND TRIM(bf-oe-ordl.est-no) EQ "" THEN DO:
    
          IF NOT AVAIL xoe-ord THEN
          FIND FIRST xoe-ord WHERE xoe-ord.company EQ g_company
                               AND xoe-ord.ord-no  EQ bf-oe-ordl.ord-no
                               NO-LOCK NO-ERROR.
    
          ASSIGN
           save_id   = RECID(bf-oe-ordl)
           lv-rowid  = ROWID(bf-oe-ordl)
           v-i-item  = bf-oe-ordl.i-no
           v-i-qty   = INT(bf-oe-ordl.qty)
           v-qty-mod = YES. /* new record, so will have been modified */
    
          FIND FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ v-i-item
              NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN DO:
            /*IF AVAIL xoe-ordl THEN lv-save-xoe-ordl= ROWID(xoe-ordl). */
            /* Depends on xoeitem */              
            IF itemfg.i-code = "C" AND bf-oe-ordl.est-no <> "" THEN DO:        
                lv-est-no = FILL(" ",8 - LENGTH(TRIM(bf-oe-ordl.est-no))) +
                     TRIM(bf-oe-ordl.est-no).
                 find first xest where xest.company eq cocode and
                       xest.est-no eq lv-est-no no-lock no-error.
                 assign lv-price = bf-oe-ordl.price
                     lv-pr-uom = bf-oe-ordl.pr-uom
                     lv-qty    = bf-oe-ordl.qty
                     v-tmp-part = bf-oe-ordl.i-no.
                 run oe/getqpric.p (recid(xest), bf-oe-ordl.part-no,
                                    v-tmp-part,
                                    input-output lv-price,
                                    input-output lv-pr-uom,
                                    OUTPUT lv-q-no,
                                    INPUT-OUTPUT lv-qty).                 
                 ASSIGN bf-oe-ordl.price  = lv-price
                        bf-oe-ordl.pr-uom = lv-pr-uom
                        .
            END.
            ELSE IF itemfg.i-code = "S" THEN DO:                
                RUN oe/oe-price.p.
            END.
    
          END. /* avail itemfg */
       END. /* avail bf-oe-ordl */
    END. /* else (est-no blank) */

    IF AVAIL bf-oe-ordl AND bf-oe-ordl.q-no <> 0 THEN DO:
       FOR EACH quotehd WHERE quotehd.company EQ cocode
                          AND quotehd.loc     EQ locode
                          AND quotehd.q-no = bf-oe-ordl.q-no
                     NO-LOCK,
           EACH quoteitm OF quotehd WHERE quoteitm.part-no EQ bf-oe-ordl.part-no OR
                      (quoteitm.part-no EQ bf-oe-ordl.i-no AND bf-oe-ordl.i-no NE "") NO-LOCK,
           EACH quoteqty OF quoteitm WHERE quoteqty.qty = bf-oe-ordl.qty NO-LOCK:           
           ASSIGN bf-oe-ordl.price  = quoteqty.price
                  bf-oe-ordl.pr-uom = quoteqty.uom
                  .
           LEAVE.
       END.       
    END.

    {oe/ordltot3.i bf-oe-ordl qty bf-oe-ordl}
  END. /* Do: */
END PROCEDURE.
