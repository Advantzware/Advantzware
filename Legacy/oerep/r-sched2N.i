
DEF VAR v-custpart AS CHAR NO-UNDO.
DEF VAR v-value-head AS LOG NO-UNDO.

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.opened  EQ YES
        AND oe-ordl.ord-no  GE v-ford-no[1]
        AND oe-ordl.ord-no  LE v-ford-no[2]
        AND oe-ordl.i-no    GE v-fitem[1]
        AND oe-ordl.i-no    LE v-fitem[2]
        AND ((oe-ordl.s-man[1] GE v-fsman[1] AND
              oe-ordl.s-man[1] LE v-fsman[2]) OR
             (oe-ordl.s-man[2] GE v-fsman[1] AND
              oe-ordl.s-man[2] LE v-fsman[2]) OR
             (oe-ordl.s-man[3] GE v-fsman[1] AND
              oe-ordl.s-man[3] LE v-fsman[2]))
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
      USE-INDEX opened NO-LOCK,

      FIRST oe-ord
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND oe-ord.cust-no GE v-fcust[1]
        AND oe-ord.cust-no LE v-fcust[2]
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq oe-ord.cust-no
        AND ttCustList.log-fld no-lock) else true)
      NO-LOCK:
      {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 
/*     RUN oe/cleanrel.p (ROWID(oe-ordl)). */

    for each oe-rel
        where oe-rel.company   eq cocode
          and oe-rel.ord-no    eq oe-ordl.ord-no
          and oe-rel.i-no      eq oe-ordl.i-no
          and oe-rel.line      eq oe-ordl.line
          and oe-rel.rel-date  ge v-fdate[1]
          and oe-rel.rel-date  le v-fdate[2]
          and oe-rel.ship-id   ge v-fship[1]
          and oe-rel.ship-id   le v-fship[2]
        no-lock:

      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

      if index("AB",v-type) gt 0 then next.

      IF INDEX(v-types,v-type) GT 0 THEN
      DO li = 1 TO EXTENT(oe-ordl.s-man):
        IF oe-ordl.s-man[li] NE ""         AND
           oe-ordl.s-man[li] GE v-fsman[1] AND
           oe-ordl.s-man[li] LE v-fsman[2] THEN DO:
          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = if v-sort eq "R" then
                                 (string(year(oe-rel.rel-date),"9999") +
                                  string(month(oe-rel.rel-date),"99")  +
                                  string(day(oe-rel.rel-date),"99"))
                                else
                               if v-sort eq "N" then oe-ordl.i-name
                               else
                               if v-sort eq "S" then oe-ordl.s-man[li]
                               else
                               if v-sort eq "O" then string(oe-rel.ord-no,"999999")
                               else
                               if v-sort eq "C" then oe-ord.cust-no
                               else oe-rel.i-no
           tt-report.key-02  = if v-sort eq "C" then
                                 oe-rel.ship-id
                               else
                               if v-sort ne "R" then
                                 (string(year(oe-rel.rel-date),"9999") +
                                  string(month(oe-rel.rel-date),"99")  +
                                  string(day(oe-rel.rel-date),"99"))
                               else oe-rel.cust-no
           tt-report.key-03  = if v-sort eq "C" then
                                 (string(year(oe-rel.rel-date),"9999") +
                                  string(month(oe-rel.rel-date),"99")  +
                                  string(day(oe-rel.rel-date),"99"))
                               else ""
           tt-report.key-04  = string(oe-ord.ord-no,"9999999999")
           tt-report.key-05  = string(index(v-types,v-type),"99")
           tt-report.key-06  = v-type
           tt-report.rec-id  = recid(oe-rel)
           tt-report.key-07  = oe-rel.i-no.
           IF li GT 1 THEN DO:
               tt-report.is-duplicate = TRUE.
           END.
               
        END.
      end.
    end.
  /*END.

  for each oe-relh
      where oe-relh.company    eq cocode
        and oe-relh.posted     eq no
        and oe-relh.deleted    eq no
        and oe-relh.cust-no    ge v-fcust[1]
        and oe-relh.cust-no    le v-fcust[2]
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq oe-relh.cust-no
        AND ttCustList.log-fld no-lock) else true)
        and oe-relh.ship-id    ge v-fship[1]
        and oe-relh.ship-id    le v-fship[2]
        and oe-relh.rel-date   ge v-fdate[1]
        and oe-relh.rel-date   le v-fdate[2]
      use-index delpost no-lock,
            
      each oe-rell
      where oe-rell.company eq cocode
        and oe-rell.r-no    eq oe-relh.r-no
        and oe-rell.ord-no  ge v-ford-no[1]
        and oe-rell.ord-no  le v-ford-no[2]
        and oe-rell.i-no    ge v-fitem[1]
        and oe-rell.i-no    le v-fitem[2]
        and ((oe-rell.b-ord-no ne 0 and index(v-types,"B") gt 0) or
             (oe-rell.b-ord-no eq 0 and index(v-types,"A") gt 0))
      USE-INDEX r-no no-lock,
      
      first oe-ordl
      where oe-ordl.company eq oe-rell.company
        and oe-ordl.ord-no  eq oe-rell.ord-no
        and oe-ordl.i-no    eq oe-rell.i-no
        and oe-ordl.line    eq oe-rell.line
        and oe-ordl.opened  eq yes
        and ((oe-ordl.s-man[1] ge v-fsman[1] and
              oe-ordl.s-man[1] le v-fsman[2]) or
             (oe-ordl.s-man[2] ge v-fsman[1] and
              oe-ordl.s-man[2] le v-fsman[2]) or
             (oe-ordl.s-man[3] ge v-fsman[1] and
              oe-ordl.s-man[3] le v-fsman[2]))
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
      no-lock,

      first oe-ord
      where oe-ord.company eq oe-ordl.company
        and oe-ord.ord-no  eq oe-ordl.ord-no
      no-lock*/

    /* new start */
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
        USE-INDEX ord-no,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.ship-id  GE v-fship[1]
          AND oe-relh.ship-id  LE v-fship[2]
          AND oe-relh.rel-date GE v-fdate[1]
          AND oe-relh.rel-date LE v-fdate[2]
    /* new end */
      
      BREAK BY oe-rell.r-no
            BY oe-rell.ord-no
            BY oe-rell.i-no
            BY oe-rell.line
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.

    IF LAST-OF(oe-rell.po-no) THEN
    DO li = 1 TO EXTENT(oe-ordl.s-man):
      IF oe-ordl.s-man[li] NE ""         AND
         oe-ordl.s-man[li] GE v-fsman[1] AND
         oe-ordl.s-man[li] LE v-fsman[2] THEN DO:
        create tt-report.
        assign
         tt-report.term-id = ""
         tt-report.key-01  = if v-sort eq "R" then
                               (string(year(oe-relh.rel-date),"9999") +
                                string(month(oe-relh.rel-date),"99")  +
                                string(day(oe-relh.rel-date),"99"))
                             else
                             if v-sort eq "N" then oe-ordl.i-name
                             else
                             if v-sort eq "S" then oe-ordl.s-man[li]
                             else
                             if v-sort eq "O" then string(oe-rell.ord-no,"9999999999")
                             else
                             if v-sort eq "C" then oe-ord.cust-no
                             else oe-rell.i-no
         tt-report.key-02  = if v-sort eq "C" then
                               oe-relh.ship-id
                             else
                             if v-sort ne "R" then
                               (string(year(oe-relh.rel-date),"9999") +
                                string(month(oe-relh.rel-date),"99")  +
                                string(day(oe-relh.rel-date),"99"))
                             else oe-relh.cust-no
         tt-report.key-03  = if v-sort eq "C" then
                               (string(year(oe-relh.rel-date),"9999") +
                                string(month(oe-relh.rel-date),"99")  +
                                string(day(oe-relh.rel-date),"99"))
                             else ""
         tt-report.key-04  = string(oe-ord.ord-no,"9999999999")
         tt-report.key-05  = string(index(v-types,v-type),"99")
         tt-report.key-06  = if oe-rell.b-ord-no eq 0 then "A" else "B"
         tt-report.qty     = lv-qty
         tt-report.rec-id  = recid(oe-rell)
         tt-report.key-07  = oe-rell.i-no.
         IF li GT 1 THEN DO:
            tt-report.is-duplicate = TRUE.
         END.

      END.
    END.

    /* new start */
    END.
    /* new end */
  end.

  for each tt-report where tt-report.term-id eq "" NO-LOCK:
      FIND FIRST tt-report-oh WHERE tt-report-oh.key-04 EQ tt-report.key-04 AND
          tt-report-oh.key-07 EQ tt-report.key-07 NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-report-oh THEN DO:
          CREATE tt-report-oh.
          ASSIGN 
              tt-report-oh.key-01 = tt-report.key-01 
              tt-report-oh.key-04 = tt-report.key-04
              tt-report-oh.key-07 = tt-report.key-07 .
      END.
      ELSE DO:
          tt-report.chkvalue = FALSE.
      END.
  END.
  
  
  FOR EACH tt-report-oh NO-LOCK:
      DELETE tt-report-oh.
  END.

  /* If record duplicated because of 2 sales reps, delete the duplicate, */
  /* unless breaking by sales rep in which case want to show both lines */
  IF v-sort NE "S" THEN  DO:
      FOR EACH tt-report WHERE tt-report.is-duplicate.
          DELETE tt-report.
      END.
  END.

  for each tt-report where tt-report.term-id eq ""
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04
            by tt-report.key-05:

    IF v-sort EQ "S" AND FIRST-OF(tt-report.key-01) THEN DO:
      FIND FIRST sman
          WHERE sman.company EQ cocode
            AND sman.sman    EQ tt-report.key-01
          NO-LOCK NO-ERROR.
      lv-slsmn-info = STRING(tt-report.key-01,"x(3)") + " " +
                      (IF AVAIL sman THEN sman.sname ELSE "").
      IF FIRST(tt-report.key-01) THEN VIEW FRAME r-top-s.
      ELSE PAGE.
    END.

    release oe-rel.
    release oe-rell.
    release oe-relh.

    find first oe-rel 
        where recid(oe-rel) eq tt-report.rec-id 
        no-lock no-error.

    if avail oe-rel then do:
      FOR EACH oe-rell
          WHERE oe-rell.company  EQ cocode
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            AND CAN-FIND(FIRST oe-relh
                         WHERE oe-relh.r-no    EQ oe-rell.r-no
                           AND oe-relh.posted  EQ NO
                           AND oe-relh.deleted EQ NO)
          USE-INDEX ord-no NO-LOCK:
        tt-report.rec-id = RECID(oe-rell).
        LEAVE.
      END.
                 
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rel.ord-no
            and oe-ordl.i-no    eq oe-rel.i-no
            and oe-ordl.line    eq oe-rel.line
          no-lock.
    end.

    find first oe-rell
        where recid(oe-rell) eq tt-report.rec-id
        no-lock no-error.
    if avail oe-rell then do:    
      find first oe-relh
           where oe-relh.company eq cocode
             and oe-relh.r-no    eq oe-rell.r-no
           no-lock. 
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rell.ord-no
            and oe-ordl.i-no    eq oe-rell.i-no
            and oe-ordl.line    eq oe-rell.line
          no-lock.
    end.

    /* gdm - 01290902 */
    ASSIGN v-custpart = oe-ordl.part-no.
  
    IF AVAIL oe-ordl THEN
        {custom/statusMsg.i "'Processing Order # ' + string(oe-ordl.ord-no)"} 

    find first oe-ord of oe-ordl no-lock.
    find first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-ord.cust-no
        no-lock.
          
    if avail oe-rell then 
      assign
       v-qty     = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty 
       v-date    = oe-relh.rel-date 
       v-po-no   = oe-rell.po-no
       v-rel-no  = oe-rell.rel-no
       v-ship-id = oe-relh.ship-id.
    else
      assign
       v-qty     = if oe-rel.link-no eq 0 then oe-rel.tot-qty else oe-rel.qty  
       v-date    = oe-rel.rel-date
       v-po-no   = oe-rel.po-no
       v-rel-no  = oe-rel.rel-no
       v-ship-id = oe-rel.ship-id. 

    /* gdm - 11030808 start */
    ASSIGN v-qtyOH = 0. 
    IF v-rdqty EQ "JobQty" THEN DO:
          FOR EACH fg-bin FIELDS(qty) NO-LOCK 
            WHERE fg-bin.company EQ cocode
              AND fg-bin.i-no    EQ oe-ordl.i-no
              AND fg-bin.job-no  EQ oe-ordl.job-no
              AND fg-bin.job-no2 EQ oe-ordl.job-no2
            USE-INDEX job:
            ASSIGN v-qtyOH = v-qtyOH + fg-bin.qty.
          END.
      END.
      ELSE 
          FOR EACH fg-bin FIELDS(qty) NO-LOCK 
            WHERE fg-bin.company EQ cocode
              AND fg-bin.i-no    EQ oe-ordl.i-no
            USE-INDEX i-no:
            ASSIGN v-qtyOH = v-qtyOH + fg-bin.qty.
          END.

    EMPTY TEMP-TABLE tt-formtext.

    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ oe-ordl.company
          AND itemfg.i-no    EQ oe-ordl.i-no NO-ERROR.
    IF AVAIL itemfg THEN DO:
        
        ASSIGN lv-text = "".
        
        IF v-spcd NE "" THEN DO:
            
            FOR EACH notes NO-LOCK 
                WHERE notes.rec_key  EQ itemfg.rec_key
                AND notes.note_code EQ v-spcd,
                FIRST ITEM-spec NO-LOCK 
                WHERE item-spec.company EQ itemfg.company
                AND item-spec.i-no    EQ ""
                AND item-spec.code    EQ notes.note_code:

              lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
            END.

            DO li = 1 TO 3:
                CREATE tt-formtext.
                ASSIGN
                    tt-line-no = li
                    tt-length  = 30.
            END.

            RUN custom/formtext.p (lv-text).
            
            ASSIGN v-icnt = 0 v-notes = "".

            FOR EACH tt-formtext:
                v-icnt = v-icnt + 1.
                IF  v-icnt <= 3 THEN v-notes[v-icnt] = tt-formtext.tt-text.
            END.
        
        END. /* WHEN AVAIL v-spcd */
        
    END. /* ITEMFG */
    RELEASE itemfg.

    /* gdm - 11030808 end */

    find first shipto
        where shipto.company eq cocode
          and shipto.cust-no eq oe-ordl.cust-no
          and shipto.ship-id eq v-ship-id
        no-lock no-error.
    
    if avail shipto then
      assign
       v-city  = shipto.ship-city
       v-state = shipto.ship-state
       v-zip   = shipto.ship-zip.
    else
      assign
       v-city  = cust.city
       v-state = cust.state
       v-zip   = cust.zip.
    
    RELEASE b-oe-ordl.
    IF oe-ordl.is-a-component THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company EQ oe-ordl.company
          AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
          AND b-oe-ordl.line    EQ oe-ordl.set-hdr-line
          AND b-oe-ordl.is-a-component EQ NO
        NO-LOCK NO-ERROR.

    RELEASE b-itemfg.

    v-value-head = NO.

    IF AVAIL b-oe-ordl AND tg_set-comp-order EQ YES THEN
    DO:
       ASSIGN
          v-value-head = YES
          v-price = 0.

       IF FIRST-OF(tt-report.key-04) THEN
          v-value = b-oe-ordl.t-price.
       ELSE
          v-value = 0.
    END.
    ELSE
    DO:
       IF AVAIL b-oe-ordl THEN
       FIND FIRST b-itemfg
            WHERE b-itemfg.company EQ b-oe-ordl.company
              AND b-itemfg.i-no    EQ b-oe-ordl.i-no
            NO-LOCK NO-ERROR.
       
       RELEASE itemfg.
       IF AVAIL b-itemfg THEN
       FIND FIRST itemfg
           WHERE itemfg.company EQ oe-ordl.company
             AND itemfg.i-no    EQ oe-ordl.i-no
           NO-LOCK NO-ERROR.
         
       IF AVAIL itemfg THEN DO:
         IF itemfg.std-tot-cost NE 0 THEN
           ASSIGN
            ld = (itemfg.std-tot-cost * oe-ordl.qty) /
                 (b-itemfg.std-tot-cost * b-oe-ordl.qty).
         ELSE
            ld = (itemfg.weight-100 * oe-ordl.qty) /
                 (b-itemfg.weight-100 * b-oe-ordl.qty).
       
         v-price = b-oe-ordl.t-price * ld / b-oe-ordl.qty.                       
       
       END.
       ELSE v-price = oe-ordl.t-price / oe-ordl.qty.
    END.

    IF v-price EQ ? THEN v-price = 0.

    IF v-value-head EQ NO THEN
       v-value = v-price * v-qty.

    ASSIGN
       v-fob = oe-ord.fob-code
       v-notes[2] = FILL(" ",142) + TRIM(v-notes[2])
       v-notes[3] = FILL(" ",142) + TRIM(v-notes[3]).

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
    
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = string(oe-ord.cust-no,"x(8)") .
                         WHEN "shipto"   THEN cVarValue = string(v-ship-id,"x(8)").
                         WHEN "fob"   THEN cVarValue = STRING(v-fob,"x(5)").
                         WHEN "city"  THEN cVarValue = STRING(v-city,"x(13)" ) .
                         WHEN "st"   THEN cVarValue = STRING(v-state,"x(2)" ) .
                         WHEN "zip"  THEN cVarValue = STRING(v-zip,"x(5)") .
                         WHEN "cust-po"   THEN cVarValue = STRING(v-po-no,"x(15)") .
                         WHEN "ord"  THEN cVarValue = STRING(oe-ord.ord-no,">>>>>>>>") .

                         WHEN "r"    THEN cVarValue = string(v-rel-no) .
                         WHEN "fgitem"   THEN cVarValue = string(oe-ordl.i-no,"x(15)").
                         WHEN "custpart"   THEN cVarValue = STRING(v-custpart,"x(15)").
                         WHEN "rel-qty"  THEN cVarValue = STRING(v-qty,"->>,>>>,>>9") .
                         WHEN "date"   THEN cVarValue = STRING(v-date,"99/99/9999") .
                         WHEN "t"  THEN cVarValue = STRING(tt-report.key-06,"x(1)") .
                         WHEN "qty-hand"   THEN cVarValue = STRING(v-qtyOH,"->>,>>>,>>9") .
                         WHEN "note"  THEN cVarValue = STRING(v-notes[1],"x(30)") .
                         WHEN "unit-pr"  THEN cVarValue = STRING(v-price,"->>>>>,>>9") .
                         WHEN "sal-val"   THEN cVarValue = STRING(v-value,"->>>,>>>,>>9") .
                         WHEN "item-name"  THEN cVarValue = STRING(oe-ordl.i-name,"x(30)") .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
                                        
   /* IF v-sort EQ "N" THEN DO:
                    
        /* gdm - 11030808 */
        IF v-noteslg THEN DO: /* WITH SPEC NOTES */

          /* gdm - 01290902 */
          IF v-custflg THEN DO: /* WITH CUST PART SPEC NOTES */
              display 
                    oe-ord.cust-no    COLUMN-LABEL "Customer"
                    v-ship-id         COLUMN-LABEL "Ship-To"
                    v-fob             COLUMN-LABEL "FOB"
                                      FORMAT "x(5)"
                    v-city            COLUMN-LABEL "City"
                                      FORMAT "x(12)"
                    v-state           COLUMN-LABEL "St"
                    v-zip             COLUMN-LABEL "Zip"
                                      FORMAT "x(5)"
                    v-po-no           COLUMN-LABEL "Customer PO"
                                      FORMAT "x(12)"
                    oe-ord.ord-no     COLUMN-LABEL "Order"                                      
                    v-rel-no          COLUMN-LABEL "R#"
                                      FORMAT ">>9"
                    tt-report.key-01  COLUMN-LABEL "FG Item Name"                   
                                      FORMAT "x(25)" 
                    v-custpart        COLUMN-LABEL "Cust Part #"                   
                                      FORMAT "x(20)" 
                    v-qty             COLUMN-LABEL "Release Qty"
                    v-date            COLUMN-LABEL "Date"
                                      FORMAT "99/99/99"
                    tt-report.key-06  COLUMN-LABEL "T"
                                      FORMAT "x(1)"
                    v-qtyOH           COLUMN-LABEL "Qty On Hand"
                    v-notes[1]        COLUMN-LABEL "Notes"
                                      FORMAT "x(25)"  
                 SKIP
                with down frame schdrel1NTC no-box stream-io width 185.

                IF TRIM(v-notes[2]) NE "" 
                    THEN
                     PUT UNFORMATTED v-notes[2] SKIP.

                IF TRIM(v-notes[3]) NE "" 
                    THEN PUT UNFORMATTED v-notes[3] SKIP.
          END.
          ELSE DO: /* WITHOUT CUST PART WITH SPEC NOTES */
              display 
                    oe-ord.cust-no    COLUMN-LABEL "Customer"
                    v-ship-id         COLUMN-LABEL "Ship-To"
                    v-fob             COLUMN-LABEL "FOB"
                                      FORMAT "x(5)"
                    v-city            COLUMN-LABEL "City"
                                      FORMAT "x(12)"
                    v-state           COLUMN-LABEL "St"
                    v-zip             COLUMN-LABEL "Zip"
                                      FORMAT "x(5)"
                    v-po-no           COLUMN-LABEL "Customer PO"
                                      FORMAT "x(12)"
                    oe-ord.ord-no     COLUMN-LABEL "Order"
                    v-rel-no          COLUMN-LABEL "R#"
                                      FORMAT ">>9"
                    tt-report.key-01  COLUMN-LABEL "FG Item Name"                   
                                      FORMAT "x(25)" 
                    v-qty             COLUMN-LABEL "Release Qty"
                    v-date            COLUMN-LABEL "Date"
                                      FORMAT "99/99/9999"
                    tt-report.key-06  COLUMN-LABEL "T"
                                      FORMAT "x(1)"
                    v-qtyOH           COLUMN-LABEL "Qty On Hand"
                    v-notes[1]        COLUMN-LABEL "Notes"
                                      FORMAT "x(30)"  
                 SKIP
                with down frame schdrel1NTN no-box stream-io width 185.
                                
                IF TRIM(v-notes[2]) NE "" 
                    THEN
                     PUT UNFORMATTED v-notes[2] SKIP.

                IF TRIM(v-notes[3]) NE "" 
                    THEN PUT UNFORMATTED v-notes[3] SKIP.
          END. 
                         
          IF tb_excel THEN DO:
          
           IF v-custflg 
             THEN /* WITH CUST PART SPEC NOTES */
              PUT STREAM excel UNFORMATTED
                '"' oe-ord.cust-no                        '",'
                '"' v-ship-id                             '",'
                '"' v-fob                                 '",' 
                '"' v-city                                '",'
                '"' v-state                               '",'
                '"' v-zip                                 '",'
                '"' v-po-no                               '",'
                '"' oe-ord.ord-no                         '",'
                '"' STRING(v-rel-no,">>9")                '",'
                '"' tt-report.key-01                      '",'
                '"' v-custpart                            '",'
                '"' STRING(v-qty,"->>,>>>,>>9")           '",'
                '"' (IF v-date NE ? THEN
                     STRING(v-date,"99/99/9999") ELSE "") '",'
                '"' tt-report.key-06                      '",'
                '"' STRING(v-qtyOH,"->>,>>>,>>9")         '",'
                '"' v-notes[1]                            '",'
                '"' v-notes[2]                            '",'
                '"' v-notes[3]                            '",'
              SKIP.

             ELSE /* WITHOUT CUST PART WITH SPEC NOTES */
              PUT STREAM excel UNFORMATTED
                '"' oe-ord.cust-no                        '",'
                '"' v-ship-id                             '",'
                '"' v-fob                                 '",' 
                '"' v-city                                '",'
                '"' v-state                               '",'
                '"' v-zip                                 '",'
                '"' v-po-no                               '",'
                '"' oe-ord.ord-no                         '",'
                '"' STRING(v-rel-no,">>9")                '",'
                '"' tt-report.key-01                      '",'
                '"' STRING(v-qty,"->>,>>>,>>9")           '",'
                '"' (IF v-date NE ? THEN
                     STRING(v-date,"99/99/9999") ELSE "") '",'
                '"' tt-report.key-06                      '",'
                '"' STRING(v-qtyOH,"->>,>>>,>>9")         '",'
                '"' v-notes[1]                            '",'
                '"' v-notes[2]                            '",'
                '"' v-notes[3]                            '",'
              SKIP.
          END. /* EXCEL REPORT*/

        END. /* WITH SPEC NOTES */
        ELSE
        IF NOT v-noteslg THEN DO: /* WITHOUT SPEC NOTES */

          /* gdm - 01290902 */
          IF v-custflg THEN DO:  /* WITH CUST PART */

            display 
              oe-ord.cust-no    COLUMN-LABEL "Customer"
              v-ship-id         COLUMN-LABEL "Ship-To"
              v-city            COLUMN-LABEL "City"
                                FORMAT "x(12)"
              v-state           COLUMN-LABEL "St"
              v-zip             COLUMN-LABEL "Zip"
                                FORMAT "x(5)"
              v-po-no           COLUMN-LABEL "Customer PO"
                                FORMAT "x(12)"
              oe-ord.ord-no     COLUMN-LABEL "Order"
              v-rel-no          COLUMN-LABEL "R#"
                                FORMAT ">>9"
              tt-report.key-01  COLUMN-LABEL "FG Item Name"
                                FORMAT "x(30)" 
              v-custpart        COLUMN-LABEL "Cust Part #"
                                FORMAT "x(30)"               
              v-qty             COLUMN-LABEL "Release Qty"
              v-date            COLUMN-LABEL "Date"
                                FORMAT "99/99/9999"
              tt-report.key-06  COLUMN-LABEL "T"
                                FORMAT "x(1)" 
              v-price           COLUMN-LABEL "Unit Pr"   
                                FORMAT ">,>>9.99<<" 
              v-value           COLUMN-LABEL "Sales Value"              
          
             with down frame schdrel1C no-box stream-io width 185.

          END. 
          ELSE DO: 

            display 
              oe-ord.cust-no    COLUMN-LABEL "Customer"
              v-ship-id         COLUMN-LABEL "Ship-To"
              v-city            COLUMN-LABEL "City"
                                FORMAT "x(12)"
              v-state           COLUMN-LABEL "St"
              v-zip             COLUMN-LABEL "Zip"
                                FORMAT "x(5)"
              v-po-no           COLUMN-LABEL "Customer PO"
                                FORMAT "x(12)" 
              oe-ord.ord-no     COLUMN-LABEL "Order"
              v-rel-no          COLUMN-LABEL "R#"
                                FORMAT ">>9"
              oe-rell.i-no      COLUMN-LABEL "FG Item#" 
                                WHEN AVAIL oe-rell 
              oe-rel.i-no       WHEN NOT AVAIL oe-rell 
                                @ oe-rell.i-no 
              tt-report.key-01  COLUMN-LABEL "FG Item Name"
                                FORMAT "x(30)"               
              v-qty             COLUMN-LABEL "Release Qty"
              v-date            COLUMN-LABEL "Date"
                                FORMAT "99/99/9999"
              tt-report.key-06  COLUMN-LABEL "T"
                                FORMAT "x(1)" 
              v-price           COLUMN-LABEL "Unit Pr"   
                                FORMAT ">,>>9.99<<" 
              v-value           COLUMN-LABEL "Sales Value"              
          
             with down frame schdrel1N no-box stream-io width 185.

          END. /* IF CUST PART  */

            IF tb_excel THEN  DO:

              IF v-custflg 
                THEN   /* WITH CUST PART */
                  PUT STREAM excel UNFORMATTED
                    '"' oe-ord.cust-no                           '",'
                    '"' v-ship-id                                '",'
                    '"' v-city                                   '",'
                    '"' v-state                                  '",'
                    '"' v-zip                                    '",'
                    '"' v-po-no                                  '",'
                    '"' oe-ord.ord-no                            '",'
                    '"' STRING(v-rel-no,">>9")                   '",'
                    '"' tt-report.key-01                         '",'
                    '"' v-custpart                               '",'
                    '"' STRING(v-qty,"->>,>>>,>>9")              '",'
                    '"' (IF v-date NE ? THEN
                         STRING(v-date,"99/99/9999") ELSE "")    '",'
                    '"' tt-report.key-06                         '",'
                    '"' STRING(v-price,">,>>9.99<<")             '",'
                    '"' STRING(v-value,"->>,>>>,>>9.99")         '",'
                   SKIP.
                ELSE
                  PUT STREAM excel UNFORMATTED
                    '"' oe-ord.cust-no                           '",'
                    '"' v-ship-id                                '",'
                    '"' v-city                                   '",'
                    '"' v-state                                  '",'
                    '"' v-zip                                    '",'
                    '"' v-po-no                                  '",'
                    '"' oe-ord.ord-no                            '",'
                    '"' STRING(v-rel-no,">>9")                   '",'
                    '"' (IF AVAIL oe-rell THEN oe-rell.i-no
                         ELSE oe-rel.i-no)                       '",'
                    '"' tt-report.key-01                         '",'
                    '"' STRING(v-qty,"->>,>>>,>>9")              '",'
                    '"' (IF v-date NE ? THEN
                         STRING(v-date,"99/99/9999") ELSE "")    '",'
                    '"' tt-report.key-06                         '",'
                    '"' STRING(v-price,">,>>9.99<<")             '",'
                    '"' STRING(v-value,"->>,>>>,>>9.99")         '",'
                   SKIP.
            END. /* EXCEL */
        END.
        
    END.
    ELSE DO:        
        /* gdm - 11030808 */
        IF v-noteslg THEN DO:
             display 
                 oe-ord.cust-no    COLUMN-LABEL "Customer"
                 v-ship-id         COLUMN-LABEL "Ship-To"
                 v-fob             COLUMN-LABEL "FOB"
                                      FORMAT "x(5)"
                 v-city            COLUMN-LABEL "City"
                                   FORMAT "x(13)"
                 v-state           COLUMN-LABEL "St"
                 v-zip             COLUMN-LABEL "Zip"
                                   FORMAT "x(5)"
                 v-po-no           COLUMN-LABEL "Customer PO"
                 oe-ord.ord-no     COLUMN-LABEL "Order"
                 v-rel-no          COLUMN-LABEL "R#"
                                   FORMAT ">>9"
                 oe-rell.i-no      COLUMN-LABEL "FG Item#" 
                                   WHEN AVAIL oe-rell 
                 oe-rel.i-no       WHEN NOT AVAIL oe-rell 
                                   @ oe-rell.i-no 
                 v-qty             COLUMN-LABEL "Release Qty"
                 v-date            COLUMN-LABEL "Date"
                                   FORMAT "99/99/9999"
                 tt-report.key-06  COLUMN-LABEL "T"
                                   FORMAT "x(1)"
                 v-qtyOH           COLUMN-LABEL "Qty On Hand"
                 v-notes[1]        COLUMN-LABEL "Notes"
                                   FORMAT "x(30)"  
               with down frame schdrelNT no-box stream-io width 185.

             IF TRIM(v-notes[2]) NE "" 
               THEN PUT UNFORMATTED v-notes[2] SKIP.
             
             IF TRIM(v-notes[3]) NE "" 
                THEN PUT UNFORMATTED v-notes[3] SKIP.


             IF tb_excel THEN
                 PUT STREAM excel UNFORMATTED
                    '"' oe-ord.cust-no                        '",'
                    '"' v-ship-id                             '",'
                    '"' v-fob                                 '",' 
                    '"' v-city                                '",'
                    '"' v-state                               '",'
                    '"' v-zip                                 '",'
                    '"' v-po-no                               '",'
                    '"' oe-ord.ord-no                         '",'
                    '"' STRING(v-rel-no,">>9")                '",'
                    '"' (IF AVAIL oe-rell THEN oe-rell.i-no
                        ELSE oe-rel.i-no)                     '",'
                    '"' STRING(v-qty,"->>,>>>,>>9")           '",'
                    '"' (IF v-date NE ? THEN
                        STRING(v-date,"99/99/9999") ELSE "")  '",'
                    '"' tt-report.key-06                      '",'
                    '"' v-qtyOH                               '",'
                    '"' v-notes[1]                            '",'
                    '"' v-notes[2]                            '",'
                    '"' v-notes[3]                            '",'
                   SKIP.
        END.
        ELSE DO:
            display 
                oe-ord.cust-no    COLUMN-LABEL "Customer"
                v-ship-id         COLUMN-LABEL "Ship-To"
                v-city            COLUMN-LABEL "City"
                                  FORMAT "x(13)"
                v-state           COLUMN-LABEL "St"
                v-zip             COLUMN-LABEL "Zip"
                                  FORMAT "x(5)"
                v-po-no           COLUMN-LABEL "Customer PO"
                oe-ord.ord-no     COLUMN-LABEL "Order"
                v-rel-no          COLUMN-LABEL "R#"
                                  FORMAT ">>9"
                oe-rell.i-no      COLUMN-LABEL "FG Item#" 
                                  WHEN AVAIL oe-rell 
                oe-rel.i-no       WHEN NOT AVAIL oe-rell 
                                  @ oe-rell.i-no 
                v-qty             COLUMN-LABEL "Release Qty"
                v-date            COLUMN-LABEL "Date"
                                  FORMAT "99/99/9999"
                tt-report.key-06  COLUMN-LABEL "T"
                                  FORMAT "x(1)"
                v-price           COLUMN-LABEL "Unit Pr"
                                  FORMAT ">,>>9.99<<"
                v-value           COLUMN-LABEL "Sales Value"               
             with down frame schdrelC no-box stream-io width 132.

            IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                    '"' oe-ord.cust-no                           '",'
                    '"' v-ship-id                                '",'
                    '"' v-city                                   '",'
                    '"' v-state                                  '",'
                    '"' v-zip                                    '",'
                    '"' v-po-no                                  '",'
                    '"' oe-ord.ord-no                            '",'
                    '"' STRING(v-rel-no,">>9")                   '",'
                    '"' (IF AVAIL oe-rell THEN oe-rell.i-no
                        ELSE oe-rel.i-no)                       '",'
                    '"' STRING(v-qty,"->>,>>>,>>9")              '",'
                    '"' (IF v-date NE ? THEN
                        STRING(v-date,"99/99/9999") ELSE "")    '",'
                    '"' tt-report.key-06                         '",'
                    '"' STRING(v-price,">,>>9.99<<")             '",'
                    '"' STRING(v-value,"->>,>>>,>>9.99")         '",'
                SKIP.
        END.        

    END.*/

     
    assign
     v-tot-qty[1] = v-tot-qty[1] + v-qty
     v-tot-val[1] = v-tot-val[1] + v-value .

    /*IF tt-report.chkvalue = TRUE THEN
        ASSIGN*/
        v-qtyOHt[1]  = v-qtyOHt[1]  + v-qtyOH.

    if last-of(tt-report.key-01) then do:
        PUT str-line SKIP.
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
    
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "shipto"   THEN cVarValue = "".
                         WHEN "fob"   THEN cVarValue = "".
                         WHEN "city"  THEN cVarValue = "" .
                         WHEN "st"   THEN cVarValue = "" .
                         WHEN "zip"  THEN cVarValue = "" .
                         WHEN "cust-po"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .

                         WHEN "r"    THEN cVarValue = "" .
                         WHEN "fgitem"   THEN cVarValue = "".
                         WHEN "custpart"   THEN cVarValue = "".
                         WHEN "rel-qty"  THEN cVarValue = STRING(v-tot-qty[1],"->>,>>>,>>9") .
                         WHEN "date"   THEN cVarValue = "" .
                         WHEN "t"  THEN cVarValue = "" .
                         WHEN "qty-hand"   THEN cVarValue = STRING(v-qtyOHt[1],"->>,>>>,>>9") .
                         WHEN "note"  THEN cVarValue = "" .
                         WHEN "unit-pr"  THEN cVarValue = "" .
                         WHEN "sal-val"   THEN cVarValue = STRING(v-tot-val[1],"->>>,>>>,>>9") .
                         WHEN "item-name"  THEN cVarValue = "" .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          SUB TOTALS" substring(cDisplay,21,300) SKIP.
            /*IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.*/

        /*IF v-sort EQ "N" THEN DO:
            /* gdm - 11030808 */
            IF v-noteslg THEN DO:
                /* gdm - 01290902 */
                IF v-custflg THEN DO:

                    UNDERLINE v-qty   
                              v-qtyOH 

                        WITH FRAME schdrel1NTC.

                    DISPLAY                          
                        "          SUB TOTALS" @ v-custpart
                        v-tot-qty[1]          @ v-qty                   
                        v-qtyOHt[1]           @ v-qtyOH 
                       WITH FRAME schdrel1NTC.
                END.
                ELSE DO:
                    UNDERLINE v-qty   
                              v-qtyOH 

                        WITH FRAME schdrel1NTN.

                    DISPLAY
                        "                    SUB TOTALS" @ tt-report.key-01
                        v-tot-qty[1]                     @ v-qty                   
                        v-qtyOHt[1]                      @ v-qtyOH 
                     WITH FRAME schdrel1NTN.
                END.
            END. /* IF NOTES */
            ELSE
            IF NOT v-noteslg THEN DO:
                /* gdm - 01290902 */
                IF v-custflg THEN DO:
                    UNDERLINE v-qty   
                              v-value 

                        WITH FRAME schdrel1C.

                    DISPLAY                          
                        "                     SUB TOTALS" @ v-custpart
                        v-tot-qty[1]                     @ v-qty                   
                        v-tot-val[1]                     @ v-value 
                      WITH FRAME schdrel1C.
                END.
                ELSE DO:
                    UNDERLINE v-qty   
                              v-value

                        WITH FRAME schdrel1N.

                    DISPLAY
                        "                    SUB TOTALS" @ tt-report.key-01
                        v-tot-qty[1]                     @ v-qty           
                        v-tot-val[1]                     @ v-value
                      WITH FRAME schdrel1N.
                END.
            END. /* IF NOT NOTES */

        END. /* SORT = N */        
        ELSE DO: /* OTHER SORT */
            /* gdm - 11030808 */
            IF v-noteslg THEN DO:
                UNDERLINE v-qty
                          v-qtyOH                    
                    WITH FRAME schdrelNT.

                DISPLAY "     SUB TOTALS" @ oe-rell.i-no
                        "   GRAND TOTALS" WHEN v-sort EQ "S" @ oe-rell.i-no
                        v-tot-qty[1]      @ v-qty
                        v-qtyOHt[1]       @ v-qtyOH
                  WITH FRAME schdrelNT.

            END.
            ELSE DO:
                UNDERLINE v-qty
                          v-value
                    WITH FRAME schdrelC.

                DISPLAY "     SUB TOTALS" @ oe-rell.i-no
                        "   GRAND TOTALS" WHEN v-sort EQ "S" @ oe-rell.i-no
                        v-tot-qty[1]      @ v-qty
                        v-tot-val[1]      @ v-value
                    WITH FRAME schdrelC.
            END.
        END.*/



        put skip(1).  

        ASSIGN 
            v-tot-val[2] = v-tot-val[2] + v-tot-val[1]
            v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
            v-qtyOHt[2]  = v-qtyOHt[2]  + v-qtyOHt[1]
            v-tot-val[1] = 0
            v-tot-qty[1] = 0
            v-qtyOHt[1]  = 0.
    end.

    if last(tt-report.key-01) AND v-sort NE "S" then do:

         PUT str-line SKIP.
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
    
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = "" .
                         WHEN "shipto"   THEN cVarValue = "".
                         WHEN "fob"   THEN cVarValue = "".
                         WHEN "city"  THEN cVarValue = "" .
                         WHEN "st"   THEN cVarValue = "" .
                         WHEN "zip"  THEN cVarValue = "" .
                         WHEN "cust-po"   THEN cVarValue = "" .
                         WHEN "ord"  THEN cVarValue = "" .

                         WHEN "r"    THEN cVarValue = "" .
                         WHEN "fgitem"   THEN cVarValue = "".
                         WHEN "custpart"   THEN cVarValue = "".
                         WHEN "rel-qty"  THEN cVarValue = STRING(v-tot-qty[2],"->>,>>>,>>9") .
                         WHEN "date"   THEN cVarValue = "" .
                         WHEN "t"  THEN cVarValue = "" .
                         WHEN "qty-hand"   THEN cVarValue = STRING(v-qtyOHt[2],"->>,>>>,>>9") .
                         WHEN "note"  THEN cVarValue = "" .
                         WHEN "unit-pr"  THEN cVarValue = "" .
                         WHEN "sal-val"   THEN cVarValue = STRING(v-tot-val[2],"->>>,>>>,>>9") .
                         WHEN "item-name"  THEN cVarValue = "" .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          GRAND TOTALS" substring(cDisplay,23,300) SKIP.
            /*IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.*/
        /*IF v-sort EQ "N" THEN DO:
            /* gdm - 11030808 */
            IF v-noteslg THEN DO:
                
                /* gdm - 01290902 */
                IF v-custflg THEN DO:
                    underline v-qty   
                              v-qtyOH

                          with frame schdrel1NTC.

                    display "        GRAND TOTALS" @ v-custpart
                            v-tot-qty[2]          @ v-qty
                            v-qtyOHt[2]           @ v-qtyOH
                        with FRAME schdrel1NTC.
                      
                END.
                ELSE DO:
                    UNDERLINE v-qty   
                              v-qtyOH

                         WITH FRAME schdrel1NTN.

                    DISPLAY "                  GRAND TOTALS" @ tt-report.key-01
                            v-tot-qty[2]      @ v-qty
                           v-qtyOHt[2]        @ v-qtyOH
                      WITH FRAME schdrel1NTN.
                END.
            END.
            ELSE
            IF NOT v-noteslg THEN DO: /* NO NOTES */
                /* gdm - 01290902 */
                IF v-custflg THEN DO:
                    UNDERLINE v-qty
                              v-value 

                       WITH FRAME schdrel1C.
                             
                    DISPLAY "                   GRAND TOTALS" @ v-custpart
                            v-tot-qty[2]                    @ v-qty
                            v-tot-val[2]                    @ v-value
                      WITH FRAME schdrel1C.
                END.
                ELSE DO:
                    UNDERLINE v-qty
                              v-value 

                       WITH FRAME schdrel1N.
                            
                    DISPLAY "                  GRAND TOTALS" @ tt-report.key-01
                            v-tot-qty[2]      @ v-qty
                            v-tot-val[2]      @ v-value
                      WITH FRAME schdrel1N.

                END.
            END. /* NO NOTES */
        END. /* SORT = N */
        ELSE DO: /* OTHER SORT */
            /* gdm - 11030808 */
            IF v-noteslg THEN DO:
                UNDERLINE v-qty
                          v-qtyOH
                  WITH FRAME schdrelNT.

                display "   GRAND TOTALS" @ oe-rell.i-no
                        v-tot-qty[2]      @ v-qty
                        v-qtyOHt[2]       @ v-qtyOH
                   WITH FRAME schdrelNT.
            END.
            ELSE DO:
                UNDERLINE v-qty
                          v-value
                    WITH FRAME schdrelC.

                DISPLAY "   GRAND TOTALS" @ oe-rell.i-no
                        v-tot-qty[2]      @ v-qty
                        v-tot-val[2]      @ v-value
                    WITH FRAME schdrelC.
            END.
        END.*/

    end.

    delete tt-report.
  end. /* each tt-report */
