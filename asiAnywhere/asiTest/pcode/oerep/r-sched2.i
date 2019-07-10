  
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
      NO-LOCK:
     IF LOOKUP(oe-ord.cust-no, custcount) = 0 THEN NEXT.

    RUN oe/cleanrel.p (ROWID(oe-ordl)).

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
           tt-report.rec-id  = recid(oe-rel).
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

IF LOOKUP(oe-relh.cust-no, custcount) = 0 THEN NEXT.

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
         tt-report.rec-id  = recid(oe-rell).
      END.
    END.

    /* new start */
    END.
    /* new end */
  end.

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
       v-qty     = oe-rel.qty 
       v-date    = oe-rel.rel-date
       v-po-no   = oe-rel.po-no
       v-rel-no  = oe-rel.rel-no
       v-ship-id = oe-rel.ship-id.
       
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

    IF v-price EQ ? THEN v-price = 0.

    v-value = v-price * v-qty.
     
    IF v-sort EQ "N" THEN DO:
      display oe-ord.cust-no    column-label "Customer"
              v-ship-id         column-label "Ship-To"
              v-city            column-label "City"
                                FORMAT "x(13)"
              v-state           column-label "St"
              v-zip             column-label "Zip"
                                format "x(5)"
              v-po-no           column-label "Customer PO"
              oe-ord.ord-no     column-label "Order"
              v-rel-no          column-label "R#"
                                format ">>9"
              oe-rell.i-no      column-label "FG Item#"
                                when avail oe-rell
              oe-rel.i-no       when not avail oe-rell
                                @ oe-rell.i-no
              tt-report.key-01  column-label "FG Item Name"
                                format "x(30)" 
              v-qty             column-label "Release Qty"
              v-date            column-label "Date"
                                format "99/99/9999"
              tt-report.key-06  column-label "T"
                                format "x(1)"
              v-price           column-label "Unit Pr"
                                format ">,>>9.99<<"
              v-value           column-label "Sales Value"
            
          with down frame schdrel2 no-box stream-io width 180.

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
           '"' tt-report.key-01                         '",'
           '"' STRING(v-qty,"->>,>>>,>>9")              '",'
           '"' (IF v-date NE ? THEN
                STRING(v-date,"99/99/9999") ELSE "")    '",'
           '"' tt-report.key-06                         '",'
           '"' STRING(v-price,">,>>9.99<<")             '",'
           '"' STRING(v-value,"->>,>>>,>>9.99")         '",'
          SKIP.
    END.

    ELSE DO:
      display oe-ord.cust-no    column-label "Customer"
              v-ship-id         column-label "Ship-To"
              v-city            column-label "City"
                                FORMAT "x(13)"
              v-state           column-label "St"
              v-zip             column-label "Zip"
                                format "x(5)"
              v-po-no           column-label "Customer PO"
              oe-ord.ord-no     column-label "Order"
              v-rel-no          column-label "R#"
                                format ">>9"
              oe-rell.i-no      column-label "Item"
                                when avail oe-rell
              oe-rel.i-no       when not avail oe-rell
                                @ oe-rell.i-no
              v-qty             column-label "Release Qty"
              v-date            column-label "Date"
                                format "99/99/9999"
              tt-report.key-06  column-label "T"
                                format "x(1)"
              v-price           column-label "Unit Pr"
                                format ">,>>9.99<<"
              v-value           column-label "Sales Value"
            
          with down frame schdrel no-box stream-io width 132.

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

    assign
     v-tot-qty[1] = v-tot-qty[1] + v-qty
     v-tot-val[1] = v-tot-val[1] + v-value.

    if last-of(tt-report.key-01) then do:
      IF v-sort EQ "N" THEN DO:
        underline v-qty
                  v-value
                
            with frame schdrel2.
      
        display "     SUB TOTALS" @ oe-rell.i-no 
                v-tot-qty[1]      @ v-qty
                v-tot-val[1]      @ v-value
            
          with frame schdrel2.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' "SUB TOTALS"                       '",'
             '"' STRING(v-tot-qty[1],"->>,>>>,>>9") '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' STRING(v-tot-val[1],"->>,>>>,>>9.99") '",'
            SKIP.
      END.

      ELSE DO:
        underline v-qty
                  v-value
                
            with frame schdrel.
      
        display "     SUB TOTALS" @ oe-rell.i-no
                "   GRAND TOTALS" WHEN v-sort EQ "S" @ oe-rell.i-no
                v-tot-qty[1]      @ v-qty
                v-tot-val[1]      @ v-value
            
          with frame schdrel.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' (IF v-sort NE "S" THEN "SUB TOTALS"
                  ELSE "GRAND TOTALS")              '",'
             '"' STRING(v-tot-qty[1],"->>,>>>,>>9") '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' STRING(v-tot-val[1],"->>,>>>,>>9.99") '",'
            SKIP.
      END.
        
      put skip(1).  
    
      assign
       v-tot-val[2] = v-tot-val[2] + v-tot-val[1]
       v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
       v-tot-val[1] = 0
       v-tot-qty[1] = 0.
    end.

    if last(tt-report.key-01) AND v-sort NE "S" then do:
      IF v-sort EQ "N" THEN DO:
        underline v-qty
                  v-value
                
            with frame schdrel2.
      
        display "   GRAND TOTALS" @ oe-rell.i-no
                v-tot-qty[2]      @ v-qty
                v-tot-val[2]      @ v-value
            
            with frame schdrel2.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' "GRAND TOTALS"                     '",'
             '"' STRING(v-tot-qty[2],"->>,>>>,>>9") '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' STRING(v-tot-val[2],"->>,>>>,>>9.99") '",'
            SKIP.
      END.

      ELSE DO:
        underline v-qty
                  v-value
                
            with frame schdrel.
      
        display "   GRAND TOTALS" @ oe-rell.i-no
                v-tot-qty[2]      @ v-qty
                v-tot-val[2]      @ v-value
            
            with frame schdrel.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' "GRAND TOTALS"                     '",'
             '"' STRING(v-tot-qty[2],"->>,>>>,>>9") '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' ""                                 '",'
             '"' STRING(v-tot-val[2],"->>,>>>,>>9.99") '",'
            SKIP.
      END.
    end.

    delete tt-report.
  end. /* each tt-report */
