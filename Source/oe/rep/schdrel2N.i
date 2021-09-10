/* oe/rep/schdrel2N.i*/
  
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER bf-oe-rell FOR oe-rell .
DEF VAR lv-qty LIKE oe-rell.qty NO-UNDO.
DEF VAR lv-subt AS CHAR NO-UNDO.
DEF VAR lv-cr-rating LIKE cust.cr-rating NO-UNDO.
DEF VAR ll-show-top-only AS LOG NO-UNDO.
DEF VAR ld-palls AS DEC NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF BUFFER bw-ord FOR w-ord.
DEF VAR v-prt-totalQty AS logic NO-UNDO.  
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR v-del-zone AS cha FORM "x(8)" NO-UNDO.
DEF VAR v-terr AS cha FORM "x(4)" NO-UNDO.
DEF VAR v-crRate AS cha NO-UNDO.
DEF VAR v-sb AS cha NO-UNDO.
DEF VAR v-routing AS cha NO-UNDO.
DEF VAR v-oh-relqty AS DEC NO-UNDO.
DEF VAR v-ship-add1 AS CHAR NO-UNDO .
DEF VAR v-ship-add2 AS CHAR NO-UNDO .
DEF VAR v-ship-city AS CHAR NO-UNDO .
DEF VAR v-ship-stat AS CHAR NO-UNDO .
DEF VAR v-ship-zip AS CHAR NO-UNDO .
DEF VAR v-ship-name AS CHAR NO-UNDO .
DEF VAR v-fg-cat AS CHAR INIT "" NO-UNDO .
DEFINE VARIABLE lSelected AS LOGICAL INIT YES NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .
DEFINE VARIABLE cRelDueDate AS CHARACTER NO-UNDO.

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

/*
  FORM HEADER
      SKIP(1)
    WITH FRAME r-top.
  */

  DEF VAR str-tit4 AS cha NO-UNDO.
  DEF VAR str-tit5 AS cha NO-UNDO.

  /*{sys/form/r-top5DL.f} */
  {sys/form/r-top.i}
{sys/inc/ctrtext.i str-tit 112}.
//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

form header 
     skip(1)
     day_str
     str-tit format "x(112)"
     "Page" at 123
     page-number format ">>9"
     skip
     tim_str
     str-tit2 format "x(112)"
     skip
     str-tit3 format "x(112)"
     skip(1)
     str-tit4 format "x(299)"
     skip
     str-tit5 format "x(299)"
     skip(1)
     with frame r-top100 row 1 column 1 stream-io width 300
     no-labels no-box no-underline .


      /*
  FORM HEADER
      "Credit Rating:"
      lv-cr-rating
      SKIP(1)
    WITH FRAME r-top2 PAGE-TOP NO-ATTR-SPACE NO-BOX WIDTH 180 STREAM-IO.
        */

  SESSION:SET-WAIT-STATE ("general").

  ASSIGN v-tot-qty = 0
         v-tot-msf = 0
         v-tot-val = 0.

  assign
   str-tit2 = c-win:title
   {sys/inc/ctrtext.i str-tit2 112}

   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-fsman[1]   = begin_slsmn
   v-fsman[2]   = end_slsmn
   v-ford-no[1] = begin_ord-no
   v-ford-no[2] = end_ord-no
   v-fitem[1]   = begin_i-no
   v-fitem[2]   = end_i-no
   v-floc[1]    = begin_loc
   v-floc[2]    = end_loc
   v-fshipfrom[1] = begin_shipfrom
   v-fshipfrom[2] = end_shipfrom
   v-fdate[1]   = begin_date
   v-fdate[2]   = end_date
   v-fcarr[1]   = begin_carr
   v-fcarr[2]   = end_carr
   v-ponum      = tb_po-no
   lSelected  = tb_cust-list 
   v-sort       = if rd_sort eq "Customer#"     then "C"  else
                  if rd_sort eq "Release Date"  then "R"  else
                  if rd_sort eq "Item#"         then "I"  else
                  if rd_sort eq "Item Name"     then "N"  else
                  if rd_sort eq "Territory"     then "T"  else
                  if rd_sort eq "Credit Rating" then "CR" else "A"
   v-print      = substr(rd_print,1,1)
   v-qty-opt    = /*rs_qty*/
                  IF CAN-DO(cSelectedList,"Job Qty OH") THEN "JobQty" ELSE "JOB#" 
   v-prt-totalQty  =  CAN-DO(cSelectedList,"Tot Qty OH") 
   v-types      = string(tb_posted,"P/")      + string(tb_actual,"A/")      +
                  string(tb_late,"L/")        + string(tb_scheduled,"S/")   +
                  string(tb_backordered,"B/") + string(tb_invoiceable,"I/") +
                  string(tb_completed,"C/")   + string(tb_invoice,"Z/")

   str-tit3 = if v-sort eq "C"  then "By Customer By Date"      else
              if v-sort eq "R"  then "By Date By Customer"      else
              if v-sort eq "I"  then "By Item By Date"          else
              if v-sort eq "N"  then "By Item Name By Date"     else
              if v-sort eq "A"  then "By Carrier By Date"       else
              if v-sort eq "CR" then "By Credit Rating By Date" else
                                     "By Territory By Date"
  {sys/inc/ctrtext.i str-tit3 132}.
   
  FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:
     IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
       THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
              str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        
     ELSE 
      ASSIGN str-tit4 = str-tit4 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
           ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".  
  END.

  IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN v-fcust[1] = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN v-fcust[2] = ttCustList.cust-no .
 END.

  
  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

  IF rd-dest EQ 3 THEN 
  DO:
  
    OUTPUT STREAM excel TO VALUE(cFileName).
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
    
  END.

  /*display "" /*str-tit4 str-tit5*/ with frame r-top.*/
  VIEW FRAME r-top100.

  EMPTY TEMP-TABLE tt-report.

  FOR EACH w-ord:
      DELETE w-ord.
  END.

  ASSIGN v-oh-relqty = 0 .
  
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
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                         USE-INDEX ord-no)
      USE-INDEX opened NO-LOCK,
      FIRST b-itemfg WHERE
            b-itemfg.company EQ cocode AND
            b-itemfg.i-no    EQ oe-ordl.i-no AND
            b-itemfg.procat  GE begin_cat AND
            b-itemfg.procat  LE end_cat
            NO-LOCK,
      FIRST oe-ord
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND oe-ord.cust-no GE v-fcust[1]
        AND oe-ord.cust-no LE v-fcust[2]
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq oe-ord.cust-no
        AND ttCustList.log-fld no-lock) else true)
        AND oe-ord.csrUser_id GE begin_csr
        AND oe-ord.csrUser_id LE end_csr
      NO-LOCK,
      
      FIRST cust
      WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
      NO-LOCK:

    STATUS DEFAULT "Processing Order#/FG#: " +
                   TRIM(STRING(oe-ordl.ord-no,">>>>>>>>")) + "/" +
                   TRIM(oe-ordl.i-no).

/*     RUN oe/cleanrel.p (ROWID(oe-ordl)). */

    for each oe-rel no-lock
        where oe-rel.company   eq oe-ordl.company
          and oe-rel.ord-no    eq oe-ordl.ord-no
          and oe-rel.i-no      eq oe-ordl.i-no
          and oe-rel.line      eq oe-ordl.line
          and oe-rel.rel-date  ge v-fdate[1]
          and oe-rel.rel-date  le v-fdate[2]
          and oe-rel.carrier   ge v-fcarr[1]
          and oe-rel.carrier   le v-fcarr[2]
          AND oe-rel.spare-char-1 GE v-fshipfrom[1]
          AND oe-rel.spare-char-1 LE v-fshipfrom[2]
        use-index ord-item:
      
      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

      if index("AB",v-type) gt 0 then next.
    
      if index(v-types,v-type) gt 0 then do:    
        create tt-report.
        assign
         tt-report.term-id = ""
         tt-report.key-01  = if v-sort eq "R" then
                               (string(year(oe-rel.rel-date),"9999") +
                                string(month(oe-rel.rel-date),"99")  +
                                string(day(oe-rel.rel-date),"99"))
                             else
                             if v-sort eq "N" then oe-ordl.i-name
                             else ""
         tt-report.key-02  = if v-sort eq "I" or v-sort eq "D" then oe-rel.i-no
                             else
                             if v-sort eq "T" then cust.terr
                             else
                             if v-sort eq "A" then oe-rel.carrier
                             else
                             if v-sort eq "CR" then cust.cr-rating
                             else oe-rel.cust-no
         tt-report.key-03  = if v-sort ne "R" then
                               (string(year(oe-rel.rel-date),"9999") +
                                string(month(oe-rel.rel-date),"99")  +
                                string(day(oe-rel.rel-date),"99"))
                             else ""
         tt-report.key-04  = string(if v-sort eq "A" then oe-rel.cust-no
                                                     else " ","x(10)") +
                             string(oe-ord.ord-no,"9999999999")
         tt-report.key-05  = string(index(v-types,v-type),"99")
         tt-report.key-06  = v-type
         tt-report.key-07  = "No"
         tt-report.rec-id  = recid(oe-rel).
      end.
    end.
    
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
          AND oe-rell.loc GE v-fshipfrom[1]
          AND oe-rell.loc LE v-fshipfrom[2]
        USE-INDEX ord-no,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.rel-date GE v-fdate[1]
          AND oe-relh.rel-date LE v-fdate[2]
          AND oe-relh.carrier  GE v-fcarr[1]
          AND oe-relh.carrier  LE v-fcarr[2]
        USE-INDEX r-no

      BREAK BY oe-rell.r-no
            BY oe-rell.ord-no
            BY oe-rell.i-no
            BY oe-rell.line
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.
  
    IF LAST-OF(oe-rell.po-no) THEN DO:  
      create tt-report.
      assign
       tt-report.term-id = ""
       tt-report.key-01  = if v-sort eq "R" then
                             (string(year(oe-relh.rel-date),"9999") +
                              string(month(oe-relh.rel-date),"99")  +
                              string(day(oe-relh.rel-date),"99"))
                           else
                           if v-sort eq "N" then oe-ordl.i-name
                           else ""
       tt-report.key-02  = if v-sort eq "I" or v-sort eq "D" then oe-rell.i-no
                           else
                           if v-sort eq "T" then cust.terr
                           else
                           if v-sort eq "A" then oe-relh.carrier
                           else
                           if v-sort eq "CR" then cust.cr-rating
                           else oe-relh.cust-no
       tt-report.key-03  = if v-sort ne "R" then
                             (string(year(oe-relh.rel-date),"9999") +
                              string(month(oe-relh.rel-date),"99")  +
                              string(day(oe-relh.rel-date),"99"))
                           else ""
       tt-report.key-04  = string(if v-sort eq "A" then oe-relh.cust-no
                                                   else " ","x(10)") +
                           string(oe-ord.ord-no,"9999999999")
       tt-report.key-05  = string(index(v-types,v-type),"99")
       tt-report.key-06  = if oe-rell.b-ord-no eq 0 then "A" else "B"
       tt-report.qty     = lv-qty
       tt-report.rec-id  = recid(oe-rell)
       tt-report.key-07  = IF oe-relh.printed THEN "Yes" ELSE "No" .
    END.

    /* new start */
    END.
    /* new end */
  end.

  STATUS DEFAULT "Printing...".

  RELEASE tt-report.

  for each tt-report where tt-report.term-id eq ""
      AND ((tt-report.key-07 EQ "Yes" AND rd_printed EQ "Yes") OR
          (tt-report.key-07 EQ "No" AND rd_printed EQ "No") OR  rd_printed EQ "All")
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04:

   
      IF FIRST(tt-report.key-01) THEN do:
          /*VIEW FRAME r-top.*/
          /*PAGE.*/
        
      END.
    release oe-rel.
    release oe-rell.
    release oe-relh.
    release oe-ord.
    release oe-ordl.

    find first oe-rel 
        where recid(oe-rel) eq tt-report.rec-id 
        no-lock no-error.

    v-rel-no = 0.
    v-shipfrom = "".
    if avail oe-rel then do:
      v-shipfrom = oe-rel.spare-char-1.
      FOR EACH oe-rell NO-LOCK
          WHERE oe-rell.company  EQ cocode
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
          USE-INDEX ord-no,
          FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK:

        ASSIGN v-rel-no   = oe-relh.release#
               v-shipfrom = oe-rell.loc
               v-dockTime = oe-relh.releaseDockTime .

        IF oe-relh.posted EQ NO AND oe-relh.deleted EQ NO THEN
          tt-report.rec-id = recid(oe-rell).
        ELSE RELEASE oe-relh.

        LEAVE.
      END.
       
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rel.ord-no
            and oe-ordl.i-no    eq oe-rel.i-no
            and oe-ordl.line    eq oe-rel.line
          no-lock.
    end.
    
    IF NOT AVAIL oe-rell THEN
        find oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock no-error.

    if avail oe-rell then do:    
      if index("SLI",tt-report.key-06) gt 0 then
        tt-report.key-06 = if oe-rell.b-ord-no eq 0 then "A" else "B" .

      find first oe-relh
          where oe-relh.company eq cocode
            and oe-relh.r-no    eq oe-rell.r-no
          use-index r-no no-lock.
      ASSIGN v-rel-no = IF AVAIL oe-relh THEN oe-relh.release# ELSE v-rel-no
             v-shipfrom = oe-rell.loc.
             v-dockTime = IF AVAIL oe-relh THEN oe-relh.releaseDockTime ELSE v-dockTime.

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rell.ord-no
            and oe-ordl.i-no    eq oe-rell.i-no
            and oe-ordl.line    eq oe-rell.line
          no-lock.
    end.

    find first oe-ord of oe-ordl no-lock no-error.
    
    if avail oe-ord then
    find first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-ord.cust-no
        no-lock no-error.

    if avail oe-relh THEN do:
        v-qty = 0 .
        FOR EACH  bf-oe-rell NO-LOCK    /* task 07151504  */
          WHERE bf-oe-rell.company  EQ oe-rell.company
            AND bf-oe-rell.ord-no   EQ oe-rell.ord-no
            AND bf-oe-rell.rel-no   EQ oe-rell.rel-no
            AND bf-oe-rell.b-ord-no EQ oe-rell.b-ord-no
            AND bf-oe-rell.i-no     EQ oe-rell.i-no 
            AND bf-oe-rell.line     EQ oe-rell.line
          USE-INDEX ord-no :
           v-qty  = v-qty + bf-oe-rell.qty .
        END.

      assign
       /*v-qty     = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty*/
       v-date    = oe-relh.rel-date 
       v-po-no   = oe-rell.po-no
       v-ship-id = oe-relh.ship-id
       v-carrier = oe-relh.carrier
       lPrinted  = oe-relh.printed.
    END.
    else
    if avail oe-rel then
      assign
       v-qty     = IF oe-rel.link-no eq 0  THEN oe-rel.tot-qty ELSE oe-rel.qty 
       v-date    = oe-rel.rel-date
       v-po-no   = oe-rel.po-no
       v-ship-id = oe-rel.ship-id
       v-carrier = oe-rel.carrier
       lPrinted  = NO.
       
       cRelDueDate = IF AVAIL oe-rel THEN STRING(ENTRY(1, oe-rel.spare-char-4)) ELSE "".

    create w-ord.

    if avail oe-ordl then do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock.

      assign
       w-ord.ord-no        = oe-ord.ord-no
       w-ord.cust-no       = oe-ord.cust-no
       w-ord.cust-name     = oe-ord.cust-name
       w-ord.part-no       = oe-ordl.part-no
       w-ord.i-no          = oe-ordl.i-no
       w-ord.i-name        = oe-ordl.i-name
       w-ord.qty           = oe-ordl.qty
       w-ord.cost          = oe-ordl.cost
       w-ord.price         = oe-ordl.price        
       w-ord.rel-qty       = v-qty  
       w-ord.t-price       = (oe-ordl.t-price / oe-ordl.qty) * w-ord.rel-qty
       w-ord.rel-date      = string(v-date) + tt-report.key-06
       w-ord.xls-rel-date  = v-date
       w-ord.xls-status    = tt-report.key-06
       w-ord.rel-no        = v-rel-no
       w-ord.ship-id       = v-ship-id
       w-ord.job-no        = oe-ordl.job-no
       w-ord.job-no2       = oe-ordl.job-no2
       w-ord.job           = if w-ord.job-no eq "" then "" else
                               (trim(w-ord.job-no) + "-" +
                                string(w-ord.job-no2,"99"))
       w-ord.po-num        = v-po-no
       w-ord.ord-qty       = oe-ordl.qty
       w-ord.due-date   = string(oe-ordl.req-date)
       w-ord.shp-qty       = oe-ordl.ship-qty
       w-ord.msf           = w-ord.rel-qty * itemfg.t-sqft / 1000
       w-ord.prom-code     = oe-ordl.prom-code
       w-ord.last-date     = oe-ord.last-date
       w-ord.carrier       = v-carrier 
       w-ord.ship-from     = v-shipfrom
       w-ord.sman          = oe-ordl.s-man[1] 
       w-ord.upd-user      = oe-ord.USER-ID
       w-ord.is-a-component = oe-ordl.is-a-component
       ld-palls            =  w-ord.rel-qty / 
                              ((IF oe-ordl.cas-cnt    EQ 0 THEN 1 ELSE oe-ordl.cas-cnt) *
                              (IF oe-ordl.cases-unit EQ 0 THEN 1 ELSE oe-ordl.cases-unit))
       w-ord.ord-date     = string(oe-ord.ord-date)
       w-ord.prom-date         = oe-ordl.prom-date
       w-ord.csrUser_id        = oe-ord.csrUser_id
       w-ord.entered-id        = oe-ord.entered-id 
       w-ord.ord-due-date      = IF oe-ord.due-date NE ? THEN string(oe-ord.due-date) ELSE ""  
       w-ord.rel-due-date      = IF cRelDueDate NE ? THEN cRelDueDate ELSE "" 
       w-ord.Printed           = lPrinted 
       w-ord.promiseDate       = IF oe-ord.promiseDate NE ? THEN  string(oe-ord.promiseDate) ELSE ""
       w-ord.priority          = oe-ord.priority
       w-ord.pr-uom            = oe-ordl.pr-uom
       .

      {sys/inc/roundup.i ld-palls}

      IF ld-palls LT 0 THEN ld-palls = ld-palls * -1.

      w-ord.palls = w-ord.palls + ld-palls.

      IF NOT FIRST-OF(tt-report.key-02) AND v-sort EQ "C" THEN w-ord.cust-name = "".
      
      IF lGetVendorPOInfo THEN
      RUN pGetVendorPOInfo (
        oe-ordl.po-no-po,
        oe-ordl.job-no,
        oe-ordl.job-no2,
        OUTPUT w-ord.vend-id,
        OUTPUT w-ord.vend-name,
        OUTPUT w-ord.vend-po,
        OUTPUT w-ord.po-due-date,
        OUTPUT w-ord.po-rm-item,
        OUTPUT w-ord.po-rm-item-name,
        OUTPUT w-ord.po-uom,
        OUTPUT w-ord.po-ord-qty,
        OUTPUT w-ord.po-rec-qty
        ).

      IF v-comps AND itemfg.isaset THEN DO:
        RUN fg/fullset.p (ROWID(itemfg)).

        FOR EACH tt-fg-set,
            FIRST itemfg
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ tt-fg-set.part-no
            NO-LOCK:

          CREATE b-w-ord.
          BUFFER-COPY w-ord TO b-w-ord
          ASSIGN
           b-w-ord.component = 1
           b-w-ord.cust-name = ""
           b-w-ord.part-no   = itemfg.part-no
           b-w-ord.i-no      = tt-fg-set.part-no
           b-w-ord.i-name    = itemfg.i-name
           b-w-ord.price     = 0
           b-w-ord.cost      = 0
           b-w-ord.t-price   = 0
           b-w-ord.job       = ""
           b-w-ord.po-num    = ""
           b-w-ord.qty       = w-ord.qty     * tt-fg-set.part-qty-dec
           b-w-ord.rel-qty   = w-ord.rel-qty * tt-fg-set.part-qty-dec
           b-w-ord.ord-qty   = w-ord.ord-qty * tt-fg-set.part-qty-dec
           b-w-ord.shp-qty   = w-ord.shp-qty * tt-fg-set.part-qty-dec
           b-w-ord.msf       = b-w-ord.rel-qty * itemfg.t-sqft / 1000.
        END.
      END.
    END.

    FOR EACH w-ord
        BREAK BY w-ord.component
              BY w-ord.i-no:

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-ord.i-no
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg OR ll-show-top-only THEN DO:
        if v-qty-opt EQ "JobQty" and w-ord.component eq 0 AND w-ord.job-no <> "" then
        for each fg-bin FIELDS(qty)
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq w-ord.i-no
              and fg-bin.job-no  eq w-ord.job-no
              and fg-bin.job-no2 eq w-ord.job-no2
              AND fg-bin.loc     GE v-floc[1]
              AND fg-bin.loc     LE v-floc[2]
            use-index job no-lock:
          w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
        end.
        /*else /* w-ord.onh-qty = itemfg.q-onh. */*/
       /* IF v-prt-totalQty THEN */
        for each fg-bin FIELDS(qty)
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq w-ord.i-no
              AND fg-bin.loc     GE v-floc[1]
              AND fg-bin.loc     LE v-floc[2]
            use-index i-no no-lock:
          w-ord.tot-qty = w-ord.tot-qty + fg-bin.qty.
        end.

        ASSIGN v-oh-relqty = w-ord.tot-qty - w-ord.rel-qty .
        
        IF tb_neg-avail THEN DO:
           IF itemfg.q-avail GE 0 THEN NEXT.
        END.
        IF tb_oh-rlqty THEN DO:
           IF v-oh-relqty GE 0 THEN NEXT.
        END.


        BUFFER bw-ord:FIND-BY-ROWID(ROWID(w-ord), NO-LOCK) .
        find cust WHERE cust.company EQ cocode
                     AND cust.cust-no EQ w-ord.cust-no NO-LOCK NO-ERROR.
        IF AVAIL cust THEN ASSIGN v-crRate = cust.cr-rating
                                  v-terr = cust.terr
                                  v-del-zone = cust.del-zone.
        ELSE ASSIGN v-crRate = ""
                    v-terr = ""
                    v-del-zone = "".
        FIND FIRST shipto WHERE shipto.company = cocode
                            AND shipto.cust-no = w-ord.cust-no
                            AND shipto.ship-id = w-ord.ship-id NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN do:
            ASSIGN v-del-zone = shipto.dest-code
                v-ship-add1 = shipto.ship-addr[1]
                v-ship-add2 = shipto.ship-addr[2]
                v-ship-city = shipto.ship-city
                v-ship-stat =shipto.ship-state
                v-ship-zip = shipto.ship-zip
                v-ship-name = shipto.ship-name  .
        END.
        ELSE 
            ASSIGN v-del-zone = ""
                v-ship-add1 = ""
                v-ship-add2 = ""
                v-ship-city = ""
                v-ship-stat = ""
                v-ship-zip = ""
                v-ship-name = ""  .


        {oe/rep/schdrelN.i}
      END.
    END.
    
    IF NOT ll-show-top-only THEN DO:
      FIND FIRST w-ord.
      /* moved to oe/rep/schdrelN.i 
      assign
       v-tot-qty[1] = v-tot-qty[1] + 1
       v-tot-msf[1] = v-tot-msf[1] + w-ord.msf
       v-tot-val[1] = v-tot-val[1] + w-ord.t-price.
      */
      IF LAST-OF(tt-report.key-02) THEN DO:
        IF v-sort EQ "CR" OR (tb_subt AND v-sort EQ "C") THEN DO:
          lv-subt = CAPS(rd_sort).
          IF v-print EQ "S" THEN
            PUT SKIP(1)
                FILL(" ",26 - LENGTH(TRIM(lv-subt) + " SALES VALUE:")) +
                    TRIM(lv-subt) + " SALES VALUE: " FORMAT "x(27)" TO 115
                v-tot-val[1] TO 130.

          IF chosen NE 2 OR v-print EQ "S" THEN
            PUT SKIP(1)
                FILL(" ",18 - LENGTH(TRIM(lv-subt) + " MSF:")) +
                TRIM(lv-subt) + " MSF: " FORMAT "x(19)" TO 115
                v-tot-msf[1] TO 130.

          PUT SKIP(1)
              FILL(" ",35 - LENGTH(TRIM(lv-subt) + " NUMBER OF RELEASES:")) +
                  TRIM(lv-subt) + " NUMBER OF RELEASES: " FORMAT "x(36)" TO 115
              v-tot-qty[1] TO 130
              SKIP(1).
        END.

        assign
         v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
         v-tot-val[2] = v-tot-val[2] + v-tot-val[1]
         v-tot-msf[2] = v-tot-msf[2] + v-tot-msf[1]
         v-tot-qty[1] = 0
         v-tot-val[1] = 0
         v-tot-msf[1] = 0.
      end.

      if last(tt-report.key-01) then do:
        if v-print eq "S" then
          put skip(1)
              "TOTAL SALES VALUE: " to 115
              v-tot-val[2] to 130.

        if chosen ne 2 or v-print eq "S" then
          put skip(1)
              "TOTAL MSF: " to 115
              v-tot-msf[2] to 130.

        put skip(1)
            "TOTAL NUMBER OF RELEASES: " to 115
            v-tot-qty[2] to 130
            skip(1).
      end.
    END.

    FOR EACH w-ord:
      DELETE w-ord.
    END.
  end. /* each tt-report */

  STATUS DEFAULT "".

  SESSION:SET-WAIT-STATE ("").

  IF rd-dest EQ 3 THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_OpenCSV:CHECKED THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
  END.
