/* ----------------------------------------------- ar/rep/pastdue.i 11/02 JLF */
/* A/R Past Due Receivables Report Program - A/R Module                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{sys/inc/ttRptSel.i}
{ar/rep/pastdue1.i}
{sys/ref/CustList.i}
DEFINE SHARED VARIABLE det-rpt2           AS LOG       NO-UNDO.
DEFINE        VARIABLE v-cr-db-amt        AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE        VARIABLE v-disc-amt         AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE        VARIABLE v-type             AS CHARACTER FORMAT "x(2)".
DEFINE        VARIABLE v-first-cust       AS LOGICAL.
DEFINE        VARIABLE d                  AS INTEGER   LABEL "Days".
DEFINE        VARIABLE ni                 AS INTEGER.
DEFINE        VARIABLE cust-t             AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99".
DEFINE        VARIABLE sman-t             AS DECIMAL   EXTENT 6 FORMAT "->,>>>,>>>,>>9.99".
DEFINE        VARIABLE onacc              AS DECIMAL.
DEFINE        VARIABLE s                  AS INTEGER.
DEFINE        VARIABLE ag                 AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE        VARIABLE amt                LIKE ag.
DEFINE        VARIABLE paid-amt           LIKE ag.
DEFINE        VARIABLE c1                 AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99".
DEFINE        VARIABLE m1                 AS CHARACTER FORMAT "x(20)".
DEFINE        VARIABLE m2                 AS CHARACTER FORMAT "x(20)".
DEFINE        VARIABLE m3                 AS CHARACTER FORMAT "x(20)".
DEFINE        VARIABLE save_id            AS RECID.
DEFINE        VARIABLE unapp              LIKE cust-t.
DEFINE        VARIABLE first-unapp        AS LOG       INIT YES.
DEFINE        VARIABLE tmp-var            AS CHARACTER FORMAT "x(20)".  /* DAR */
DEFINE        VARIABLE v-disc-type        AS CHARACTER FORMAT "x(4)".
DEFINE        VARIABLE v-sman             AS CHARACTER FORMAT "x(21)".
DEFINE        VARIABLE v-dec              AS DECIMAL   EXTENT 4 NO-UNDO.
DEFINE        VARIABLE v-cr-lim           LIKE cust.cr-lim NO-UNDO.
DEFINE SHARED VARIABLE lSelected          AS LOG       INIT YES NO-UNDO.

DEFINE SHARED VARIABLE cSelectedList      AS cha       NO-UNDO.
DEFINE SHARED VARIABLE str-line           AS cha       FORM "x(300)" NO-UNDO.
DEFINE        VARIABLE cDisplay           AS cha       NO-UNDO.
DEFINE        VARIABLE cExcelDisplay      AS cha       NO-UNDO.
DEFINE        VARIABLE hField             AS HANDLE    NO-UNDO.
DEFINE        VARIABLE cTmpField          AS CHA       NO-UNDO.
DEFINE        VARIABLE cVarValue          AS cha       NO-UNDO.
DEFINE        VARIABLE cExcelVarValue     AS cha       NO-UNDO.

DEFINE SHARED VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE cTextListToSelect  AS cha       NO-UNDO.
DEFINE SHARED VARIABLE cFieldListToSelect AS cha       NO-UNDO.
DEFINE SHARED VARIABLE cFieldLength       AS cha       NO-UNDO.
DEFINE SHARED VARIABLE cFieldType         AS cha       NO-UNDO.
DEFINE SHARED VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE SHARED BUFFER b-itemfg FOR itemfg .
DEFINE SHARED VARIABLE cTextListToDefault AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR cslist AS cha NO-UNDO.
/*{sys/form/r-top5L3.f} */

/* 21685 Frame r-top no longer shared, so set up title here */
/*DEFINE SHARED FRAME r-top. */
FORM HEADER "" WITH FRAME r-top.

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

    IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
        THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
             .        
    ELSE 
        ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            
            .        
    cSlist = cSlist + ttRptSelected.FieldList + ",".


    IF LOOKUP(ttRptSelected.TextList, "InvoiceAmt,Past Due Amt") <> 0    THEN
        ASSIGN
            str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
    ELSE
        str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .

END.
/*{sys/form/r-top3.f}*/
{sys/form/r-top5DL2.f} 
VIEW FRAME r-top.
FOR EACH cust
    WHERE cust.company EQ cocode
    AND cust.cust-no GE v-s-cust
    AND cust.cust-no LE v-e-cust
    AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
    AND ttCustList.log-fld NO-LOCK) ELSE TRUE)  /*v-s-cust*/
    /* and cust.cust-no le v-e-cust*/
    AND cust.sman    GE v-s-sman
    AND cust.sman    LE v-e-sman
    NO-LOCK
      
    BREAK BY {&sort-by}:
    {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
    FIND FIRST sman
        WHERE sman.company EQ cocode
        AND sman.sman    EQ cust.sman
        NO-LOCK NO-ERROR.
    v-sman = cust.sman + "-" + (IF AVAILABLE sman THEN sman.sname
    ELSE "Slsmn not on file").
      
    IF FIRST-OF({&sort-by})        AND
        NOT first ({&sort-by})      AND
        "{&sort-by}" EQ "cust.sman" THEN PAGE.

    v-first-cust = YES.

    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company     EQ cust.company
        AND ar-inv.posted      EQ YES
        AND ar-inv.cust-no     EQ cust.cust-no
        AND ((ar-inv.inv-date  LE v-date - v-days[1] AND ll-date) OR
        (ar-inv.due-date  LE v-date - v-days[1] AND NOT ll-date))
        AND ar-inv.terms       NE "CASH"
        BY ar-inv.{&sort-by2} BY ar-inv.inv-no:

        /* task 09200521 include factored fg items*/
        /*    IF NOT v-include-factored THEN DO:
              FIND FIRST ar-invl NO-LOCK
                  WHERE ar-invl.x-no EQ ar-inv.x-no
                    AND CAN-FIND(FIRST reftable
                                 WHERE reftable.reftable EQ "FACTORED"
                                   AND reftable.company  EQ ar-inv.company
                                   AND reftable.loc      EQ ""
                                   AND reftable.code     EQ ar-invl.i-no
                                   AND reftable.code2    EQ "YES")
                  NO-ERROR.
              IF AVAIL ar-invl THEN NEXT.
            END. */

        /* Inserted because AR stores gross wrong */
        IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN
            amt = ar-inv.net.
        ELSE
            amt = ar-inv.gross.

        ASSIGN
            ag     = amt
            d      = v-date - ar-inv.{&date}
            ni     = ni + 1
            v-type = IF ar-inv.terms EQ "FCHG" THEN "FC" ELSE "IN".

        FOR EACH ar-cashl
            WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
            USE-INDEX inv-no NO-LOCK,

            EACH ar-cash
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
            AND ar-cash.check-date LE v-date
            USE-INDEX c-no NO-LOCK:

            IF ar-cashl.memo THEN
                /*
                          if ar-cashl.dscr begins "CREDIT MEMO CREATED FROM OE RETURN" then
                */
                IF ar-cashl.amt-disc NE 0 THEN
                    ag = ag - ar-cashl.amt-disc.
                ELSE
                    IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
                        ag = ag + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                    ELSE
                        ag = ag + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
            ELSE
                ag = ag + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
        END.

        IF ag GT 0 THEN 
        DO:
            IF v-first-cust THEN 
            DO:
                ASSIGN 
                    paid-amt = 0  
                    m3       = ""  
                    ni       = 0.
                IF cust.area-code NE "" THEN
                    m3 = STRING(cust.area-code,"(999) ").

                ASSIGN
                    m3       = m3 + string(cust.phone,"999-9999")
                    v-cr-lim = cust.cr-lim.

                IF v-cr-lim GT 9999999.99 THEN v-cr-lim = 9999999.99.

                FIND FIRST terms WHERE terms.company = cocode AND
                    terms.t-code = cust.terms NO-LOCK NO-ERROR.

                /*  if det-rpt2 then 
                    display cust.cust-no
                            space(3)
                            cust.name
                            space(3)
                            cust.area-code                           format "(xxx)"
                            cust.phone                               format "xxx-xxxx"
                            "  Fax:"
                            substr(cust.fax,1,3)                     format "(xxx)"
                            substr(cust.fax,4,7)                     format "xxx-xxxx"
                            skip
                            "CL:"
                            trim(string(v-cr-lim,">,>>>,>>>,>>9.99")) format "x(17)"
                            cust.contact                             format "x(20)"
                            space(2)
                            v-sman
                            space(2)
                            terms.dscr when avail terms              format "x(13)"
                        with no-labels no-box frame a1 STREAM-IO width 80. 
                       
                  if v-prt-add then run print-cust-add.   */                 

                v-first-cust = NO.
            END.

            /*  if det-rpt2 then
                display d at 4 format "-9999" when v-days-old 
                        space(7) "IN" space(5) ar-inv.inv-no
                        space(2) ar-inv.inv-date FORMAT "99/99/99"
                        amt to 54 ag to 77
                        with frame c no-labels no-box STREAM-IO width 80.  */

            ASSIGN
                cust-t[1] = cust-t[1] + ag
                v-dec     = 0
                v-dec[1]  = ag.
       
            /*   if v-export then
                 run export-data ("", d, "IN", string(ar-inv.inv-no,">>>>>>>>>>"),
                                  ar-inv.inv-date, amt, v-dec[1]). */
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
     
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField: 
                    WHEN "cust"      THEN 
                        cVarValue = (cust.cust-no)  .                               
                    WHEN "name"      THEN 
                        cVarValue = (cust.name)   .                                 
                    WHEN "cont"      THEN 
                        cVarValue = TRIM(cust.contact) .                                
                    WHEN "rep"       THEN 
                        cVarValue = TRIM(v-sman)        .                               
                    WHEN "trm"       THEN 
                        cVarValue = TRIM(IF AVAILABLE terms THEN terms.dscr ELSE "")  .     
                    WHEN "add1"      THEN 
                        cVarValue = TRIM(cust.addr[1])  .                               
                    WHEN "add2"      THEN 
                        cVarValue = TRIM(cust.addr[2]) .                                
                    WHEN "cty"       THEN 
                        cVarValue = TRIM(cust.city) .                                   
                    WHEN "stat"      THEN 
                        cVarValue = TRIM(cust.state) .                                  
                    WHEN "zip"       THEN 
                        cVarValue = TRIM(cust.zip)   .                                  
                    WHEN "crdt"      THEN 
                        cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99")   .       
                    WHEN "phon"      THEN 
                        cVarValue = STRING(TRIM(STRING(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                    WHEN "fax"       THEN 
                        cVarValue = STRING(TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                    WHEN "chk-mmo"   THEN 
                        cVarValue = ""      .                    
                    WHEN "dy-old"    THEN 
                        cVarValue = STRING(d,"->>>>")     .               
                    WHEN "typ"       THEN 
                        cVarValue = TRIM("IN") .                                  
                    WHEN "inv"       THEN 
                        cVarValue = STRING(ar-inv.inv-no,">>>>>>>>>>") .                                   
                    WHEN "inv-dt"    THEN 
                        cVarValue = IF ar-inv.inv-date NE ? THEN STRING(ar-inv.inv-date,"99/99/9999") ELSE "".              
                    WHEN "inv-amt"   THEN 
                        cVarValue = STRING(amt,"->>>>>>>>9.99")  .         
                    WHEN "curr"      THEN 
                        cVarValue = STRING(v-dec[1],"->>>>>>>>9.99")  .         
                                                             
                END CASE.                                       
             
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 

            END.
     
            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN 
            DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            FOR EACH ar-cashl
                WHERE ar-cashl.company  EQ ar-inv.company
                AND ar-cashl.posted   EQ YES
                AND ar-cashl.cust-no  EQ ar-inv.cust-no
                AND ar-cashl.inv-no   EQ ar-inv.inv-no
                USE-INDEX inv-no NO-LOCK,

                EACH ar-cash
                WHERE ar-cash.c-no       EQ ar-cashl.c-no
                AND ar-cash.check-date LE v-date
                USE-INDEX c-no NO-LOCK:

                IF ar-cashl.memo THEN

                    /* CTS CM/DM signs are reversed *****************************/
                    /*if (ar-cashl.amt-paid + ar-cashl.amt-disc) lt 0 then
                       assign v-type = "CM"
                              v-cr-db-amt = ar-cashl.amt-paid
                              v-disc-amt = ar-cashl.amt-disc.
         
                    else*/
                    IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                        ASSIGN v-type      = "DM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = ar-cashl.amt-disc.

                    ELSE
                        ASSIGN v-type      = "CM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = - (ar-cashl.amt-disc).

                ELSE
                    ASSIGN v-type      = "PY"
                        v-cr-db-amt = (ar-cashl.amt-paid) * -1
                        v-disc-amt  = ar-cashl.amt-disc * -1.

                IF v-disc-amt NE 0 THEN 
                DO:

                    v-disc-type = "DISC".
                    /*
                              if ar-cashl.dscr begins "CREDIT MEMO CREATED FROM OE RETURN" then
                    */
                    IF ar-cashl.memo THEN
                        ASSIGN
                            v-disc-type = "RETN"
                            v-disc-amt  = - v-disc-amt.

                    IF det-rpt2 THEN 
                    DO:
                        IF v-disc-type EQ "DISC" THEN 
                        DO:
                            /*  display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                                      v-type at 16
                                      ar-cashl.inv-no at 23
                                      ar-cash.check-date at 31 FORMAT "99/99/99"
                                      v-cr-db-amt to 54 skip
                                  with frame f-1 no-box no-labels STREAM-IO width 80.
                                  
                              if v-export then
                                run export-data (ar-cashl.check-no, 0, v-type,
                                                 string(ar-cashl.inv-no,">>>>>>>>>>"),
                                                 ar-cash.check-date, v-cr-db-amt, 0). */

                            ASSIGN 
                                cDisplay       = ""
                                cTmpField      = ""
                                cVarValue      = ""
                                cExcelDisplay  = ""
                                cExcelVarValue = "".
                
                            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                                CASE cTmpField: 
                                    WHEN "cust"      THEN 
                                        cVarValue = TRIM(cust.cust-no)  .                               
                                    WHEN "name"      THEN 
                                        cVarValue = TRIM(cust.name)   .                                 
                                    WHEN "cont"      THEN 
                                        cVarValue = TRIM(cust.contact) .                                
                                    WHEN "rep"       THEN 
                                        cVarValue = TRIM(v-sman)        .                               
                                    WHEN "trm"       THEN 
                                        cVarValue = TRIM(IF AVAILABLE terms THEN terms.dscr ELSE "")  .     
                                    WHEN "add1"      THEN 
                                        cVarValue = TRIM(cust.addr[1])  .                               
                                    WHEN "add2"      THEN 
                                        cVarValue = TRIM(cust.addr[2]) .                                
                                    WHEN "cty"       THEN 
                                        cVarValue = TRIM(cust.city) .                                   
                                    WHEN "stat"      THEN 
                                        cVarValue = TRIM(cust.state) .                                  
                                    WHEN "zip"       THEN 
                                        cVarValue = TRIM(cust.zip)   .                                  
                                    WHEN "crdt"      THEN 
                                        cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99")   .       
                                    WHEN "phon"      THEN 
                                        cVarValue = STRING(TRIM(STRING(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                                    WHEN "fax"       THEN 
                                        cVarValue = STRING(TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                                    WHEN "chk-mmo"   THEN 
                                        cVarValue = STRING(ar-cashl.check-no)      .                    
                                    WHEN "dy-old"    THEN 
                                        cVarValue = STRING(0,"->>>>")     .               
                                    WHEN "typ"       THEN 
                                        cVarValue = TRIM(v-type) .                                  
                                    WHEN "inv"       THEN 
                                        cVarValue = STRING(ar-cashl.inv-no,">>>>>>>>>>") .                                   
                                    WHEN "inv-dt"    THEN 
                                        cVarValue = IF ar-cash.check-date NE ? THEN STRING(ar-cash.check-date,"99/99/9999") ELSE "".              
                                    WHEN "inv-amt"   THEN 
                                        cVarValue = STRING(v-cr-db-amt,"->>>>>>>>9.99")  .         
                                    WHEN "curr"      THEN 
                                        cVarValue = STRING(0,"->>>>>>>>9.99")  .         
                                                                        
                                END CASE.                                       
                        
                                cExcelVarValue = cVarValue.
                                cDisplay = cDisplay + cVarValue +
                                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                            END.
                
                            PUT UNFORMATTED cDisplay SKIP.
                            IF v-export THEN 
                            DO:
                                PUT STREAM s-temp UNFORMATTED  
                                    cExcelDisplay SKIP.
                            END.
                        END.

                        /*  display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                                  v-disc-type at 16
                                  ar-cashl.inv-no at 23
                                  ar-cash.check-date at 31 FORMAT "99/99/99"
                                  v-disc-amt to 54
                              with frame f-50{&frame} no-box no-labels STREAM-IO width 80.
                              
                          if v-export then
                            run export-data (ar-cashl.check-no, 0, v-disc-type,
                                             string(ar-cashl.inv-no,">>>>>>>>>>"),
                                             ar-cash.check-date, v-disc-amt, 0). */

                        ASSIGN 
                            cDisplay       = ""
                            cTmpField      = ""
                            cVarValue      = ""
                            cExcelDisplay  = ""
                            cExcelVarValue = "".
            
                        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                            CASE cTmpField: 
                                WHEN "cust"      THEN 
                                    cVarValue = TRIM(cust.cust-no)  .                               
                                WHEN "name"      THEN 
                                    cVarValue = TRIM(cust.name)   .                                 
                                WHEN "cont"      THEN 
                                    cVarValue = TRIM(cust.contact) .                                
                                WHEN "rep"       THEN 
                                    cVarValue = TRIM(v-sman)        .                               
                                WHEN "trm"       THEN 
                                    cVarValue = TRIM(IF AVAILABLE terms THEN terms.dscr ELSE "")  .     
                                WHEN "add1"      THEN 
                                    cVarValue = TRIM(cust.addr[1])  .                               
                                WHEN "add2"      THEN 
                                    cVarValue = TRIM(cust.addr[2]) .                                
                                WHEN "cty"       THEN 
                                    cVarValue = TRIM(cust.city) .                                   
                                WHEN "stat"      THEN 
                                    cVarValue = TRIM(cust.state) .                                  
                                WHEN "zip"       THEN 
                                    cVarValue = TRIM(cust.zip)   .                                  
                                WHEN "crdt"      THEN 
                                    cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99")   .       
                                WHEN "phon"      THEN 
                                    cVarValue = STRING(TRIM(STRING(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                                WHEN "fax"       THEN 
                                    cVarValue = STRING(TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                                WHEN "chk-mmo"   THEN 
                                    cVarValue = STRING(ar-cashl.check-no)      .                    
                                WHEN "dy-old"    THEN 
                                    cVarValue = STRING(0,"->>>>")     .               
                                WHEN "typ"       THEN 
                                    cVarValue = TRIM(v-disc-type) .                                  
                                WHEN "inv"       THEN 
                                    cVarValue = STRING(ar-cashl.inv-no,">>>>>>>>>>") .                                   
                                WHEN "inv-dt"    THEN 
                                    cVarValue = IF ar-cash.check-date NE ? THEN STRING(ar-cash.check-date,"99/99/9999") ELSE "".              
                                WHEN "inv-amt"   THEN 
                                    cVarValue = STRING(v-disc-amt,"->>>>>>>>9.99")  .         
                                WHEN "curr"      THEN 
                                    cVarValue = STRING(0,"->>>>>>>>9.99")  .         
                                                                    
                            END CASE.                                       
                    
                            cExcelVarValue = cVarValue.
                            cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                        END.
            
                        PUT UNFORMATTED cDisplay SKIP.
                        IF v-export THEN 
                        DO:
                            PUT STREAM s-temp UNFORMATTED  
                                cExcelDisplay SKIP.
                        END.

                    END.
                END.

                ELSE
                    IF det-rpt2 THEN 
                    DO:
                        /* display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                                 v-type at 16
                                 ar-cashl.inv-no at 23
                                 ar-cash.check-date at 31 FORMAT "99/99/99"
                                 v-cr-db-amt to 54
                             with frame f-100 no-box no-labels STREAM-IO width 80.
                             
                         if v-export then
                           run export-data (ar-cashl.check-no, 0, v-type,
                                            string(ar-cashl.inv-no,">>>>>>>>>>"),
                                            ar-cash.check-date, v-cr-db-amt, 0). */
                        ASSIGN 
                            cDisplay       = ""
                            cTmpField      = ""
                            cVarValue      = ""
                            cExcelDisplay  = ""
                            cExcelVarValue = "".
            
                        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                            CASE cTmpField: 
                                WHEN "cust"      THEN 
                                    cVarValue = TRIM(cust.cust-no)  .                               
                                WHEN "name"      THEN 
                                    cVarValue = TRIM(cust.name)   .                                 
                                WHEN "cont"      THEN 
                                    cVarValue = TRIM(cust.contact) .                                
                                WHEN "rep"       THEN 
                                    cVarValue = TRIM(v-sman)        .                               
                                WHEN "trm"       THEN 
                                    cVarValue = TRIM(IF AVAILABLE terms THEN terms.dscr ELSE "")  .     
                                WHEN "add1"      THEN 
                                    cVarValue = TRIM(cust.addr[1])  .                               
                                WHEN "add2"      THEN 
                                    cVarValue = TRIM(cust.addr[2]) .                                
                                WHEN "cty"       THEN 
                                    cVarValue = TRIM(cust.city) .                                   
                                WHEN "stat"      THEN 
                                    cVarValue = TRIM(cust.state) .                                  
                                WHEN "zip"       THEN 
                                    cVarValue = TRIM(cust.zip)   .                                  
                                WHEN "crdt"      THEN 
                                    cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99")   .       
                                WHEN "phon"      THEN 
                                    cVarValue = STRING(TRIM(STRING(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                                WHEN "fax"       THEN 
                                    cVarValue = STRING(TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                                WHEN "chk-mmo"   THEN 
                                    cVarValue = STRING(ar-cashl.check-no)      .                    
                                WHEN "dy-old"    THEN 
                                    cVarValue = STRING(0,"->>>>")     .               
                                WHEN "typ"       THEN 
                                    cVarValue = TRIM(v-type) .                                  
                                WHEN "inv"       THEN 
                                    cVarValue = STRING(ar-cashl.inv-no,">>>>>>>>>>") .                                   
                                WHEN "inv-dt"    THEN 
                                    cVarValue = IF ar-cash.check-date NE ? THEN STRING(ar-cash.check-date,"99/99/9999") ELSE "".              
                                WHEN "inv-amt"   THEN 
                                    cVarValue = STRING(v-cr-db-amt,"->>>>>>>>9.99")  .         
                                WHEN "curr"      THEN 
                                    cVarValue = STRING(0,"->>>>>>>>9.99")  .         
                                                                    
                            END CASE.                                       
                    
                            cExcelVarValue = cVarValue.
                            cDisplay = cDisplay + cVarValue +
                                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                        END.
            
                        PUT UNFORMATTED cDisplay SKIP.
                        IF v-export THEN 
                        DO:
                            PUT STREAM s-temp UNFORMATTED  
                                cExcelDisplay SKIP.
                        END.
                    END.
            END. /* for each ar-cashl record */
        END.
    END. /* for each ar-inv record */

    ASSIGN 
        unapp[1] = 0.

    /* This loop finds all unapplied balances and totals by age */
    IF cust-t[1] NE 0 THEN
        FOR EACH ar-cash
            WHERE ar-cash.company     EQ cust.company
            AND ar-cash.cust-no     EQ cust.cust-no
            AND (ar-cash.check-date LE v-date OR
            ar-cash.check-date EQ ?)
            AND ar-cash.posted      EQ YES
            USE-INDEX ar-cash NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no       EQ ar-cash.c-no
            AND ar-cashl.posted     EQ YES
            USE-INDEX c-no NO-LOCK:

            IF ar-cashl.inv-no NE 0 THEN 
            DO:
                FIND FIRST ar-inv
                    WHERE ar-inv.company     EQ cust.company
                    AND ar-inv.inv-no      EQ ar-cashl.inv-no
                    AND ar-inv.inv-date    GT v-date
                    USE-INDEX inv-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ar-inv THEN NEXT.
            END.

            IF ar-cashl.memo THEN 
            DO:

                /* CTS CM/DM signs are reversed *****************************/
                IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                    ASSIGN v-type      = "DM"
                        v-cr-db-amt = ar-cashl.amt-paid
                        v-disc-amt  = ar-cashl.amt-disc.

                ELSE
                    ASSIGN v-type      = "CM"
                        v-cr-db-amt = ar-cashl.amt-paid
                        v-disc-amt  = ar-cashl.amt-disc.
            END.

            ELSE
                ASSIGN v-cr-db-amt = ar-cashl.amt-paid * -1
                    v-disc-amt  = ar-cashl.amt-disc * -1.

            unapp[1] = unapp[1] + v-cr-db-amt - v-disc-amt.
        END. /* for each ar-cashl record */

    first-unapp = YES.
    /* this loop displays all unapplied balances */
    
    IF unapp[1] NE 0 THEN
        FOR EACH ar-cash
            WHERE ar-cash.company     EQ cust.company
            AND ar-cash.cust-no     EQ cust.cust-no
            AND (ar-cash.check-date LE v-date OR
            ar-cash.check-date EQ ?)
            AND ar-cash.posted      EQ YES
            USE-INDEX ar-cash NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no       EQ ar-cash.c-no
            AND ar-cashl.posted     EQ YES
            USE-INDEX c-no NO-LOCK:

            IF ar-cashl.inv-no NE 0 THEN 
            DO:
                FIND FIRST ar-inv
                    WHERE ar-inv.company     EQ cust.company
                    AND ar-inv.inv-no      EQ ar-cashl.inv-no
                    AND ar-inv.inv-date    GT v-date
                    USE-INDEX inv-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ar-inv THEN NEXT.
            END.

            IF v-first-cust THEN 
            DO:
                ASSIGN 
                    paid-amt = 0  
                    cust-t   = 0  
                    m3       = ""  
                    ni       = 0.
                IF cust.area-code NE "" THEN
                    m3 = STRING(cust.area-code,"(999) ").

                m3 = m3 + string(cust.phone,"999-9999").

                FIND FIRST terms WHERE terms.company = cocode AND
                    terms.t-code = cust.terms NO-LOCK NO-ERROR.

                /*  if det-rpt2 then
                     display cust.cust-no
                             cust.name
                             cust.area-code                           format "(xxx)"
                             cust.phone                               format "xxx-xxxx"
                             "Fax:"
                             substr(cust.fax,1,3)                     format "(xxx)"
                             substr(cust.fax,4,7)                     format "xxx-xxxx"
                             skip
                             "CL:"
                             trim(string(v-cr-lim,">,>>>,>>>,>>9.99")) format "x(17)"
                             cust.contact                             format "x(20)"
                             space(2)
                             v-sman
                             space(2)
                             terms.dscr when avail terms              format "x(13)"
                         with no-labels no-box frame a2 STREAM-IO width 80.
                        
                  if v-prt-add then run print-cust-add. */
        
                ASSIGN 
                    v-first-cust = NO.
            END.

            IF ar-cashl.memo EQ TRUE THEN 
            DO:
                IF (ar-cashl.amt-paid + ar-cashl.amt-disc) LT 0 THEN
                    ASSIGN v-type      = "CM"
                        v-cr-db-amt = ar-cashl.amt-paid
                        v-disc-amt  = ar-cashl.amt-disc.
                ELSE
                    IF (ar-cashl.amt-paid + ar-cashl.amt-disc) GT 0 THEN
                        ASSIGN v-type      = "DM"
                            v-cr-db-amt = ar-cashl.amt-paid
                            v-disc-amt  = ar-cashl.amt-disc.
            END.

            ELSE
                ASSIGN v-type      = "PY"
                    v-cr-db-amt = ar-cashl.amt-paid * -1
                    v-disc-amt  = ar-cashl.amt-disc * -1.

            IF first-unapp THEN 
            DO:

                /*  if det-rpt2 then
                    display skip(1)
                            ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                            v-type at 16
                            "ON ACCT" at 23
                            ar-cash.check-date at 31 FORMAT "99/99/99"
                            (v-cr-db-amt + v-disc-amt)
                                  format "->>>,>>>,>>9.99" to 54
                            unapp[1] when unapp[1] ne 0 to 77
                        with frame ab no-labels no-box STREAM-IO width 80.
                        
                  if v-export then
                    run export-data (ar-cashl.check-no, 0, v-type, "ON ACCT",
                                     ar-cash.check-date, v-cr-db-amt + v-disc-amt,
                                     unapp[1]).*/
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".
            
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                        WHEN "cust"      THEN 
                            cVarValue = TRIM(cust.cust-no)  .                               
                        WHEN "name"      THEN 
                            cVarValue = TRIM(cust.name)   .                                 
                        WHEN "cont"      THEN 
                            cVarValue = TRIM(cust.contact) .                                
                        WHEN "rep"       THEN 
                            cVarValue = TRIM(v-sman)        .                               
                        WHEN "trm"       THEN 
                            cVarValue = TRIM(IF AVAILABLE terms THEN terms.dscr ELSE "")  .     
                        WHEN "add1"      THEN 
                            cVarValue = TRIM(cust.addr[1])  .                               
                        WHEN "add2"      THEN 
                            cVarValue = TRIM(cust.addr[2]) .                                
                        WHEN "cty"       THEN 
                            cVarValue = TRIM(cust.city) .                                   
                        WHEN "stat"      THEN 
                            cVarValue = TRIM(cust.state) .                                  
                        WHEN "zip"       THEN 
                            cVarValue = TRIM(cust.zip)   .                                  
                        WHEN "crdt"      THEN 
                            cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99")   .       
                        WHEN "phon"      THEN 
                            cVarValue = STRING(TRIM(STRING(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                        WHEN "fax"       THEN 
                            cVarValue = STRING(TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                        WHEN "chk-mmo"   THEN 
                            cVarValue = STRING(ar-cashl.check-no)      .                    
                        WHEN "dy-old"    THEN 
                            cVarValue = STRING(0,"->>>>")     .               
                        WHEN "typ"       THEN 
                            cVarValue = TRIM(v-type) .                                  
                        WHEN "inv"       THEN 
                            cVarValue = STRING("ON ACCT") .                                   
                        WHEN "inv-dt"    THEN 
                            cVarValue = IF ar-cash.check-date NE ? THEN STRING(ar-cash.check-date,"99/99/9999") ELSE "".              
                        WHEN "inv-amt"   THEN 
                            cVarValue = STRING((v-cr-db-amt + v-disc-amt),"->>>>>>>>9.99")  .         
                        WHEN "curr"      THEN 
                            cVarValue = STRING(unapp[1],"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
            
                PUT UNFORMATTED cDisplay SKIP.
                IF v-export THEN 
                DO:
                    PUT STREAM s-temp UNFORMATTED  
                        cExcelDisplay SKIP.
                END.

                cust-t[1] = cust-t[1] + unapp[1].
            END.

            IF first-unapp THEN first-unapp = NO.

            ELSE 
            DO:
                /* if det-rpt2 then
                   display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                           v-type at 16
                           "ON ACCT" at 23
                           ar-cash.check-date at 31 FORMAT "99/99/99"
                           (v-cr-db-amt + v-disc-amt)
                                    format "->>>,>>>,>>9.99" to 54
                       with frame f-2 no-box no-labels STREAM-IO width 80.
                       
                 if v-export then
                   run export-data (ar-cashl.check-no, 0, v-type, "ON ACCT",
                                    ar-cash.check-date, v-cr-db-amt + v-disc-amt, 0). */
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".
            
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                        WHEN "cust"      THEN 
                            cVarValue = TRIM(cust.cust-no)  .                               
                        WHEN "name"      THEN 
                            cVarValue = TRIM(cust.name)   .                                 
                        WHEN "cont"      THEN 
                            cVarValue = TRIM(cust.contact) .                                
                        WHEN "rep"       THEN 
                            cVarValue = TRIM(v-sman)        .                               
                        WHEN "trm"       THEN 
                            cVarValue = TRIM(IF AVAILABLE terms THEN terms.dscr ELSE "")  .     
                        WHEN "add1"      THEN 
                            cVarValue = TRIM(cust.addr[1])  .                               
                        WHEN "add2"      THEN 
                            cVarValue = TRIM(cust.addr[2]) .                                
                        WHEN "cty"       THEN 
                            cVarValue = TRIM(cust.city) .                                   
                        WHEN "stat"      THEN 
                            cVarValue = TRIM(cust.state) .                                  
                        WHEN "zip"       THEN 
                            cVarValue = TRIM(cust.zip)   .                                  
                        WHEN "crdt"      THEN 
                            cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99")   .       
                        WHEN "phon"      THEN 
                            cVarValue = STRING(TRIM(STRING(cust.area-code,"(xxx)") + " " + string(cust.phone,"xxx-xxxx"))) .
                        WHEN "fax"       THEN 
                            cVarValue = STRING(TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " + string(substr(cust.fax,4,7),"xxx-xxxx"))) .                     
                        WHEN "chk-mmo"   THEN 
                            cVarValue = STRING(ar-cashl.check-no)      .                    
                        WHEN "dy-old"    THEN 
                            cVarValue = STRING(0,"->>>>")     .               
                        WHEN "typ"       THEN 
                            cVarValue = TRIM(v-type) .                                  
                        WHEN "inv"       THEN 
                            cVarValue = STRING("ON ACCT") .                                   
                        WHEN "inv-dt"    THEN 
                            cVarValue = IF ar-cash.check-date NE ? THEN STRING(ar-cash.check-date,"99/99/9999") ELSE "".              
                        WHEN "inv-amt"   THEN 
                            cVarValue = STRING((v-cr-db-amt + v-disc-amt),"->>>>>>>>9.99")  .         
                        WHEN "curr"      THEN 
                            cVarValue = STRING(0,"->>>>>>>>9.99")  .         
                                                                    
                    END CASE.                                       
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
            
                PUT UNFORMATTED cDisplay SKIP.
                IF v-export THEN 
                DO:
                    PUT STREAM s-temp UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
            END.
        END. /* for each ar-cashl record */

    c1 = cust-t[1].

    IF (NOT v-first-cust) OR c1 NE 0 THEN 
    DO:
        /* if det-rpt2 then do:
           display skip(1) "***** CUSTOMER TOTALS" at 4 c1 to 54 cust-t[1] to 77
                   skip(1)
               with frame a3 no-labels no-box no-attr-space STREAM-IO width 80.
           
           if not last-of({&sort-by}) or "{&sort-by}" ne "cust.sman" then
             put skip(1).
         end.
         
         else
           display cust.cust-no space(2) cust.name + "  " + m3 format "x(50)" skip
                   c1        to 54
                   cust-t[1] to 77
                   skip(1)
               with frame a3sum no-labels no-box no-attr-space STREAM-IO width 80. */
        PUT SKIP str-line SKIP.
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
            
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField: 
                WHEN "cust"      THEN 
                    cVarValue = "" .
                WHEN "name"      THEN 
                    cVarValue = "" .
                WHEN "cont"      THEN 
                    cVarValue = "" . 
                WHEN "rep"       THEN 
                    cVarValue = "" . 
                WHEN "trm"       THEN 
                    cVarValue = "" . 
                WHEN "add1"      THEN 
                    cVarValue = "" . 
                WHEN "add2"      THEN 
                    cVarValue = "" . 
                WHEN "cty"       THEN 
                    cVarValue = "" . 
                WHEN "stat"      THEN 
                    cVarValue = "" . 
                WHEN "zip"       THEN 
                    cVarValue = "" . 
                WHEN "crdt"      THEN 
                    cVarValue = "" . 
                WHEN "phon"      THEN 
                    cVarValue = "" . 
                WHEN "fax"       THEN 
                    cVarValue = "" . 
                WHEN "chk-mmo"   THEN 
                    cVarValue = "" . 
                WHEN "dy-old"    THEN 
                    cVarValue = "" . 
                WHEN "typ"       THEN 
                    cVarValue = "" . 
                WHEN "inv"       THEN 
                    cVarValue = "" . 
                WHEN "inv-dt"    THEN 
                    cVarValue = "" . 
                WHEN "inv-amt"   THEN 
                    cVarValue = STRING(c1,"->>>>>>>>9.99")  .         
                WHEN "curr"      THEN 
                    cVarValue = STRING(cust-t[1],"->>>>>>>>9.99")  .         
                                                                    
            END CASE.                                       
                    
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
            
        PUT UNFORMATTED  
            "       CUSTOMER TOTALS:" SUBSTRING(cDisplay,24,300) SKIP.
        IF v-export THEN 
        DO:
            PUT STREAM s-temp UNFORMATTED  
                "CUSTOMER TOTALS: " + substring(cExcelDisplay,3,300) SKIP.
        END.

            
        ASSIGN
            sman-t[1] = sman-t[1] + cust-t[1]
            cust-t[1] = 0.
    END.
    
    IF LAST-OF({&sort-by}) THEN 
    DO:
        c1 = sman-t[1].
          
        IF "{&sort-by}" EQ "cust.sman" THEN 
        DO:
            /* display v-sman                  at 4    format "x(33)"
                     "TOTALS: " + v-sman                  @ v-sman
                     "***** SALESMAN TOTALS" when det-rpt2 @ v-sman
                     c1                      to 54
                     sman-t[1]               to 77
                     skip(2)
                 with frame slsmn no-labels no-box no-attr-space STREAM-IO width 80. */
            PUT SKIP str-line SKIP.
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField: 
                    WHEN "cust"      THEN 
                        cVarValue = "" .
                    WHEN "name"      THEN 
                        cVarValue = "" .
                    WHEN "cont"      THEN 
                        cVarValue = "" . 
                    WHEN "rep"       THEN 
                        cVarValue = "" . 
                    WHEN "trm"       THEN 
                        cVarValue = "" . 
                    WHEN "add1"      THEN 
                        cVarValue = "" . 
                    WHEN "add2"      THEN 
                        cVarValue = "" . 
                    WHEN "cty"       THEN 
                        cVarValue = "" . 
                    WHEN "stat"      THEN 
                        cVarValue = "" . 
                    WHEN "zip"       THEN 
                        cVarValue = "" . 
                    WHEN "crdt"      THEN 
                        cVarValue = "" . 
                    WHEN "phon"      THEN 
                        cVarValue = "" . 
                    WHEN "fax"       THEN 
                        cVarValue = "" . 
                    WHEN "chk-mmo"   THEN 
                        cVarValue = "" . 
                    WHEN "dy-old"    THEN 
                        cVarValue = "" . 
                    WHEN "typ"       THEN 
                        cVarValue = "" . 
                    WHEN "inv"       THEN 
                        cVarValue = "" . 
                    WHEN "inv-dt"    THEN 
                        cVarValue = "" . 
                    WHEN "inv-amt"   THEN 
                        cVarValue = STRING(c1,"->>>>>>>>9.99")  .         
                    WHEN "curr"      THEN 
                        cVarValue = STRING(sman-t[1],"->>>>>>>>9.99")  .         
                                                                    
                END CASE.                                       
                    
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            
            PUT UNFORMATTED  
                "       SALESMAN TOTALS:" SUBSTRING(cDisplay,24,300) SKIP.
            IF v-export THEN 
            DO:
                PUT STREAM s-temp UNFORMATTED  
                    "SALESMAN TOTALS: " + substring(cExcelDisplay,3,300) SKIP.
            END.
        END.

        ASSIGN
            grand-t[1] = grand-t[1] + sman-t[1]
            sman-t[1]  = 0.
    END.
    
    m3 = "".
    IF ni EQ 1 THEN m3 = m2.
    ASSIGN
        v-cr-db-amt = 0
        v-disc-amt  = 0.
END.  /* for each cust record */
  
RETURN.
 /* 
  procedure print-cust-add:
    display cust.addr[1]                                                skip
            cust.addr[2]                                                skip
            trim(cust.city) + ", " +
            trim(cust.state) + "  " + trim(cust.zip) format "x(50)"
            
        with no-labels no-box frame cust-detail STREAM-IO width 80.
  end.
  
  procedure export-data:
    def input parameter v-field-01 like ar-cashl.check-no no-undo.
    def input parameter v-field-02 like d                 no-undo.
    def input parameter v-field-03 like v-type            no-undo.
    def input parameter v-field-04 as   char              no-undo.
    def input parameter v-field-05 like ar-inv.inv-date   no-undo.
    def input parameter v-field-06 like amt               no-undo.
    def input parameter v-field-07 like ag                no-undo.
    
    
    /*put stream s-temp unformatted
        trim(cust.cust-no)                                      + "," +
        trim(cust.name)                                         + "," +
        trim(cust.contact)                                      + "," +
        trim(v-sman)                                            + "," +
        trim(if avail terms then terms.dscr else "")            + "," +
        trim(cust.addr[1])                                      + "," +
        trim(cust.addr[2])                                      + "," +
        trim(cust.city)                                         + "," +
        trim(cust.state)                                        + "," +
        trim(cust.zip)                                          + "," +
        trim(string(cust.cr-lim,">>>>>>>>9.99"))                + "," +
        trim(string(cust.area-code,"(xxx)") + " " +
             string(cust.phone,"xxx-xxxx"))                     + "," +
        trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
             string(substr(cust.fax,4,7),"xxx-xxxx"))           + "," +
        trim(v-field-01)                                        + "," +
        trim(string(v-field-02,"->>>>"))                        + "," +
        trim(v-field-03)                                        + "," +
        trim(v-field-04)                                        + "," +
        trim(string(v-field-05,"99/99/9999"))                   + "," +
        trim(string(v-field-06,"->>>>>>>>9.99"))                + "," +
        trim(string(v-field-07,"->>>>>>>>9.99"))
        skip.*/
    EXPORT STREAM s-temp DELIMITER ","
            trim(cust.cust-no)                               
            trim(cust.name)                                  
            trim(cust.contact)                               
            trim(v-sman)                                     
            trim(if avail terms then terms.dscr else "")     
            trim(cust.addr[1])                               
            trim(cust.addr[2])                               
            trim(cust.city)                                  
            trim(cust.state)                                 
            trim(cust.zip)                                   
            trim(string(cust.cr-lim,">>>>>>>>9.99"))         
            trim(string(cust.area-code,"(xxx)") + " " +      
                 string(cust.phone,"xxx-xxxx"))              
            trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
                 string(substr(cust.fax,4,7),"xxx-xxxx"))    
            trim(v-field-01)                                 
            trim(string(v-field-02,"->>>>"))                 
            trim(v-field-03)                                 
            trim(v-field-04)                                 
            trim(string(v-field-05,"99/99/9999"))            
            trim(string(v-field-06,"->>>>>>>>9.99"))         
            trim(string(v-field-07,"->>>>>>>>9.99"))  
       SKIP.

  end.  */
  
/* End ---------------------------------- Copr. 2002  Advanced Software, Inc. */
