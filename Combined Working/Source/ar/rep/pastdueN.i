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

END.

{sys/form/r-top5DL2.f} 
/*VIEW FRAME r-top.*/
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

                v-first-cust = NO.
            END.

            ASSIGN
                cust-t[1] = cust-t[1] + ag
                v-dec     = 0
                v-dec[1]  = ag.
            
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
     
            PUT UNFORMATTED cDisplay FORMAT "x(340)" SKIP.
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
                
                            PUT UNFORMATTED cDisplay FORMAT "x(340)" SKIP.
                            IF v-export THEN 
                            DO:
                                PUT STREAM s-temp UNFORMATTED  
                                    cExcelDisplay SKIP.
                            END.
                        END.

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
            
                        PUT UNFORMATTED cDisplay FORMAT "x(340)" SKIP.
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
            
                        PUT UNFORMATTED cDisplay FORMAT "x(340)" SKIP.
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
            
                PUT UNFORMATTED cDisplay FORMAT "x(340)" SKIP.
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
            
                PUT UNFORMATTED cDisplay FORMAT "x(340)" SKIP.
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
            "       CUSTOMER TOTALS:" SUBSTRING(cDisplay,24,300) SKIP(1).
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
                "       SALESMAN TOTALS:" SUBSTRING(cDisplay,24,300) SKIP(1).
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
 
  
/* End ---------------------------------- Copr. 2002  Advanced Software, Inc. */
