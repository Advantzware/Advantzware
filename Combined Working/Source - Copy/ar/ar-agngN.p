/* ------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------ */
/* -------------------------------------------------- ar/ar-aging.i 01/97 JLF  */
/* A/R Aged Receivables Report Program - A/R Module                            */
/* -------------------------------------------------------------------------- */
def input parameter ipcSort1 as char no-undo.  
def input parameter ipcSort2 as char no-undo. 
def input parameter ipcDateToUse as char no-undo.

{sys/inc/var.i shared}
{sys/ref/CustList.i}
{ar/ar-agng2.i}
{sys/inc/ttRptSel.i}
{sys/form/r-top3w.f} 

    DEF SHARED VAR ldummy               AS LOG     NO-UNDO.
    DEF SHARED VAR cTextListToSelect    AS cha     NO-UNDO.
    DEF SHARED VAR cFieldListToSelect   AS cha     NO-UNDO.
    DEF SHARED VAR cFieldLength         AS cha     NO-UNDO.
    DEF SHARED VAR cFieldType           AS cha     NO-UNDO.
    DEF SHARED VAR iColumnLength        AS INT     NO-UNDO.
    DEF SHARED VAR cTextListToDefault   AS cha     NO-UNDO.
    DEF SHARED VAR cColumnInit          AS LOG     INIT YES NO-UNDO.
    DEF SHARED VAR cSelectedList        AS cha     NO-UNDO.
    DEF SHARED VAR str-line             AS cha     FORM "x(300)" NO-UNDO.

    DEF        VAR cDisplay             AS cha     NO-UNDO.
    DEF        VAR cExcelDisplay        AS cha     NO-UNDO.
    DEF        VAR hField               AS HANDLE  NO-UNDO.
    DEF        VAR cTmpField            AS CHA     NO-UNDO.
    DEF        VAR cVarValue            AS cha     NO-UNDO.
    DEF        VAR cExcelVarValue       AS cha     NO-UNDO.
    DEF        VAR cFieldName           AS cha     NO-UNDO.

    DEF        VAR v-cr-db-amt          AS DEC     FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR v-disc-amt           AS DEC     FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR v-type               AS CHAR    FORMAT "x(2)" NO-UNDO.
    DEF        VAR v-first-cust         AS LOGICAL NO-UNDO.
    DEF        VAR d                    AS INT     LABEL "Days" NO-UNDO.
    DEF        VAR ni                   AS INT     NO-UNDO.
    DEF        VAR cust-t               AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR cust-t-pri           AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR cust-t-fc            AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR sman-t               AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR sman-t-pri           AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR sman-t-fc            AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR v-current-trend-days AS INT     NO-UNDO FORMAT "->>9".
    DEF        VAR curr-t               AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR curr-t-pri           AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR curr-t-fc            AS DEC     EXTENT 6 FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR onacc                AS DEC     NO-UNDO.
    DEF        VAR s                    AS INT     NO-UNDO.
    DEF        VAR cPO                  AS CHAR    FORMAT "x(20)" NO-UNDO.
    DEF        VAR ag                   AS DEC     FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR amt                  LIKE ag NO-UNDO.
    DEF        VAR paid-amt             LIKE ag NO-UNDO.
    DEF        VAR c1                   AS DEC     FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR c1-pri               AS DEC     FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR c1-fc                AS DEC     FORMAT "->,>>>,>>>,>>9.99" NO-UNDO. 
    DEF        VAR m1                   AS CHAR    FORMAT "x(20)" NO-UNDO.
    DEF        VAR m2                   AS CHAR    FORMAT "x(20)" NO-UNDO.
    DEF        VAR m3                   AS CHAR    FORMAT "x(20)" NO-UNDO.
    DEF        VAR t1                   AS DEC     FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR t1-pri               AS DEC     FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR t1-fc                AS DEC     FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEF        VAR save_id              AS RECID   NO-UNDO.
    DEF        VAR unapp                LIKE cust-t NO-UNDO.
    DEF        VAR first-unapp          AS LOG     INIT YES NO-UNDO.
    DEF        VAR tmp-var              AS CHAR    FORMAT "x(20)" NO-UNDO.
    DEF        VAR v-disc-type          AS CHAR    FORMAT "x(4)" NO-UNDO.
    DEF        VAR v-sman               AS CHAR    FORMAT "x(24)" NO-UNDO.
    DEF        VAR v-int                AS INT     NO-UNDO.
    DEF        VAR v-dec                AS DEC     EXTENT 4 NO-UNDO.
    DEF        VAR ll-valid-cust        AS LOG     NO-UNDO.
    DEF        VAR ll-mult-curr         AS LOG     NO-UNDO.
    DEF        VAR lv-page-break        AS CHAR    NO-UNDO.
    DEF        VAR lv-f-bot-hdr         AS CHAR    FORMAT "x(12)" NO-UNDO.
    DEF        VAR v-neg-text           AS CHAR    NO-UNDO.
    DEF        VAR v-tr-dscr            AS CHAR    NO-UNDO.
    DEF        VAR v-check-date         AS DATE    NO-UNDO.
    DEF        VAR v-gltrans-desc       AS CHAR    FORMAT "X(60)" NO-UNDO.
    DEF        VAR cPoNo                LIKE ar-inv.po-no NO-UNDO.
    DEF        VAR cJobStr              AS CHAR    FORMAT "x(9)" NO-UNDO.
    DEF TEMP-TABLE tt-cust NO-UNDO 
        FIELD curr-code LIKE cust.curr-code
        FIELD sorter    LIKE cust.cust-no
        FIELD row-id    AS ROWID
        INDEX tt-cust curr-code sorter.

    DEF TEMP-TABLE tt-inv NO-UNDO  
        FIELD sorter   LIKE ar-inv.inv-no
        FIELD inv-no   LIKE ar-inv.inv-no
        FIELD row-id   AS ROWI
        FIELD net      LIKE ar-inv.net
        FIELD gross    LIKE ar-inv.gross
        FIELD freight  LIKE ar-inv.freight
        FIELD tax-amt  LIKE ar-inv.tax-amt
        FIELD x-no     LIKE ar-inv.x-no
        FIELD inv-date LIKE ar-inv.inv-date
        FIELD terms    LIKE ar-inv.terms
        FIELD company  LIKE ar-inv.company
        FIELD cust-no  LIKE ar-inv.cust-no                                      
        FIELD due-date LIKE ar-inv.due-date  
        FIELD rec_key  LIKE ar-inv.rec_key  
        INDEX tt-inv sorter inv-no.


        DEF TEMP-TABLE tt-ar-cash
            FIELD c-no       LIKE ar-cash.c-no
            FIELD check-date LIKE ar-cash.check-date
            FIELD check-no   LIKE ar-cash.check-no 
            FIELD company    LIKE ar-cash.company             
            FIELD cust-no    LIKE ar-cash.cust-no 
            FIELD posted     LIKE ar-cash.posted              
            INDEX c-no       c-no 
            INDEX tt-ar-cash company cust-no check-date  
            .  
        DEF TEMP-TABLE tt-ar-cashl 
            FIELD company  LIKE ar-cashl.company
            FIELD amt-paid LIKE ar-cashl.amt-paid
            FIELD rec_key  LIKE ar-cashl.rec_key
            FIELD inv-no   LIKE ar-cashl.inv-no
            FIELD memo     LIKE ar-cashl.memo
            FIELD amt-disc LIKE ar-cashl.amt-disc            
            FIELD c-no     LIKE ar-cashl.c-no
            FIELD posted   LIKE ar-cashl.posted
            FIELD cust-no  LIKE ar-cashl.cust-no 
            FIELD check-no LIKE ar-cashl.check-no
            INDEX inv-no company posted cust-no inv-no 
            INDEX c-no   c-no
            . 
    DEF TEMP-TABLE tt-reftable
        FIELD reftable LIKE reftable.reftable
        FIELD rec_key  LIKE reftable.rec_key
        FIELD CODE     LIKE reftable.CODE
        INDEX rec_key reftable rec_key. 
                            
    FORM HEADER SKIP(1)
        lv-page-break FORMAT "x(200)"
        WITH PAGE-TOP FRAME r-top-1 STREAM-IO WIDTH 200 NO-BOX.


    DEF NEW SHARED TEMP-TABLE tt-formtext
        FIELD tt-line-no AS INT
        FIELD tt-length  AS INT
        FIELD tt-text    AS CHAR
        INDEX tt-form-text tt-line-no.
                          
 
    DEF VAR lv-text           AS cha NO-UNDO.
    DEF VAR v-Inv-note        AS cha FORM "x(80)" EXTENT 8 NO-UNDO.
    DEF VAR v-Collection-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.

    FORMAT HEADER
        SKIP(1)
        "Customer/Contact/SalesRep/Terms/Recent Payment Trend" SKIP
        v-chk-day " Type   Inv.#   Inv Date" SPACE(10)
        "Amount                Current           "
        v-days[1] SPACE(13) v-days[2] SPACE(13) v-days[3] SPACE(0) "+" SKIP  
        FILL("_",132) FORMAT "x(131)"
        WITH PAGE-TOP FRAME r-top-2 STREAM-IO WIDTH 200 NO-BOX.

 DEF TEMP-TABLE tt-factored
        FIELD company LIKE itemfg.company
        FIELD i-no    LIKE itemfg.i-no
        FIELD x-no    LIKE ar-invl.x-no
        INDEX i1 i-no
        INDEX i2 x-no.
  
    FOR EACH itemfg FIELDS (company i-no)
        WHERE itemfg.factored    EQ YES
        NO-LOCK:        
        FIND FIRST tt-factored WHERE tt-factored.i-no EQ itemfg.i-no
            NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-factored THEN 
        DO:
            FOR EACH ar-invl WHERE ar-invl.company EQ cocode
                AND ar-invl.i-no EQ itemfg.i-no
                NO-LOCK:
                CREATE tt-factored.
                ASSIGN 
                    tt-factored.company = itemfg.company
                    tt-factored.i-no    = itemfg.i-no.
            END.
        END.
    END.

    /* Start temp-table creation section */
DEF VAR cFromCust AS CHAR NO-UNDO.
DEF VAR cToCust AS CHAR NO-UNDO.
    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
      BY ttCustList.cust-no :
      IF cFromCust EQ "" THEN
        cFromCust =  ttCustList.cust-no.
      cToCust =  ttCustList.cust-no.
    END.

    IF v-inc OR v-date NE TODAY THEN
        FOR EACH ar-inv
   FIELDS(company posted cust-no inv-date                   
            terms x-no due-date net gross                     
            freight tax-amt inv-no)                    
            NO-LOCK                                           
            WHERE ar-inv.company     EQ cocode
            AND ar-inv.posted      EQ YES                     
            AND ar-inv.cust-no     GE cFromCust
            AND ar-inv.cust-no     LE cToCust
            AND ar-inv.inv-date    LE v-date                  
            AND ar-inv.terms       NE "CASH":
            IF NOT v-include-factored
                AND                                         
                CAN-FIND(FIRST tt-factored                                                    
                WHERE tt-factored.x-no EQ ar-inv.x-no) THEN              
                NEXT.

            CREATE tt-inv.
            ASSIGN
                tt-inv.sorter   = if ipcSort2 eq "ar-inv.inv-no" then int(ar-inv.inv-no) else
                                      INT(if ipcSort2 eq "ar-inv.due-date" then  ar-inv.due-date
                                      else if ipcSort2 eq "ar-inv.inv-date" then  ar-inv.inv-date
                                      else today)
                tt-inv.inv-no   = ar-inv.inv-no
                tt-inv.row-id   = ROWID(ar-inv)
                tt-inv.net      = ar-inv.net
                tt-inv.gross    = ar-inv.gros

                tt-inv.freight  = ar-inv.freight
                tt-inv.tax-amt  = ar-inv.tax-amt
                tt-inv.x-no     = ar-inv.x-no
                tt-inv.inv-date = ar-inv.inv-date
                tt-inv.terms    = ar-inv.terms
                tt-inv.company  = ar-inv.company
                tt-inv.cust-no  = ar-inv.cust-no      
                tt-inv.due-date = ar-inv.due-date  
                tt-inv.rec_key  = ar-inv.rec_key  
                .
        END.

    ELSE 
    DO:
        FOR EACH ar-inv                                       
  FIELDS(company posted cust-no inv-date                   
            terms x-no due-date net gross                     
            freight tax-amt inv-no)                    
            WHERE ar-inv.company     EQ cocode
            AND ar-inv.posted      EQ YES                              
            AND ar-inv.cust-no     GE cFromCust
            AND ar-inv.cust-no     LE cToCust
            AND ar-inv.inv-date    LE v-date                  
            AND ar-inv.terms       NE "CASH"
            AND ar-inv.due LT 0
            USE-INDEX posted-due
            :


            IF NOT v-include-factored AND
                CAN-FIND(FIRST tt-factored 
                WHERE tt-factored.x-no EQ ar-inv.x-no) THEN 
                NEXT.

            CREATE tt-inv.
            ASSIGN
                tt-inv.sorter   = if ipcSort2 eq "ar-inv.inv-no" then int(ar-inv.inv-no) else
                                      INT(if ipcSort2 eq "ar-inv.due-date" then  ar-inv.due-date
                                      else if ipcSort2 eq "ar-inv.inv-date" then  ar-inv.inv-date
                                      else today)
                tt-inv.inv-no   = ar-inv.inv-no
                tt-inv.row-id   = ROWID(ar-inv)  
                tt-inv.net      = ar-inv.net
                tt-inv.gross    = ar-inv.gross        
                tt-inv.freight  = ar-inv.freight
                tt-inv.tax-amt  = ar-inv.tax-amt
                tt-inv.x-no     = ar-inv.x-no
                tt-inv.inv-date = ar-inv.inv-date
                tt-inv.terms    = ar-inv.terms
                tt-inv.company  = ar-inv.company
                tt-inv.cust-no  = ar-inv.cust-no        
                tt-inv.due-date = ar-inv.due-date   
                tt-inv.rec_key  = ar-inv.rec_key    
                .
        END.

        FOR EACH ar-inv                                       
            FIELDS(company posted cust-no inv-date                   
            terms x-no due-date net gross                     
            freight tax-amt inv-no)                    
            NO-LOCK                                          
            WHERE ar-inv.company     EQ cocode           
            AND ar-inv.posted      EQ YES                                 
            AND ar-inv.cust-no     GE cFromCust
            AND ar-inv.cust-no     LE cToCust
            AND ar-inv.inv-date    LE v-date                  
            AND ar-inv.terms       NE "CASH"
            AND ar-inv.due GT 0
            USE-INDEX posted-due:
            IF NOT v-include-factored AND                                         
                CAN-FIND(FIRST tt-factored                                                    
                WHERE tt-factored.x-no EQ ar-inv.x-no) THEN              
                NEXT.

            CREATE tt-inv.
            ASSIGN
                tt-inv.sorter   = if ipcSort2 eq "ar-inv.inv-no" then int(ar-inv.inv-no) else
                                      INT(if ipcSort2 eq "ar-inv.due-date" then  ar-inv.due-date
                                      else if ipcSort2 eq "ar-inv.inv-date" then  ar-inv.inv-date
                                      else today)
                tt-inv.inv-no   = ar-inv.inv-no
                tt-inv.row-id   = ROWID(ar-inv) 
                tt-inv.net      = ar-inv.net
                tt-inv.gross    = ar-inv.gross        
                tt-inv.freight  = ar-inv.freight
                tt-inv.tax-amt  = ar-inv.tax-amt
                tt-inv.x-no     = ar-inv.x-no
                tt-inv.inv-date = ar-inv.inv-date
                tt-inv.terms    = ar-inv.terms
                tt-inv.company  = ar-inv.company
                tt-inv.cust-no  = ar-inv.cust-no                 
                tt-inv.due-date = ar-inv.due-date  
                tt-inv.rec_key  = ar-inv.rec_key    
                .  
        END.
    END.


    FOR EACH ar-cash 
        WHERE ar-cash.company EQ cocode        
          AND ar-cash.cust-no     GE cFromCust
            AND ar-cash.cust-no     LE cToCust
        NO-LOCK:
        CREATE tt-ar-cash.
        ASSIGN
            tt-ar-cash.c-no       = ar-cash.c-no
            tt-ar-cash.check-date = ar-cash.check-date
            tt-ar-cash.check-no   = ar-cash.check-no  
            tt-ar-cash.company    = ar-cash.company 
            tt-ar-cash.cust-no    = ar-cash.cust-no  
            tt-ar-cash.posted     = ar-cash.posted   
            .  

    /*                         v-gltrans-desc = "VOID " + cust.cust-no + " " + */
    /*                             STRING(tt-ar-cash.check-no,"9999999999") +  */
    /*                             " Inv# " + STRING(tt-ar-cashl.inv-no).      */
    /*                          FIND FIRST gltrans  WHERE                      */
    /*                             gltrans.company EQ cust.company AND         */
    /*                             gltrans.jrnl EQ "CASHRVD" AND               */
    /*                             gltrans.tr-dscr EQ v-gltrans-desc           */
    /*                             NO-LOCK NO-ERROR.                           */
    END.




    FOR EACH ar-cashl 
        WHERE ar-cashl.company EQ cocode
        AND ar-cashl.posted EQ TRUE
        
          AND ar-cashl.cust-no     GE cFromCust
            AND ar-cashl.cust-no     LE cToCust
        NO-LOCK:
        CREATE tt-ar-cashl.
        ASSIGN
            tt-ar-cashl.company  = ar-cashl.company
            tt-ar-cashl.amt-paid = ar-cashl.amt-paid
            tt-ar-cashl.rec_key  = ar-cashl.rec_key
            tt-ar-cashl.inv-no   = ar-cashl.inv-no
            tt-ar-cashl.memo     = ar-cashl.memo
            tt-ar-cashl.amt-disc = ar-cashl.amt-disc            
            tt-ar-cashl.c-no     = ar-cashl.c-no
            tt-ar-cashl.posted   = ar-cashl.posted
            tt-ar-cashl.cust-no  = ar-cashl.cust-no  
            tt-ar-cashl.check-no = ar-cashl.check-no
            .


        FIND FIRST reftable WHERE
            reftable.reftable EQ "ARCASHLVDDATE" AND
            reftable.rec_key EQ tt-ar-cashl.rec_key
            USE-INDEX rec_key
            NO-LOCK NO-ERROR.
        IF AVAIL reftable THEN 
        DO:
            CREATE tt-reftable.
            ASSIGN
                tt-reftable.reftable = reftable.reftable
                tt-reftable.rec_key  = reftable.rec_key
                tt-reftable.CODE     = reftable.CODE.
        END.

    END.
/* end tt creation section */



    /* Start processing */
    FOR EACH company WHERE
        company.company GE b-comp AND
        company.company LE e-comp
        NO-LOCK,
        EACH ttCustList 
        WHERE ttCustList.log-fld,
        EACH cust 
      FIELDS(company cust-no sman curr-code name area-code
        phone terms fax cr-lim contact addr city state zip)
        NO-LOCK
        WHERE cust.company EQ company.company
        AND cust.cust-no EQ ttCustList.cust-no
        AND cust.sman    GE v-s-sman
        AND cust.sman    LE v-e-sman
        AND cust.terms    GE v-s-terms
        AND cust.terms    LE v-e-terms
        AND (cust.ACTIVE NE "I" OR v-inactive-custs)
        AND ((cust.curr-code GE v-s-curr    AND
        cust.curr-code LE v-e-curr)       OR
        (cust.curr-code EQ ""          AND
        company.curr-code GE v-s-curr AND
        company.curr-code LE v-e-curr)):
     
        STATUS DEFAULT "Checking Customer: " + TRIM(cust.cust-no).
        PROCESS EVENTS.
    
        ll-valid-cust = NO.

        IF NOT ll-valid-cust THEN
            FOR EACH ar-inv                                       
    FIELDS(company posted cust-no inv-date                   
                terms x-no due-date net gross                     
                freight tax-amt inv-no)                    
                NO-LOCK                                           
                WHERE ar-inv.company     EQ cust.company            
                AND ar-inv.posted      EQ YES                     
                AND ar-inv.cust-no     EQ cust.cust-no            
                AND ar-inv.inv-date    LE v-date                  
                AND ar-inv.terms       NE "CASH":
                IF NOT v-include-factored AND                                         
                    CAN-FIND(FIRST tt-factored                                                    
                    WHERE tt-factored.x-no EQ ar-inv.x-no) THEN              
                    NEXT.

                ll-valid-cust = YES.

                LEAVE.
            END.

        IF NOT ll-valid-cust THEN
            FOR EACH ar-cash                                            
    FIELDS(company cust-no posted check-date c-no check-no)  
                WHERE ar-cash.company     EQ cust.company                 
                AND ar-cash.cust-no     EQ cust.cust-no                 
                AND (ar-cash.check-date LE v-date OR                         
                ar-cash.check-date EQ ?)                           
                AND ar-cash.posted      EQ YES                        
                USE-INDEX ar-cash,                                                                                               
                EACH ar-cashl                                           
    FIELDS(check-no c-no posted inv-no company                     
                cust-no memo amt-disc amt-paid on-account rec_key)        
                NO-LOCK                                                 
                WHERE ar-cashl.c-no       EQ ar-cash.c-no                 
                AND ar-cashl.posted     EQ YES                        
                USE-INDEX c-no:                                                                                                
                IF ar-cashl.inv-no NE 0 THEN 
                DO:                         
                    FIND FIRST ar-inv NO-LOCK                                   
                        WHERE ar-inv.company     EQ cust.company                  
                        AND ar-inv.inv-no      EQ ar-cashl.inv-no               
                        AND ar-inv.inv-date    GT v-date                     
                        USE-INDEX inv-no NO-ERROR.                          
                    IF NOT AVAIL ar-inv THEN NEXT.                        
                END.                                                    
                IF ar-cashl.amt-paid GT 0 THEN 
                DO:                      
                    FIND FIRST reftable WHERE                                    
                        reftable.reftable EQ "ARCASHLVDDATE" AND                
                        reftable.rec_key EQ ar-cashl.rec_key                    
                        USE-INDEX rec_key                                      
                        NO-LOCK NO-ERROR.                                  
                    IF AVAIL reftable THEN                                     
                        v-check-date = DATE(reftable.CODE).                  
                    ELSE                                                    
                    DO:                                                        
                        v-gltrans-desc = "VOID " + cust.cust-no + " " +                          
                            STRING(ar-cash.check-no,"9999999999")
                            +                          " Inv# " + STRING(ar-cashl.inv-no).         
                        FIND FIRST gltrans WHERE               gltrans.company EQ cust.company
                            AND               gltrans.jrnl EQ "CASHRVD"
                            AND               gltrans.tr-dscr EQ v-gltrans-desc              
                            NO-LOCK NO-ERROR.         
                        IF AVAIL gltrans THEN            
                            v-check-date = gltrans.tr-date.         
                        ELSE            
                            v-check-date = ar-cash.check-date.      
                    END.      
                END.      
                ELSE v-check-date = ar-cash.check-date.      
                IF v-check-date NE ? AND v-check-date GT v-date THEN         
                    NEXT.

                ll-valid-cust = YES.

                LEAVE.
            END.

        IF ll-valid-cust THEN 
        DO:
            CREATE tt-cust.
            ASSIGN
                tt-cust.curr-code = IF cust.curr-code EQ "" THEN company.curr-code
                                                   ELSE cust.curr-code
                tt-cust.sorter    = (if ipcSort1 eq "cust.name" then cust.name else
                                     if ipcSort1 eq "Cust.sman" then cust.sman else  
                                     if ipcSort1 eq "Cust.cust-no" then cust.cust-no else "")
                tt-cust.row-id    = ROWID(cust).

            IF tt-cust.curr-code NE company.curr-code THEN ll-mult-curr = YES.
        END.
    END.

    FOR EACH tt-cust,
        FIRST cust 
      FIELDS(company cust-no sman curr-code NAME area-code
        phone terms fax cr-lim contact addr city state zip)
        NO-LOCK
        WHERE ROWID(cust) EQ tt-cust.row-id
        BREAK BY tt-cust.curr-code
        BY tt-cust.sorter:

        STATUS DEFAULT "Printing Currency/" + TRIM(v-sort) + ": " +
            TRIM(tt-cust.curr-code) + "/" + TRIM(tt-cust.sorter).
        PROCESS EVENTS.
    
        IF FIRST-OF(tt-cust.curr-code) THEN 
        DO:
            lv-page-break = "Currency: " + TRIM(tt-cust.curr-code).

            IF FIRST(tt-cust.curr-code) THEN 
            DO:
                IF ll-mult-curr THEN VIEW FRAME r-top-1.
            /*VIEW FRAME r-top-2.*/
            END.

            IF ll-mult-curr OR FIRST(tt-cust.curr-code) THEN PAGE.
        END.

        IF FIRST-OF(tt-cust.sorter)     AND
            NOT FIRST(tt-cust.curr-code) AND
            ipcSort1 EQ "cust.sman"  THEN PAGE.

        FIND FIRST sman NO-LOCK
            WHERE sman.company EQ cust.company
            AND sman.sman    EQ cust.sman
            NO-ERROR.
        v-sman = cust.sman + "-" + (IF AVAIL sman THEN sman.sname
        ELSE "Slsmn not on file").

        v-first-cust = YES.

        
/*         EMPTY  TEMP-TABLE tt-inv.     */
/*         EMPTY TEMP-TABLE tt-ar-cash.  */
/*         EMPTY TEMP-TABLE tt-ar-cashl. */
/*         EMPTY TEMP-TABLE tt-reftable. */
/* Section moved from here to top */        
       
       
        FOR EACH tt-inv,
            FIRST ar-inv WHERE ROWID(ar-inv) EQ tt-inv.row-id NO-LOCK
            BY tt-inv.sorter
            BY tt-inv.inv-no:

            /* Inserted because AR stores gross wrong */
            IF tt-inv.net EQ tt-inv.gross + tt-inv.freight + tt-inv.tax-amt THEN
                amt = tt-inv.net.
            ELSE
                amt = tt-inv.gross.

            IF amt EQ ? THEN amt = 0.

            /* if fuel surcharge should not be aged, get it out of 'amt' */
            IF NOT v-include-fuel THEN 
                FOR EACH ar-invl NO-LOCK 
                    WHERE ar-invl.x-no EQ tt-inv.x-no
                    AND CAN-FIND(FIRST itemfg WHERE itemfg.company EQ ar-invl.company
                    AND itemfg.i-no    EQ ar-invl.i-no
                    AND itemfg.procat  EQ "FS"):
                    ASSIGN 
                        amt = amt - ar-invl.amt.
                END.

            cPoNo = "". 
            cJobStr = "".
            FOR EACH ar-invl NO-LOCK 
                WHERE ar-invl.x-no EQ tt-inv.x-no:
                IF ar-invl.po-no GT "" THEN
                    ASSIGN cPoNo = ar-invl.po-no.
                IF ar-invl.job-no GT "" THEN
                    cJobStr = ar-invl.job-no + "-" + STRING(ar-invl.job-no2, "99").
            END.


            ASSIGN
                ag     = amt
                d      = v-date - if ipcDateToUse eq "ar-inv.due-date" then tt-inv.due-date  
                                  else if ipcDateToUse eq "ar-inv.inv-date" then tt-inv.inv-date  
                                  else ?  
                ni     = ni + 1
                v-type = IF tt-inv.terms EQ "FCHG" THEN "FC" ELSE "IN".

            FOR EACH tt-ar-cashl
                WHERE tt-ar-cashl.company  EQ tt-inv.company
                AND tt-ar-cashl.posted   EQ YES
                AND tt-ar-cashl.cust-no  EQ tt-inv.cust-no
                AND tt-ar-cashl.inv-no   EQ tt-inv.inv-no
                USE-INDEX inv-no NO-LOCK,
                FIRST tt-ar-cash
                WHERE tt-ar-cash.c-no       EQ tt-ar-cashl.c-no
                AND tt-ar-cash.check-date LE v-date
                USE-INDEX c-no NO-LOCK
                BY tt-ar-cashl.rec_key:

                IF tt-ar-cashl.amt-paid GT 0 THEN
                DO:
                    FIND FIRST tt-reftable WHERE                        
                        tt-reftable.reftable EQ "ARCASHLVDDATE" AND      
                        tt-reftable.rec_key EQ tt-ar-cashl.rec_key          
                        USE-INDEX rec_key
                        NO-LOCK NO-ERROR.                             
                    IF AVAIL tt-reftable THEN                             
                        v-check-date = DATE(tt-reftable.CODE).             
                    ELSE                                               
                    DO:                                                
                        v-gltrans-desc = "VOID " + cust.cust-no + " " + 
                            STRING(tt-ar-cash.check-no,"9999999999") +
                            " Inv# " + STRING(tt-ar-cashl.inv-no).
                        FIND FIRST gltrans WHERE 
                            gltrans.company EQ cust.company AND
                            gltrans.jrnl EQ "CASHRVD" AND
                            gltrans.tr-dscr EQ v-gltrans-desc
                            NO-LOCK NO-ERROR.
                        IF AVAIL gltrans THEN
                            v-check-date = gltrans.tr-date.
                        ELSE
                            v-check-date = tt-ar-cash.check-date.
                    END.
                END.
                ELSE
                    v-check-date = tt-ar-cash.check-date.

                IF v-check-date NE ? AND v-check-date GT v-date THEN NEXT.

                IF tt-ar-cashl.memo THEN

                    IF tt-ar-cashl.amt-disc NE 0 AND tt-ar-cashl.amt-paid EQ 0 THEN
                        ag = ag - tt-ar-cashl.amt-disc.
                    ELSE 
                        IF tt-ar-cashl.amt-paid + tt-ar-cashl.amt-disc GT 0 THEN
                            ag = ag + (tt-ar-cashl.amt-paid + tt-ar-cashl.amt-disc).
                        ELSE
                            ag = ag + (tt-ar-cashl.amt-paid + (- (tt-ar-cashl.amt-disc))).
                ELSE
                    ag = ag + ((tt-ar-cashl.amt-paid * -1) + (tt-ar-cashl.amt-disc * -1)).
            END.

            IF ag NE 0                                                     OR
                (v-inc AND
                tt-inv.inv-date GE v-s-dat AND tt-inv.inv-date LE v-e-dat) THEN 
            DO:
                IF v-first-cust THEN 
                DO:
                    ASSIGN 
                        paid-amt = 0  
                        m3       = ""  
                        ni       = 0.
                    IF cust.area-code NE "" THEN
                        m3 = STRING(cust.area-code,"(999) ").

                    m3 = m3 + string(cust.phone,"999-9999").

                    FIND FIRST terms WHERE terms.company = cust.company AND
                        terms.t-code = cust.terms NO-LOCK NO-ERROR.

                    /* If input trend days entered, then do the trend days calculation. */
                    IF  v-trend-days > 0 THEN
                        RUN get-trend-days (INPUT v-trend-days,
                            OUTPUT v-current-trend-days).

                    IF det-rpt = 1 THEN 
                    DO:
         
                    /* Display statement removed here */

                    END. /* if det-rpt = 1 */
        
                    v-first-cust = NO.
                END.

                IF d GE v-days[3] THEN 
                DO:
                    /* Display statement removed here */

                    v-int = 4.
                END.
       
                ELSE
                    IF d GE v-days[2] THEN 
                    DO:
                        /* Display statement removed here */

                        v-int = 3.
                    END.
       
                    ELSE
                        IF d GE v-days[1] THEN 
                        DO:
                            /* Display statement removed here */

                            v-int = 2.
                        END.
       
                        ELSE 
                        DO:
                            /* Display statement removed here */

                            v-int = 1.
                        END.

                /* Display statement removed here */

                ASSIGN
                    cust-t[v-int] = cust-t[v-int] + ag
                    v-dec         = 0
                    v-dec[v-int]  = ag.

                IF v-sep-fc THEN
                DO:
                    IF v-type NE "FC" THEN
                        cust-t-pri[v-int] = cust-t-pri[v-int] + ag.
                    ELSE
                        cust-t-fc[v-int] = cust-t-fc[v-int] + ag.
                END.
       
                /* Run Export statement removed here */
                IF det-rpt = 1 THEN 
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
                                cVarValue = STRING(cust.cust-no,"x(8)")  .
                            WHEN "cust-name" THEN 
                                cVarValue = STRING(cust.NAME,"x(30)")  .
                            WHEN "cont"      THEN 
                                cVarValue = STRING(cust.contact,"x(15)") .
                            WHEN "sman"      THEN 
                                cVarValue = STRING(v-sman,"x(25)") .
                            WHEN "term"      THEN 
                                cVarValue = IF AVAIL terms THEN STRING(terms.dscr,"x(15)") ELSE ""  .
                            WHEN "add1"      THEN 
                                cVarValue = STRING(cust.addr[1],"x(25)").
                            WHEN "add2"      THEN 
                                cVarValue = STRING(cust.addr[2],"x(25)").
                            WHEN "city"      THEN 
                                cVarValue = STRING(cust.city,"x(10)") .
                            WHEN "stat"      THEN 
                                cVarValue = STRING(cust.state,"x(5)") .
                            WHEN "zip"       THEN 
                                cVarValue = STRING(cust.zip,"x(10)")  .
                            WHEN "cre-lim"   THEN 
                                cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99") .
                            WHEN "phone"     THEN 
                                cVarValue = TRIM(STRING(cust.area-code,"(xxx)") + string(cust.phone,"xxx-xxxx")) .
                            WHEN "fax"       THEN 
                                cVarValue = TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                            WHEN "chk-memo"  THEN 
                                cVarValue = STRING("0").
                            WHEN "day-old"   THEN 
                                cVarValue = STRING(d,">>>>>>>>"). /*8*/
                            WHEN "type"      THEN 
                                cVarValue = STRING(v-type,"x(4)").
                            WHEN "inv"       THEN 
                                cVarValue = STRING(tt-inv.inv-no,">>>>>>>>") .
                            WHEN "inv-date"  THEN 
                                cVarValue = STRING(tt-inv.inv-date,"99/99/99") .
                            WHEN "amount"    THEN 
                                cVarValue = STRING(amt,"->>>>>>>>9.99").
                            WHEN "current"   THEN 
                                cVarValue = STRING(v-dec[1],"->>>>>>>>9.99").
                            WHEN "adtp"      THEN 
                                cVarValue = STRING(cust.avg-pay,">>>9").
                            WHEN "td"        THEN 
                                cVarValue = STRING(v-current-trend-days,"->>9").
                            WHEN "per-1"     THEN 
                                cVarValue = STRING(v-dec[2],"->>>>>>>>9.99") .
                            WHEN "per-2"     THEN 
                                cVarValue = STRING(v-dec[3],"->>>>>>>>9.99").
                            WHEN "per-3"     THEN 
                                cVarValue = STRING(v-dec[4],"->>>>>>>>9.99") .
                            WHEN "cust-po"   THEN 
                                cVarValue = STRING(cPoNo,"x(15)") .
                            WHEN "job"       THEN 
                                cVarValue = STRING(cJobStr,"x(9)")  .
                            WHEN "inv-note"  THEN 
                                cVarValue = "".
                            WHEN "coll-note" THEN 
                                cVarValue = "".
                    
                        END CASE.
                  
                        cExcelVarValue = cVarValue.
                        cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                    END.
        
                    PUT UNFORMATTED cDisplay SKIP.

                    IF sPrtInvNote THEN RUN Display-InvNote.
                    IF sPrtCollectionNote THEN RUN Display-CollectionNote.

                    IF v-export THEN 
                    DO:
                        PUT STREAM s-temp UNFORMATTED  
                            cExcelDisplay SKIP.
                    END.
                END.  /* if det-rpt = 1 THEN */

                FOR EACH tt-ar-cashl
                    WHERE tt-ar-cashl.company  EQ tt-inv.company
                    AND tt-ar-cashl.posted   EQ YES
                    AND tt-ar-cashl.cust-no  EQ tt-inv.cust-no
                    AND tt-ar-cashl.inv-no   EQ tt-inv.inv-no
                    USE-INDEX inv-no NO-LOCK,

                    FIRST tt-ar-cash
                    WHERE tt-ar-cash.c-no       EQ tt-ar-cashl.c-no
                    AND tt-ar-cash.check-date LE v-date
                    USE-INDEX c-no NO-LOCK:

                    IF tt-ar-cashl.amt-paid GT 0 THEN
                    DO:
                        FIND FIRST tt-reftable WHERE                        
                            tt-reftable.reftable EQ "ARCASHLVDDATE" AND      
                            tt-reftable.rec_key EQ tt-ar-cashl.rec_key
                            USE-INDEX rec_key
                            NO-LOCK NO-ERROR.                             
                        IF AVAIL tt-reftable THEN                             
                            v-check-date = DATE(tt-reftable.CODE).             
                        ELSE                                               
                        DO:                                                
                            v-gltrans-desc = "VOID " + cust.cust-no + " " + 
                                STRING(tt-ar-cash.check-no,"9999999999") +
                                " Inv# " + STRING(tt-ar-cashl.inv-no).
                            FIND FIRST gltrans WHERE 
                                gltrans.company EQ cust.company AND
                                gltrans.jrnl EQ "CASHRVD" AND
                                gltrans.tr-dscr EQ v-gltrans-desc
                                NO-LOCK NO-ERROR.
                            IF AVAIL gltrans THEN
                                v-check-date = gltrans.tr-date.
                            ELSE
                                v-check-date = tt-ar-cash.check-date.
                        END.
                    END.
                    ELSE
                        v-check-date = tt-ar-cash.check-date.

                    IF v-check-date NE ? AND v-check-date GT v-date THEN NEXT.

                    IF tt-ar-cashl.memo THEN

                        /* CTS CM/DM signs are reversed *****************************/
                        /*if (tt-ar-cashl.amt-paid + tt-ar-cashl.amt-disc) lt 0 then
                           assign v-type = "CM"
                                  v-cr-db-amt = tt-ar-cashl.amt-paid
                                  v-disc-amt = tt-ar-cashl.amt-disc.
             
                        else*/
                        IF (tt-ar-cashl.amt-paid + tt-ar-cashl.amt-disc) GT 0 THEN
                            ASSIGN v-type      = "DM"
                                v-cr-db-amt = tt-ar-cashl.amt-paid
                                v-disc-amt  = tt-ar-cashl.amt-disc.

                        ELSE
                            ASSIGN v-type      = "CM"
                                v-cr-db-amt = tt-ar-cashl.amt-paid
                                v-disc-amt  = - (tt-ar-cashl.amt-disc).

                    ELSE
                    DO:
                        v-tr-dscr = "VOID " + cust.cust-no + " "
                            + STRING(tt-ar-cash.check-no,"9999999999")
                            + " Inv# " + STRING(tt-ar-cashl.inv-no).

                        IF tt-ar-cashl.amt-paid GT 0 AND
                            (CAN-FIND(FIRST tt-reftable WHERE
                            tt-reftable.reftable = "ARCASHLVDDATE" AND
                            tt-reftable.rec_key = tt-ar-cashl.rec_key
                            USE-INDEX rec_key) OR
                            CAN-FIND(FIRST gltrans WHERE
                            gltrans.company EQ cust.company AND
                            gltrans.jrnl EQ "CASHRVD" AND
                            gltrans.tr-dscr EQ v-tr-dscr)) THEN
                            v-type = "VD".
                        ELSE
                            v-type = "PY".

                        ASSIGN
                            v-cr-db-amt = tt-ar-cashl.amt-paid * -1
                            v-disc-amt  = tt-ar-cashl.amt-disc * -1.

                        /*IF v-type = "PY" AND v-cr-db-amt GT 0 THEN
                           v-cr-db-amt = v-cr-db-amt * -1.
                        ELSE*/
                        IF v-type EQ "VD" AND v-cr-db-amt LT 0 THEN
                            v-cr-db-amt = v-cr-db-amt * -1.
                    END.

                    IF v-disc-amt NE 0 THEN 
                    DO:

                        v-disc-type = "DISC".

                        IF tt-ar-cashl.memo THEN
                            ASSIGN
                                v-disc-type = "RETN"
                                v-disc-amt  = - v-disc-amt.

                        IF det-rpt = 1 THEN 
                        DO:
                            IF v-disc-type EQ "DISC" THEN 
                            DO:
                                /* display tt-ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                                         v-type at 16
                                         tt-ar-cashl.inv-no at 23
                                         tt-ar-cash.check-date at 31 format "99/99/99"
                                         v-cr-db-amt to 54 skip
                                     with frame f-1 no-box no-labels stream-io width 200.
                                     
                                 if v-export then
                                   run export-data (tt-ar-cashl.check-no, 0, v-type,
                                                    string(tt-ar-cashl.inv-no,">>>>>>>>>>"),
                                                    tt-ar-cash.check-date, v-cr-db-amt, 0, 0, 0, 0).*/
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
                                            cVarValue = STRING(cust.cust-no,"x(8)")  .
                                        WHEN "cust-name" THEN 
                                            cVarValue = STRING(cust.NAME,"x(30)")  .
                                        WHEN "cont"      THEN 
                                            cVarValue = STRING(cust.contact,"x(15)") .
                                        WHEN "sman"      THEN 
                                            cVarValue = STRING(v-sman,"x(25)") .
                                        WHEN "term"      THEN 
                                            cVarValue = IF AVAIL terms THEN STRING(terms.dscr,"x(15)") ELSE ""  .
                                        WHEN "add1"      THEN 
                                            cVarValue = STRING(cust.addr[1],"x(25)").
                                        WHEN "add2"      THEN 
                                            cVarValue = STRING(cust.addr[2],"x(25)").
                                        WHEN "city"      THEN 
                                            cVarValue = STRING(cust.city,"x(10)") .
                                        WHEN "stat"      THEN 
                                            cVarValue = STRING(cust.state,"x(5)") .
                                        WHEN "zip"       THEN 
                                            cVarValue = STRING(cust.zip,"x(10)")  .
                                        WHEN "cre-lim"   THEN 
                                            cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99") .
                                        WHEN "phone"     THEN 
                                            cVarValue = TRIM(STRING(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                                        WHEN "fax"       THEN 
                                            cVarValue = TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                                        WHEN "chk-memo"  THEN 
                                            cVarValue = STRING(tt-ar-cashl.check-no).
                                        WHEN "day-old"   THEN 
                                            cVarValue = STRING("").
                                        WHEN "type"      THEN 
                                            cVarValue = STRING(v-type,"x(4)").
                                        WHEN "inv"       THEN 
                                            cVarValue = STRING(tt-ar-cashl.inv-no,">>>>>>>>") .
                                        WHEN "inv-date"  THEN 
                                            cVarValue = STRING(tt-ar-cash.check-date,"99/99/99") .
                                        WHEN "amount"    THEN 
                                            cVarValue = STRING(v-cr-db-amt,"->>>>>>>>9.99").
                                        WHEN "current"   THEN 
                                            cVarValue = /*STRING(v-dec[1],"->>>>>>>>9.99")*/ "".
                                        WHEN "adtp"      THEN 
                                            cVarValue = STRING(cust.avg-pay,">>>9").
                                        WHEN "td"        THEN 
                                            cVarValue = STRING(v-current-trend-days,"->>9").
                                        WHEN "per-1"     THEN 
                                            cVarValue = /*STRING(v-dec[2],"->>>>>>>>9.99")"*/ "" .
                                        WHEN "per-2"     THEN 
                                            cVarValue = /*STRING(v-dec[3],"->>>>>>>>9.99")*/ "" .
                                        WHEN "per-3"     THEN 
                                            cVarValue = /*STRING(v-dec[4],"->>>>>>>>9.99")*/ "" .
                                        WHEN "cust-po"   THEN 
                                            cVarValue = STRING(cPoNo,"x(15)") .
                                        WHEN "job"       THEN 
                                            cVarValue = STRING(cJobStr,"x(10)")  .
                                        WHEN "inv-note"  THEN 
                                            cVarValue = "".
                                        WHEN "coll-note" THEN 
                                            cVarValue = "".
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
                                IF sPrtInvNote THEN RUN Display-InvNote.
                            END.
                            /*IF det-rpt <> 3 THEN
                            display tt-ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                                    v-disc-type at 16
                                    tt-ar-cashl.inv-no at 23
                                    tt-ar-cash.check-date at 31 format "99/99/99"
                                    v-disc-amt to 54
                                with frame f-50{&frame} no-box no-labels stream-io width 200.
                                
                            if v-export then
                              run export-data (tt-ar-cashl.check-no, 0, v-disc-type,
                                               string(tt-ar-cashl.inv-no,">>>>>>>>>>"),
                                               tt-ar-cash.check-date, v-disc-amt, 0, 0, 0, 0).*/
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
                                        cVarValue = STRING(cust.cust-no,"x(8)")  .
                                    WHEN "cust-name" THEN 
                                        cVarValue = STRING(cust.NAME,"x(30)")  .
                                    WHEN "cont"      THEN 
                                        cVarValue = STRING(cust.contact,"x(15)") .
                                    WHEN "sman"      THEN 
                                        cVarValue = STRING(v-sman,"x(25)") .
                                    WHEN "term"      THEN 
                                        cVarValue = IF AVAIL terms THEN STRING(terms.dscr,"x(15)") ELSE ""  .
                                    WHEN "add1"      THEN 
                                        cVarValue = STRING(cust.addr[1],"x(25)").
                                    WHEN "add2"      THEN 
                                        cVarValue = STRING(cust.addr[2],"x(25)").
                                    WHEN "city"      THEN 
                                        cVarValue = STRING(cust.city,"x(10)") .
                                    WHEN "stat"      THEN 
                                        cVarValue = STRING(cust.state,"x(5)") .
                                    WHEN "zip"       THEN 
                                        cVarValue = STRING(cust.zip,"x(10)")  .
                                    WHEN "cre-lim"   THEN 
                                        cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99") .
                                    WHEN "phone"     THEN 
                                        cVarValue = TRIM(STRING(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                                    WHEN "fax"       THEN 
                                        cVarValue = TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                                    WHEN "chk-memo"  THEN 
                                        cVarValue = STRING(tt-ar-cashl.check-no).
                                    WHEN "day-old"   THEN 
                                        cVarValue = STRING("").
                                    WHEN "type"      THEN 
                                        cVarValue = STRING(v-disc-type,"x(4)").
                                    WHEN "inv"       THEN 
                                        cVarValue = STRING(tt-ar-cashl.inv-no,">>>>>>>>") .
                                    WHEN "inv-date"  THEN 
                                        cVarValue = STRING(tt-ar-cash.check-date,"99/99/99") .
                                    WHEN "amount"    THEN 
                                        cVarValue = STRING(v-disc-amt,"->>>>>>>>9.99").
                                    WHEN "current"   THEN 
                                        cVarValue = /*STRING(v-dec[1],"->>>>>>>>9.99")*/ "".
                                    WHEN "adtp"      THEN 
                                        cVarValue = STRING(cust.avg-pay,">>>9").
                                    WHEN "td"        THEN 
                                        cVarValue = STRING(v-current-trend-days,"->>9").
                                    WHEN "per-1"     THEN 
                                        cVarValue = /*STRING(v-dec[2],"->>>>>>>>9.99")"*/ "" .
                                    WHEN "per-2"     THEN 
                                        cVarValue = /*STRING(v-dec[3],"->>>>>>>>9.99")*/ "" .
                                    WHEN "per-3"     THEN 
                                        cVarValue = /*STRING(v-dec[4],"->>>>>>>>9.99")*/ "" .
                                    WHEN "cust-po"   THEN 
                                        cVarValue = STRING(cPoNo,"x(15)") .
                                    WHEN "job"       THEN 
                                        cVarValue = STRING(cJobStr,"x(10)")  .
                                    WHEN "inv-note"  THEN 
                                        cVarValue = "".
                                    WHEN "coll-note" THEN 
                                        cVarValue = "".
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
                            IF sPrtInvNote THEN RUN Display-InvNote.
                    
                        END.
                    END.

                    ELSE
                        IF det-rpt = 1 THEN 
                        DO:

                            IF v-type EQ "VD" THEN
                            DO:
                                FIND FIRST tt-reftable WHERE
                                    tt-reftable.reftable EQ "ARCASHLVDDATE" AND
                                    tt-reftable.rec_key EQ tt-ar-cashl.rec_key
                                    USE-INDEX rec_key
                                    NO-LOCK NO-ERROR.

                                IF AVAIL tt-reftable THEN
                                    v-check-date = DATE(tt-reftable.CODE).
                                ELSE
                                DO:
                                    v-gltrans-desc = "VOID " + cust.cust-no + " " +
                                        STRING(tt-ar-cash.check-no,"9999999999") +
                                        " Inv# " + STRING(tt-ar-cashl.inv-no).

                                    FIND FIRST gltrans WHERE
                                        gltrans.company EQ cust.company AND
                                        gltrans.jrnl EQ "CASHRVD" AND
                                        gltrans.tr-dscr EQ v-gltrans-desc
                                        NO-LOCK NO-ERROR.
                
                                    IF AVAIL gltrans THEN
                                        v-check-date = gltrans.tr-date.
                                    ELSE
                                        v-check-date = tt-ar-cash.check-date.
                                END.
                            END.
                            ELSE
                                v-check-date = tt-ar-cash.check-date.
                            /* IF det-rpt = 1 THEN
                             display tt-ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                                     v-type at 16
                                     tt-ar-cashl.inv-no at 23
                                     v-check-date @ tt-ar-cash.check-date at 31 format "99/99/99"
                                     v-cr-db-amt to 54
                                 with frame f-100 no-box no-labels stream-io width 200.
                                 
                             if v-export AND det-rpt = 1 then
                               run export-data (tt-ar-cashl.check-no, 0, v-type,
                                                string(tt-ar-cashl.inv-no,">>>>>>>>>>"),
                                                v-check-date, v-cr-db-amt, 0, 0, 0, 0).*/
                            IF det-rpt = 1 THEN 
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
                                            cVarValue = STRING(cust.cust-no,"x(8)")  .
                                        WHEN "cust-name" THEN 
                                            cVarValue = STRING(cust.NAME,"x(30)")  .
                                        WHEN "cont"      THEN 
                                            cVarValue = STRING(cust.contact,"x(15)") .
                                        WHEN "sman"      THEN 
                                            cVarValue = STRING(v-sman,"x(25)") .
                                        WHEN "term"      THEN 
                                            cVarValue = IF AVAIL terms THEN STRING(terms.dscr,"x(15)") ELSE ""  .
                                        WHEN "add1"      THEN 
                                            cVarValue = STRING(cust.addr[1],"x(25)").
                                        WHEN "add2"      THEN 
                                            cVarValue = STRING(cust.addr[2],"x(25)").
                                        WHEN "city"      THEN 
                                            cVarValue = STRING(cust.city,"x(10)") .
                                        WHEN "stat"      THEN 
                                            cVarValue = STRING(cust.state,"x(5)") .
                                        WHEN "zip"       THEN 
                                            cVarValue = STRING(cust.zip,"x(10)")  .
                                        WHEN "cre-lim"   THEN 
                                            cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99") .
                                        WHEN "phone"     THEN 
                                            cVarValue = TRIM(STRING(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                                        WHEN "fax"       THEN 
                                            cVarValue = TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                                        WHEN "chk-memo"  THEN 
                                            cVarValue = STRING(tt-ar-cashl.check-no).
                                        WHEN "day-old"   THEN 
                                            cVarValue = STRING("").
                                        WHEN "type"      THEN 
                                            cVarValue = STRING(v-type,"x(4)").
                                        WHEN "inv"       THEN 
                                            cVarValue = STRING(tt-ar-cashl.inv-no,">>>>>>>>") .
                                        WHEN "inv-date"  THEN 
                                            cVarValue = STRING(v-check-date,"99/99/99") .
                                        WHEN "amount"    THEN 
                                            cVarValue = STRING(v-cr-db-amt,"->>>>>>>>9.99").
                                        WHEN "current"   THEN 
                                            cVarValue = /*STRING(v-dec[1],"->>>>>>>>9.99")*/ "".
                                        WHEN "adtp"      THEN 
                                            cVarValue = STRING(cust.avg-pay,">>>9").
                                        WHEN "td"        THEN 
                                            cVarValue = STRING(v-current-trend-days,"->>9").
                                        WHEN "per-1"     THEN 
                                            cVarValue = /*STRING(v-dec[2],"->>>>>>>>9.99")"*/ "" .
                                        WHEN "per-2"     THEN 
                                            cVarValue = /*STRING(v-dec[3],"->>>>>>>>9.99")*/ "" .
                                        WHEN "per-3"     THEN 
                                            cVarValue = /*STRING(v-dec[4],"->>>>>>>>9.99")*/ "" .
                                        WHEN "cust-po"   THEN 
                                            cVarValue = STRING(cPoNo,"x(15)") .
                                        WHEN "job"       THEN 
                                            cVarValue = STRING(cJobStr,"x(10)")  .
                                        WHEN "inv-note"  THEN 
                                            cVarValue = "".
                                        WHEN "coll-note" THEN 
                                            cVarValue = "".
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
                                IF sPrtInvNote THEN RUN Display-InvNote.

                            END. /* det-prt = 1 */

                        END.
                END. /* for each tt-ar-cashl record */
            END.
        /* IF sPrtInvNote THEN RUN Display-InvNote.*/
        END. /* for each ar-inv record */

        ASSIGN 
            unapp[1] = 0
            unapp[2] = 0
            unapp[3] = 0
            unapp[4] = 0.

        /* This loop finds all unapplied balances and totals by age */
        FOR EACH tt-ar-cash                                            
              
            NO-LOCK                                                 
            WHERE tt-ar-cash.company     EQ cust.company                 
            AND tt-ar-cash.cust-no     EQ cust.cust-no                 
            AND (tt-ar-cash.check-date LE v-date OR                        
            tt-ar-cash.check-date EQ ?)                           
            AND tt-ar-cash.posted      EQ YES                        
            USE-INDEX tt-ar-cash, 
                                                                                                  
            EACH tt-ar-cashl                                           
         FIELDS(check-no c-no posted inv-no company                     
            cust-no memo amt-disc amt-paid on-account rec_key)        
            NO-LOCK                                                 
            WHERE tt-ar-cashl.c-no       EQ tt-ar-cash.c-no                 
            AND tt-ar-cashl.posted     EQ YES                        
            USE-INDEX c-no:                                                                                                 
            IF tt-ar-cashl.inv-no NE 0 THEN 
            DO:                           
                FIND FIRST ar-inv NO-LOCK                                    WHERE tt-inv.company     EQ cust.company                   AND tt-inv.inv-no      EQ tt-ar-cashl.inv-no                AND tt-inv.inv-date    GT v-date                       USE-INDEX inv-no NO-ERROR.                           
                IF NOT AVAIL ar-inv THEN NEXT.                         
            END.                                                     
            IF tt-ar-cashl.amt-paid GT 0 THEN 
            DO:                       
                FIND FIRST tt-reftable WHERE                                     tt-reftable.reftable EQ "ARCASHLVDDATE" AND                 tt-reftable.rec_key EQ tt-ar-cashl.rec_key                     USE-INDEX rec_key                                        NO-LOCK NO-ERROR.                                   
                IF AVAIL tt-reftable THEN                                      v-check-date = DATE(tt-reftable.CODE).                   
                ELSE                                                     
                DO:                                                         
                    v-gltrans-desc = "VOID " + cust.cust-no + " " +                           STRING(tt-ar-cash.check-no,"9999999999") +                          " Inv# " + STRING(tt-ar-cashl.inv-no).          
                    FIND FIRST gltrans WHERE               gltrans.company EQ cust.company AND               gltrans.jrnl EQ "CASHRVD" AND               gltrans.tr-dscr EQ v-gltrans-desc               NO-LOCK NO-ERROR.          
                    IF AVAIL gltrans THEN             v-check-date = gltrans.tr-date.          
                    ELSE             v-check-date = tt-ar-cash.check-date.       
                END.       
            END.       
            ELSE v-check-date = tt-ar-cash.check-date.       
            IF v-check-date NE ? AND v-check-date GT v-date THEN          NEXT.

            IF tt-ar-cashl.memo THEN 
            DO:

                /* CTS CM/DM signs are reversed *****************************/
                IF (tt-ar-cashl.amt-paid + tt-ar-cashl.amt-disc) GT 0 THEN
                    ASSIGN v-type      = "DM"
                        v-cr-db-amt = tt-ar-cashl.amt-paid
                        v-disc-amt  = tt-ar-cashl.amt-disc.

                ELSE
                    ASSIGN v-type      = "CM"
                        v-cr-db-amt = tt-ar-cashl.amt-paid
                        v-disc-amt  = tt-ar-cashl.amt-disc.
            END.

            ELSE
                ASSIGN v-cr-db-amt = tt-ar-cashl.amt-paid * -1
                    v-disc-amt  = tt-ar-cashl.amt-disc * -1.

            d = v-date - tt-ar-cash.check-date.

            IF d GE v-days[3] THEN
                unapp[4] = unapp[4] + v-cr-db-amt - v-disc-amt.
            ELSE
                IF d GE v-days[2] AND d LT v-days[3] THEN
                    unapp[3] = unapp[3] + v-cr-db-amt - v-disc-amt.
                ELSE
                    IF d GE v-days[1] AND d LT v-days[2] THEN
                        unapp[2] = unapp[2] + v-cr-db-amt - v-disc-amt.
                    ELSE
                        IF d LT v-days[1] THEN
                            unapp[1] = unapp[1] + v-cr-db-amt - v-disc-amt.

        END. /* for each tt-ar-cashl record */

        first-unapp = YES.
        /* this loop displays all unapplied balances */
        FOR EACH tt-ar-cash                                           
     FIELDS(company cust-no posted check-date c-no check-no)         
            NO-LOCK                                                 
            WHERE tt-ar-cash.company     EQ cust.company                
            AND tt-ar-cash.cust-no     EQ cust.cust-no                 
            AND (tt-ar-cash.check-date LE v-date OR                         
            tt-ar-cash.check-date EQ ?)                           
            AND tt-ar-cash.posted      EQ YES                        
            USE-INDEX tt-ar-cash,  
                                                                              
            EACH tt-ar-cashl                                           
         FIELDS(check-no c-no posted inv-no company                     
            cust-no memo amt-disc amt-paid on-account rec_key)        
            NO-LOCK                                                
            WHERE tt-ar-cashl.c-no       EQ tt-ar-cash.c-no                
            AND tt-ar-cashl.posted     EQ YES                        
            USE-INDEX c-no:                                                                                                 
            IF tt-ar-cashl.inv-no NE 0 THEN 
            DO:                           
                FIND FIRST ar-inv NO-LOCK                                    WHERE tt-inv.company     EQ cust.company                   AND tt-inv.inv-no      EQ tt-ar-cashl.inv-no                AND tt-inv.inv-date    GT v-date                       USE-INDEX inv-no NO-ERROR.                           
                IF NOT AVAIL ar-inv THEN NEXT.                         
            END.                                                     
            IF tt-ar-cashl.amt-paid GT 0 THEN 
            DO:                       
                FIND FIRST tt-reftable WHERE                                     tt-reftable.reftable EQ "ARCASHLVDDATE" AND                 tt-reftable.rec_key EQ tt-ar-cashl.rec_key                     USE-INDEX rec_key                                        NO-LOCK NO-ERROR.                                   
                IF AVAIL tt-reftable THEN                                      v-check-date = DATE(tt-reftable.CODE).                   
                ELSE                                                     
                DO:                                                         
                    v-gltrans-desc = "VOID " + cust.cust-no + " " +                           STRING(tt-ar-cash.check-no,"9999999999") +                          " Inv# " + STRING(tt-ar-cashl.inv-no).          
                    FIND FIRST gltrans WHERE               gltrans.company EQ cust.company AND               gltrans.jrnl EQ "CASHRVD" AND               gltrans.tr-dscr EQ v-gltrans-desc               NO-LOCK NO-ERROR.          
                    IF AVAIL gltrans THEN             v-check-date = gltrans.tr-date.          
                    ELSE             v-check-date = tt-ar-cash.check-date.       
                END.       
            END.       
            ELSE v-check-date = tt-ar-cash.check-date.       
            IF v-check-date NE ? AND v-check-date GT v-date THEN          NEXT.

            IF v-first-cust THEN 
            DO:
                ASSIGN
                    paid-amt   = 0
                    cust-t     = 0
                    m3         = ""
                    ni         = 0
                    cust-t-pri = 0
                    cust-t-fc  = 0.

                IF cust.area-code NE "" THEN
                    m3 = STRING(cust.area-code,"(999) ").

                m3 = m3 + string(cust.phone,"999-9999").

                FIND FIRST terms WHERE terms.company = cust.company AND
                    terms.t-code = cust.terms NO-LOCK NO-ERROR.

                /* if det-rpt = 1 then
                   display cust.cust-no
                           space(2)
                           cust.name
                           space(2)
                           cust.area-code                            format "(xxx)"
                           cust.phone                                format "xxx-xxxx"
                           "  Fax:"
                           substr(cust.fax,1,3)                      format "(xxx)"
                           substr(cust.fax,4,7)                      format "xxx-xxxx"
                           "  Credit Limit:"
                           string(cust.cr-lim,">,>>>,>>>,>>9.99")    format "x(17)"
                           skip
                           cust.contact
                           v-sman
                           space(3)
                           terms.dscr when avail terms
                     /* stacey */
         /*                   cust.avg-pay */
                           (v-trend-days - cust.avg-pay) WHEN v-trend-days > 0
                       with no-labels no-box frame a2 stream-io width 200.
                       
                 if v-prt-add then run print-cust-add.*/
       
                ASSIGN 
                    v-first-cust = NO.
            END.

            v-neg-text = "ON ACCT".

            IF tt-ar-cashl.memo EQ TRUE THEN 
            DO:
                IF (tt-ar-cashl.amt-paid + tt-ar-cashl.amt-disc) GT 0 THEN
                    ASSIGN v-type      = "DM"
                        v-cr-db-amt = tt-ar-cashl.amt-paid
                        v-disc-amt  = tt-ar-cashl.amt-disc.
                ELSE
                    ASSIGN v-type      = "CM"
                        v-cr-db-amt = tt-ar-cashl.amt-paid
                        v-disc-amt  = tt-ar-cashl.amt-disc.
            END.

            ELSE
            DO:
                v-tr-dscr = "VOID " + cust.cust-no + " "
                    + STRING(tt-ar-cash.check-no,"9999999999")
                    + " Inv# " + STRING(tt-ar-cashl.inv-no).

                IF CAN-FIND(FIRST tt-reftable WHERE
                    tt-reftable.reftable = "ARCASHLVDDATE" AND
                    tt-reftable.rec_key = tt-ar-cashl.rec_key
                    USE-INDEX rec_key) OR
                    CAN-FIND(FIRST gltrans WHERE
                    gltrans.company EQ cust.company AND
                    gltrans.jrnl EQ "CASHRVD" AND
                    gltrans.tr-dscr EQ v-tr-dscr) THEN
                DO:
                    ASSIGN
                        v-type     = "VD"
                        v-neg-text = "VOID".
                    RELEASE tt-reftable.
                END.
                ELSE
                    v-type = "PY".

                ASSIGN
                    v-cr-db-amt = tt-ar-cashl.amt-paid * -1
                    v-disc-amt  = tt-ar-cashl.amt-disc * -1.

                /* IF v-type = "PY" AND v-cr-db-amt GT 0 THEN
                   v-cr-db-amt = v-cr-db-amt * -1.
                ELSE */
                IF v-type EQ "VD" AND v-cr-db-amt LT 0 THEN
                    v-cr-db-amt = v-cr-db-amt * -1.
            END.

            IF first-unapp THEN 
            DO:
                IF v-type EQ "VD" THEN
                DO:
                    FIND FIRST tt-reftable WHERE
                        tt-reftable.reftable EQ "ARCASHLVDDATE" AND
                        tt-reftable.rec_key EQ tt-ar-cashl.rec_key
                        USE-INDEX rec_key
                        NO-LOCK NO-ERROR.
           
                    IF AVAIL tt-reftable THEN
                        v-check-date = DATE(tt-reftable.CODE).
                    ELSE
                    DO:
                        v-gltrans-desc = "VOID " + cust.cust-no + " " +
                            STRING(tt-ar-cash.check-no,"9999999999") +
                            " Inv# " + STRING(tt-ar-cashl.inv-no).

                        FIND FIRST gltrans WHERE
                            gltrans.company EQ cust.company AND
                            gltrans.jrnl EQ "CASHRVD" AND
                            gltrans.tr-dscr EQ v-gltrans-desc
                            NO-LOCK NO-ERROR.
              
                        IF AVAIL gltrans THEN
                            v-check-date = gltrans.tr-date.
                        ELSE
                            v-check-date = tt-ar-cash.check-date.
                    END.
                END.
                ELSE
                    v-check-date = tt-ar-cash.check-date.

                /* Display statement removed here */
            
                IF det-rpt = 1 THEN 
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
                                cVarValue = STRING(cust.cust-no,"x(8)")  .
                            WHEN "cust-name" THEN 
                                cVarValue = STRING(cust.NAME,"x(30)")  .
                            WHEN "cont"      THEN 
                                cVarValue = STRING(cust.contact,"x(15)") .
                            WHEN "sman"      THEN 
                                cVarValue = STRING(v-sman,"x(25)") .
                            WHEN "term"      THEN 
                                cVarValue = IF AVAIL terms THEN STRING(terms.dscr,"x(15)") ELSE ""  .
                            WHEN "add1"      THEN 
                                cVarValue = STRING(cust.addr[1],"x(25)").
                            WHEN "add2"      THEN 
                                cVarValue = STRING(cust.addr[2],"x(25)").
                            WHEN "city"      THEN 
                                cVarValue = STRING(cust.city,"x(10)") .
                            WHEN "stat"      THEN 
                                cVarValue = STRING(cust.state,"x(5)") .
                            WHEN "zip"       THEN 
                                cVarValue = STRING(cust.zip,"x(10)")  .
                            WHEN "cre-lim"   THEN 
                                cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99") .
                            WHEN "phone"     THEN 
                                cVarValue = TRIM(STRING(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                            WHEN "fax"       THEN 
                                cVarValue = TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                            WHEN "chk-memo"  THEN 
                                cVarValue = STRING(tt-ar-cashl.check-no).
                            WHEN "day-old"   THEN 
                                cVarValue = STRING("").
                            WHEN "type"      THEN 
                                cVarValue = STRING(v-type,"x(4)").
                            WHEN "inv"       THEN 
                                cVarValue = STRING(v-neg-text) .
                            WHEN "inv-date"  THEN 
                                cVarValue = STRING(v-check-date,"99/99/99") .
                            WHEN "amount"    THEN 
                                cVarValue = STRING(v-cr-db-amt + v-disc-amt,"->>>>>>>>9.99").
                            WHEN "current"   THEN 
                                cVarValue = STRING(unapp[1],"->>>>>>>>9.99").
                            WHEN "adtp"      THEN 
                                cVarValue = STRING(cust.avg-pay,">>>9").
                            WHEN "td"        THEN 
                                cVarValue = STRING(v-current-trend-days,"->>9").
                            WHEN "per-1"     THEN 
                                cVarValue = STRING(unapp[2],"->>>>>>>>9.99") .
                            WHEN "per-2"     THEN 
                                cVarValue = STRING(unapp[3],"->>>>>>>>9.99").
                            WHEN "per-3"     THEN 
                                cVarValue = STRING(unapp[4],"->>>>>>>>9.99") .
                            WHEN "cust-po"   THEN 
                                cVarValue = STRING(cPoNo,"x(15)") .
                            WHEN "job"       THEN 
                                cVarValue = STRING(cJobStr,"x(10)")  .
                            WHEN "inv-note"  THEN 
                                cVarValue = "".
                            WHEN "coll-note" THEN 
                                cVarValue = "".
                    
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
                END. /*det-prt = 1 */

                ASSIGN
                    cust-t[4] = cust-t[4] + unapp[4]
                    cust-t[3] = cust-t[3] + unapp[3]
                    cust-t[2] = cust-t[2] + unapp[2]
                    cust-t[1] = cust-t[1] + unapp[1].

                IF v-sep-fc THEN
                    ASSIGN
                        cust-t-pri[4] = cust-t-pri[4] + unapp[4]
                        cust-t-pri[3] = cust-t-pri[3] + unapp[3]
                        cust-t-pri[2] = cust-t-pri[2] + unapp[2]
                        cust-t-pri[1] = cust-t-pri[1] + unapp[1].
            END.

            IF first-unapp THEN first-unapp = NO.

            ELSE 
            DO:

                IF v-type EQ "VD" THEN
                DO:
                    FIND FIRST tt-reftable WHERE
                        tt-reftable.reftable EQ "ARCASHLVDDATE" AND
                        tt-reftable.rec_key EQ tt-ar-cashl.rec_key
                        USE-INDEX rec_key
                        NO-LOCK NO-ERROR.
          
                    IF AVAIL tt-reftable THEN
                        v-check-date = DATE(tt-reftable.CODE).
                    ELSE
                    DO:
                        v-gltrans-desc = "VOID " + cust.cust-no + " " +
                            STRING(tt-ar-cash.check-no,"9999999999") +
                            " Inv# " + STRING(tt-ar-cashl.inv-no).

                        FIND FIRST gltrans WHERE
                            gltrans.company EQ cust.company AND
                            gltrans.jrnl EQ "CASHRVD" AND
                            gltrans.tr-dscr EQ v-gltrans-desc
                            NO-LOCK NO-ERROR.
             
                        IF AVAIL gltrans THEN
                            v-check-date = gltrans.tr-date.
                        ELSE
                            v-check-date = tt-ar-cash.check-date.
                    END.
                END.
                ELSE
                    v-check-date = tt-ar-cash.check-date.

                /* Display statement removed here */
       
                IF det-rpt = 1 THEN 
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
                                cVarValue = STRING(cust.cust-no,"x(8)")  .
                            WHEN "cust-name" THEN 
                                cVarValue = STRING(cust.NAME,"x(30)")  .
                            WHEN "cont"      THEN 
                                cVarValue = STRING(cust.contact,"x(15)") .
                            WHEN "sman"      THEN 
                                cVarValue = STRING(v-sman,"x(25)") .
                            WHEN "term"      THEN 
                                cVarValue = IF AVAIL terms THEN STRING(terms.dscr,"x(15)") ELSE ""  .
                            WHEN "add1"      THEN 
                                cVarValue = STRING(cust.addr[1],"x(25)").
                            WHEN "add2"      THEN 
                                cVarValue = STRING(cust.addr[2],"x(25)").
                            WHEN "city"      THEN 
                                cVarValue = STRING(cust.city,"x(10)") .
                            WHEN "stat"      THEN 
                                cVarValue = STRING(cust.state,"x(5)") .
                            WHEN "zip"       THEN 
                                cVarValue = STRING(cust.zip,"x(10)")  .
                            WHEN "cre-lim"   THEN 
                                cVarValue = STRING(cust.cr-lim,">>>>>>>>9.99") .
                            WHEN "phone"     THEN 
                                cVarValue = TRIM(STRING(cust.area-code,"(xxx)") + string(cust.phone,"xxx-xxxx")) .
                            WHEN "fax"       THEN 
                                cVarValue = TRIM(STRING(substr(cust.fax,1,3),"(xxx)") +  string(substr(cust.fax,4,7),"xxx-xxxx")).
                            WHEN "chk-memo"  THEN 
                                cVarValue = STRING(tt-ar-cashl.check-no).
                            WHEN "day-old"   THEN 
                                cVarValue = STRING("").
                            WHEN "type"      THEN 
                                cVarValue = STRING(v-type,"x(4)").
                            WHEN "inv"       THEN 
                                cVarValue = STRING(v-neg-text) .
                            WHEN "inv-date"  THEN 
                                cVarValue = STRING(v-check-date,"99/99/99") .
                            WHEN "amount"    THEN 
                                cVarValue = STRING(v-cr-db-amt + v-disc-amt,"->>>>>>>>9.99").
                            WHEN "current"   THEN 
                                cVarValue = /*STRING(unapp[1],"->>>>>>>>9.99")*/ "".
                            WHEN "adtp"      THEN 
                                cVarValue = STRING(cust.avg-pay,">>>9").
                            WHEN "td"        THEN 
                                cVarValue = STRING(v-current-trend-days,"->>9").
                            WHEN "per-1"     THEN 
                                cVarValue = /*STRING(unapp[2],"->>>>>>>>9.99")*/ "" .
                            WHEN "per-2"     THEN 
                                cVarValue = /*STRING(unapp[3],"->>>>>>>>9.99")*/ "" .
                            WHEN "per-3"     THEN 
                                cVarValue = /*STRING(unapp[4],"->>>>>>>>9.99")*/ "" .
                            WHEN "cust-po"   THEN 
                                cVarValue = STRING(cPoNo,"x(15)") .
                            WHEN "job"       THEN 
                                cVarValue = STRING(cJobStr,"x(10)")  .
                            WHEN "inv-note"  THEN 
                                cVarValue = "".
                            WHEN "coll-note" THEN 
                                cVarValue = "".
                    
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
                END. /* det-prt  */
            END.
        END. /* for each tt-ar-cashl record */

        c1 = cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4].

        IF (NOT v-first-cust) OR c1 NE 0 THEN 
        DO:
            IF det-rpt = 1 THEN 
            DO:
                /* Display statement removed here */
                RUN total-head("****** CUSTOMER TOTALS",c1,cust-t[1],cust-t[2],
                    cust-t[3],cust-t[4]).
        
                IF v-sep-fc THEN
                DO:
                    ASSIGN
                        c1-pri = cust-t-pri[1] + cust-t-pri[2] + cust-t-pri[3] + cust-t-pri[4]
                        c1-fc  = cust-t-fc[1] + cust-t-fc[2] + cust-t-fc[3] + cust-t-fc[4].

                    /*display skip(1) "***** PRINCIPAL AMOUNT" at 4 c1-pri to 54 cust-t-pri[1] to 77
                         cust-t-pri[2] to 94 cust-t-pri[3] to 112 cust-t-pri[4] to 131 skip(1)
                     with frame a4 no-labels no-box no-attr-space stream-io width 200.*/

                    RUN total-head("***** PRINCIPAL AMOUNT",c1-pri,cust-t-pri[1],cust-t-pri[2],
                        cust-t-pri[3],cust-t-pri[4]).

                    /*display skip(1) "***** FINANCE CHARGES" at 4 c1-fc to 54 cust-t-fc[1] to 77
                         cust-t-fc[2] to 94 cust-t-fc[3] to 112 cust-t-fc[4] to 131 skip(1)
                     with frame a5 no-labels no-box no-attr-space stream-io width 200.*/
                    RUN total-head("****** FINANCE CHARGES",c1-fc,cust-t-fc[1],cust-t-fc[2],
                        cust-t-fc[3],cust-t-fc[4]).
                END.

                IF NOT LAST-OF(tt-cust.sorter) OR ipcSort1 NE "cust.sman" THEN
                    PUT SKIP(1).
            END.
            ELSE IF det-rpt = 2 THEN 
                DO:

                    RUN total-head("cust.cust-no",c1,cust-t[1],cust-t[2],
                        cust-t[3],cust-t[4]).

                    /* Display statement removed here */
                    IF v-export THEN 
                    DO:
                    /* Display statement removed here */
                    END.
                END.
            
            DO i = 1 TO 4:
                ASSIGN
                    sman-t[i] = sman-t[i] + cust-t[i]
                    cust-t[i] = 0.

                IF v-sep-fc THEN
                    ASSIGN
                        sman-t-pri[i] = sman-t-pri[i] + cust-t-pri[i]
                        sman-t-fc[i]  = sman-t-fc[i] + cust-t-fc[i]
                        cust-t-pri[i] = 0
                        cust-t-fc[i]  = 0.
            END.
        END.
    
        IF LAST-OF(tt-cust.sorter) THEN 
        DO:
            c1 = sman-t[1] + sman-t[2] + sman-t[3] + sman-t[4].
          
            IF ipcSort1 EQ "cust.sman" THEN 
            DO:
                IF det-rpt <> 3 THEN
                    RUN total-head("****** SALESREP TOTALS",c1,sman-t[1],sman-t[2],
                        sman-t[3],sman-t[4]).
                /* Display statement removed here */
                IF v-export THEN 
                DO:
                    IF det-rpt = 2 /* was if det-rpt was if not dep-rpt */THEN 
                    DO:
                    /* Display statement removed here */
                    END.
                END.
            END.

            DO i = 1 TO 4:
                ASSIGN
                    curr-t[i]     = curr-t[i] + sman-t[i]
                    sman-t[i]     = 0
                    curr-t-pri[i] = curr-t-pri[i] + sman-t-pri[i]
                    curr-t-fc[i]  = curr-t-fc[i] + sman-t-fc[i]
                    sman-t-pri[i] = 0
                    sman-t-fc[i]  = 0.
            END.
        END.
    
        IF LAST-OF(tt-cust.curr-code) THEN 
        DO:
            IF ll-mult-curr THEN 
            DO:
                c1 = curr-t[1] + curr-t[2] + curr-t[3] + curr-t[4].
                IF NOT det-rpt = 3 THEN
                    RUN total-head("        CURRENCY TOTAL",c1,curr-t[1],curr-t[2],
                        curr-t[3],curr-t[4]).


                /* Display statement removed here */

                RUN total-head("PERCENTAGE COMPOSITION",0,(IF c1 NE 0 THEN (curr-t[1] / c1) * 100 ELSE 0),(IF c1 NE 0 THEN (curr-t[2] / c1) * 100 ELSE 0),
                    (IF c1 NE 0 THEN (curr-t[3] / c1) * 100 ELSE 0),(IF c1 NE 0 THEN (curr-t[4] / c1) * 100 ELSE 0)).

                IF v-export THEN 
                DO:
                    IF NOT det-rpt = 1 THEN 
                    DO:
                    /* Display statement removed here */
                    END.
                END.
            END.

            DO i = 1 TO 4:
                ASSIGN
                    grand-t[i] = grand-t[i] + curr-t[i]
                    curr-t[i]  = 0.

                IF v-sep-fc THEN
                    ASSIGN
                        grand-t-pri[i] = grand-t-pri[i] + curr-t-pri[i]
                        grand-t-fc[i]  = grand-t-fc[i] + curr-t-fc[i]
                        curr-t-pri[i]  = 0
                        curr-t-fc[i]   = 0.
            END.
        END.
    
        m3 = "".
        IF ni EQ 1 THEN m3 = m2.
        ASSIGN
            v-cr-db-amt = 0
            v-disc-amt  = 0.
    END.  /* for each cust record */

    IF ll-mult-curr THEN 
    DO:
        HIDE FRAME r-top-1 NO-PAUSE.
        HIDE FRAME r-top-2 NO-PAUSE.
        PAGE.
    END.

    t1 = grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4].

    /* Display statement removed here */
    
    RUN total-head("           GRAND TOTAL",t1,grand-t[1],grand-t[2],
        grand-t[3],grand-t[4]).

    RUN total-head("PERCENTAGE COMPOSITION",0,(IF t1 NE 0 THEN (grand-t[1] / t1) * 100 ELSE 0),(IF t1 NE 0 THEN (grand-t[2] / t1) * 100 ELSE 0),
        (IF t1 NE 0 THEN (grand-t[3] / t1) * 100 ELSE 0),(IF t1 NE 0 THEN (grand-t[4] / t1) * 100 ELSE 0)).

    /*display SPACE(11) "PERCENTAGE COMPOSITION"
      (IF t1 NE 0 THEN (grand-t[1] / t1) * 100 ELSE 0) to 77 format "->,>>>,>>>,>>9.99"
      (IF t1 NE 0 THEN (grand-t[2] / t1) * 100 ELSE 0) to 94 format "->,>>>,>>>,>>9.99"
      (IF t1 NE 0 THEN (grand-t[3] / t1) * 100 ELSE 0) to 112 format "->,>>>,>>>,>>9.99"
      (IF t1 NE 0 THEN (grand-t[4] / t1) * 100 ELSE 0) to 131 format "->,>>>,>>>,>>9.99"
      with frame grand2 STREAM-IO WIDTH 200 no-labels no-box no-attr-space.*/

    IF v-export THEN 
    DO:
        IF NOT det-rpt = 1 THEN 
        DO:
            IF NOT v-prt-add THEN 
            DO:
            /* Display statement removed here */
            END.
            ELSE 
            DO:
            /* Display statement removed here */
            END.
        END.
    END.

    IF v-sep-fc THEN
    DO:
        ASSIGN
            t1-pri = grand-t-pri[1] + grand-t-pri[2] + grand-t-pri[3] + grand-t-pri[4]
            t1-fc  = grand-t-fc[1] + grand-t-fc[2] + grand-t-fc[3] + grand-t-fc[4].

        RUN total-head("      PRINCIPAL AMOUNT",t1-pri,grand-t-pri[1],grand-t-pri[2],
            grand-t-pri[3],grand-t-pri[4]).



        RUN total-head("       FINANCE CHARGES",t1-fc,grand-t-fc[1],grand-t-fc[2],
            grand-t-fc[3],grand-t-fc[4]).


        IF v-export THEN 
        DO:
            IF NOT det-rpt = 1 THEN 
            DO:
                IF NOT v-prt-add THEN 
                DO:
                /*EXPORT STREAM s-temp DELIMITER ","
                   " " 
                   "PRINCIPAL AMOUNT" 
                   " "
                   t1-pri                                      
                   grand-t-pri[1]                                           
                   grand-t-pri[2]
                   grand-t-pri[3]
                   grand-t-pri[4]
                   SKIP.
                EXPORT STREAM s-temp DELIMITER ","
                   " " 
                   "FINANCE CHARGES" 
                   " "
                   t1-fc                                      
                   grand-t-fc[1]                                           
                   grand-t-fc[2]
                   grand-t-fc[3]
                   grand-t-fc[4]
                   SKIP.*/
                END.
                ELSE 
                DO:
                /* Display statement removed here */
                END.
            END.
        END.
    END.


    STATUS DEFAULT "".

    RETURN.
/*-----------------------------------------------------------------------------*/
PROCEDURE print-cust-add:
    IF det-rpt <> 3 THEN
        DISPLAY cust.addr[1]                                                SKIP
            cust.addr[2]                                                SKIP
            TRIM(cust.city) + ", " +
            trim(cust.state) + "  " + trim(cust.zip) FORMAT "x(50)"
            
            WITH NO-LABELS NO-BOX FRAME cust-detail STREAM-IO WIDTH 200.
END.
/*-----------------------------------------------------------------------------*/
PROCEDURE export-data:
    DEF INPUT PARAMETER v-field-01 LIKE tt-ar-cashl.check-no NO-UNDO.
    DEF INPUT PARAMETER v-field-02 LIKE d                 NO-UNDO.
    DEF INPUT PARAMETER v-field-03 LIKE v-type            NO-UNDO.
    DEF INPUT PARAMETER v-field-04 AS   CHAR              NO-UNDO.
    DEF INPUT PARAMETER v-field-05 LIKE tt-inv.inv-date   NO-UNDO.
    DEF INPUT PARAMETER v-field-06 LIKE amt               NO-UNDO.
    DEF INPUT PARAMETER v-field-07 LIKE ag                NO-UNDO.
    DEF INPUT PARAMETER v-field-08 LIKE ag                NO-UNDO.
    DEF INPUT PARAMETER v-field-09 LIKE ag                NO-UNDO.
    DEF INPUT PARAMETER v-field-10 LIKE ag                NO-UNDO.
    DEF VAR v-delimiter AS cha NO-UNDO.       /* 9: tab 44: comma*/
    v-delimiter = "~t" /*CHR(9)*/ .
    /* Display statement removed here */
    IF det-rpt = 1 THEN 
    DO:

        EXPORT STREAM s-temp DELIMITER ","
            TRIM(cust.cust-no)                                     
            TRIM(cust.name)                                         
            TRIM(cust.contact)                                      
            TRIM(v-sman)                                            
            TRIM(IF AVAIL terms THEN terms.dscr ELSE "")            
            TRIM(cust.addr[1])                                      
            TRIM(cust.addr[2])                                      
            TRIM(cust.city)                                         
            TRIM(cust.state)                                        
            TRIM(cust.zip)                                          
            TRIM(STRING(cust.cr-lim,">>>>>>>>9.99"))                
            TRIM(STRING(cust.area-code,"(xxx)") + " " +
            string(cust.phone,"xxx-xxxx"))                     
            TRIM(STRING(substr(cust.fax,1,3),"(xxx)") + " " +
            string(substr(cust.fax,4,7),"xxx-xxxx"))           
            TRIM(v-field-01)                                        
            TRIM(STRING(v-field-02,"->>>>"))                        
            TRIM(v-field-03)                                        
            TRIM(v-field-04)                                        
            TRIM(STRING(v-field-05,"99/99/9999"))                   
            TRIM(STRING(v-field-06,"->>>>>>>>9.99"))                
            TRIM(STRING(v-field-07,"->>>>>>>>9.99"))  
            TRIM(STRING(cust.avg-pay,">>9"))                /*Task# 11151304*/
            TRIM(STRING(v-current-trend-days,"->>9"))       /*Task# 11151304*/ 
            TRIM(STRING(v-field-08,"->>>>>>>>9.99"))                
            TRIM(STRING(v-field-09,"->>>>>>>>9.99"))                
            TRIM(STRING(v-field-10,"->>>>>>>>9.99"))
            TRIM(STRING(IF cPoNo NE "" AND v-print-cust-po THEN cPoNo ELSE "")).    /*Task# 02071402*/

        ASSIGN 
            cPoNo = "" . 
    END.
END.
  
/*-----------------------------------------------------------------------------*/
PROCEDURE Display-InvNote:
    DEF VAR li AS INT NO-UNDO.

    ASSIGN 
        lv-text           = ""
        v-Inv-note        = ""
        v-Collection-note = "".

    FOR EACH tt-formtext:
        DELETE tt-formtext.
    END.
    
       
    FOR EACH notes NO-LOCK WHERE notes.rec_key = tt-inv.rec_key
        AND notes.note_type = "I" : 
                                
        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
    END.

    DO li = 1 TO 8:
        CREATE tt-formtext.
        ASSIGN 
            tt-line-no = li
            tt-length  = 80.
    END.
    RUN custom/formtext.p (lv-text).
    i = 0.           
    FOR EACH tt-formtext:
        i = i + 1.
        IF  i <= 8 THEN v-inv-note[i] = tt-formtext.tt-text.      
    END.
    

    IF v-Inv-Note[1] <> "" THEN 
    DO:
        PUT SKIP(1) "Invoice Note: " v-Inv-Note[1] SKIP.
        DO i = 2 TO 5:
            IF v-Inv-Note[i] > "" THEN 
                PUT v-Inv-Note[i] SKIP.
        END.
        PUT SKIP(1) .
    END.
END.
/*-----------------------------------------------------------------------------*/
PROCEDURE Display-CollectionNote:
    DEF VAR li AS INT NO-UNDO.

    ASSIGN 
        lv-text           = ""
        v-Inv-note        = ""
        v-Collection-note = "".

    FOR EACH tt-formtext:
        DELETE tt-formtext.
    END.
    
    FOR EACH notes NO-LOCK WHERE notes.rec_key = cust.rec_key
        AND notes.note_type = "G"
        AND notes.note_group = "Collection" :
        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
    END.

    DO li = 1 TO 8:
        CREATE tt-formtext.
        ASSIGN 
            tt-line-no = li
            tt-length  = 80.
    END.
    RUN custom/formtext.p (lv-text).
    i = 0.           
    FOR EACH tt-formtext:
        i = i + 1.
        IF  i <= 8 THEN v-Collection-note[i] = tt-formtext.tt-text.      
    END.

    IF v-Collection-Note[1] <> "" THEN 
    DO:
        PUT SKIP(1) "Collection Note: " v-Collection-Note[1] SKIP.
        DO i = 2 TO 5:
            IF v-Collection-Note[i] > "" THEN
                PUT v-Collection-Note[i] SKIP.
        END.
        PUT SKIP(1) .
    END.

END PROCEDURE.
/*-----------------------------------------------------------------------------*/
PROCEDURE get-trend-days:
    DEFINE INPUT PARAMETER ip-trend-days AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER op-trend-days  AS INT NO-UNDO INIT 0.

    DEFINE BUFFER buf-ar-inv FOR ar-inv.
   
    DEF VAR v-days     AS INT NO-UNDO.
    DEF VAR v-invs     AS INT NO-UNDO.
    DEF VAR v-avg-days LIKE cust.avg-pay NO-UNDO INIT 0.

    /* If zero trend days, then abort calculation. */
    IF ip-trend-days = 0 THEN RETURN.

    ASSIGN
        v-days = 0
        v-invs = 0.
    FIND CURRENT cust.
    FOR EACH buf-ar-inv
        WHERE buf-ar-inv.company  EQ cust.company
        AND buf-ar-inv.posted   EQ YES
        AND buf-ar-inv.cust-no  EQ cust.cust-no
        AND buf-ar-inv.due      LE 0
        AND buf-ar-inv.pay-date GE (TODAY - ip-trend-days)
        USE-INDEX posted-due NO-LOCK:

        ASSIGN 
            v-days = v-days + (buf-ar-inv.pay-date - buf-ar-inv.inv-date)
            v-invs = v-invs + 1.

    END. /*  FOR each buf-ar-inv */
    
    ASSIGN 
        v-avg-days = v-days / v-invs. 
  
    IF v-avg-days LT 1 OR v-avg-days EQ ? THEN v-avg-days = 1.
    ASSIGN 
        op-trend-days = (cust.avg-pay - v-avg-days).


    RETURN.

END PROCEDURE.

PROCEDURE total-head:
    DEF INPUT PARAMETER vname AS CHAR .
    DEF INPUT PARAMETER amount AS DECIMAL.
    DEF INPUT PARAMETER vCURRENT AS DECIMAL.
    DEF INPUT PARAMETER per-day1 AS DECIMAL.
    DEF INPUT PARAMETER per-day2 AS DECIMAL.
    DEF INPUT PARAMETER per-day3 AS DECIMAL.

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
                cVarValue = ""  .
            WHEN "cust-name" THEN 
                cVarValue = ""  .
            WHEN "cont"      THEN 
                cVarValue = "" .
            WHEN "sman"      THEN 
                cVarValue = "" .
            WHEN "term"      THEN 
                cVarValue = ""  .
            WHEN "add1"      THEN 
                cVarValue = "".
            WHEN "add2"      THEN 
                cVarValue = "".
            WHEN "city"      THEN 
                cVarValue = "" .
            WHEN "stat"      THEN 
                cVarValue = "" .
            WHEN "zip"       THEN 
                cVarValue = ""  .
            WHEN "cre-lim"   THEN 
                cVarValue = "" .
            WHEN "phone"     THEN 
                cVarValue = "" .
            WHEN "fax"       THEN 
                cVarValue = "".
            WHEN "chk-memo"  THEN 
                cVarValue = "".
            WHEN "day-old"   THEN 
                cVarValue = "".
            WHEN "type"      THEN 
                cVarValue = "".
            WHEN "inv"       THEN 
                cVarValue = "" .
            WHEN "inv-date"  THEN 
                cVarValue = "" .
            WHEN "amount"    THEN 
                cVarValue = STRING(amount + v-disc-amt,"->>>>>>>>9.99").
            WHEN "current"   THEN 
                cVarValue = STRING(vCURRENT,"->>>>>>>>9.99").
            WHEN "adtp"      THEN 
                cVarValue = "".
            WHEN "td"        THEN 
                cVarValue = "".
            WHEN "per-1"     THEN 
                cVarValue = STRING(per-day1,"->>>>>>>>9.99") .
            WHEN "per-2"     THEN 
                cVarValue = STRING(per-day2,"->>>>>>>>9.99") .
            WHEN "per-3"     THEN 
                cVarValue = STRING(per-day3,"->>>>>>>>9.99")  .
            WHEN "cust-po"   THEN 
                cVarValue = "" .
            WHEN "job"       THEN 
                cVarValue = ""  .
            WHEN "inv-note"  THEN 
                cVarValue = "".
            WHEN "coll-note" THEN 
                cVarValue = "".
                    
        END CASE.
                  
        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
        

    IF vname = "cust.cust-no" THEN 
    DO:
        PUT UNFORMATTED   cust.cust-no FORMAT "x(8)" SPACE(1)  cust.name  FORMAT "x(25)"   SUBSTRING(cDisplay,35,400) SKIP.
        IF v-export THEN 
        DO:
            PUT STREAM s-temp UNFORMATTED   
                cust.cust-no FORMAT "x(8)" SPACE(1)  cust.name  FORMAT "x(25)" ','  SUBSTRING(cExcelDisplay,4,400) SKIP(1).
        END.
    END.
    ELSE 
    DO:
        PUT SKIP(1) str-line SKIP . 
        PUT UNFORMATTED  
            "          " vname  SUBSTRING(cDisplay,33,400) SKIP.
        IF v-export THEN 
        DO:
            PUT STREAM s-temp UNFORMATTED  
                '                       ' vname  ','  SUBSTRING(cExcelDisplay,4,400) SKIP(1).
        END.
    END.
end procedure.
