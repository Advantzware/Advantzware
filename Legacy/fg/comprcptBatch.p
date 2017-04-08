
/*------------------------------------------------------------------------
    File        : comprcptBatch.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Jan 19 10:43:31 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER  ipcTag AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  ipcLoc AS CHARACTER NO-UNDO.
/* For testing */
OUTPUT TO c:\temp\cmprcptbatch-err.txt.

/* Define globals here until strategy is available to pull into web app */
DEFINE NEW SHARED VARIABLE g_lookup-var  AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_track_usage AS LOGICAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE g_header_line AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_groups      AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE init_menu     AS LOGICAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE g_developer   AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_version     AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_rec_key     AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_pageno      AS INTEGER       NO-UNDO.
DEFINE NEW SHARED VARIABLE g_mainmenu    AS WIDGET-HANDLE NO-UNDO.

g_lookup-var = "".

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i &new=NEW}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i NEW SHARED}
{custom/globdefs.i &NEW=NEW}

DEFINE VARIABLE lQtyChanged    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ipcLinker      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-auto-add-tag AS LOG       NO-UNDO.
DEFINE VARIABLE v-next-tag     AS cha       NO-UNDO.
{sys/inc/fgsetrec.i}
{sys/inc/rfidtag.i}

{fg/fullset.i NEW}
FIND FIRST sys-ctrl NO-LOCK 
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "FGPOTAG#"
    NO-ERROR.
v-auto-add-tag = NO.
IF AVAILABLE sys-ctrl THEN
    v-auto-add-tag = sys-ctrl.log-fld.
DEF TEMP-TABLE tt-bin 
    FIELD tt-bin-row AS ROWID .

DEF TEMP-TABLE tt-del-fg-rctd
    FIELD fg-rctd-row AS ROWID.

DEF TEMP-TABLE tt-del-fg-rcpts
    FIELD fg-rcpts-row AS ROWID.
  
/* ********************  Preprocessor Definitions  ******************** */
/* ***************************  Main Block  *************************** */

DO: 
    ASSIGN 
        cocode    = ipcCompany
        locode    = ipcLoc    
        g_company = ipcCompany
        gcompany  = ipcCompany
        g_loc     = ipcLoc
        gloc      = ipcLoc.
              
    FIND FIRST fg-rctd EXCLUSIVE-LOCK WHERE 
      fg-rctd.company EQ ipcCompany
        AND fg-rctd.tag EQ ipcTag 
        AND fg-rctd.rita-code EQ 'R' NO-ERROR.
     IF NOT AVAILABLE fg-rctd THEN
        RETURN. 
      
    FIND FIRST itemfg WHERE itemfg.company EQ fg-rctd.company
        AND itemfg.i-no EQ fg-rctd.i-no 
        NO-LOCK NO-ERROR.
        
        
    IF lQtyChanged AND CAN-FIND(FIRST fg-rcpts
        WHERE fg-rcpts.company EQ cocode
        AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")) 
        AND NOT (FGSetRec-int EQ 1 AND itemfg.alloc NE YES) THEN 
    DO:
        MESSAGE "Set Parts Receipts will be reset since the set header quantity was changed. Please review the Set Parts tab."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RUN DeleteSetParts (INPUT ("fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"))).
    END.
    
    
    IF NOT (FGSetRec-Int EQ 1 AND itemfg.alloc NE YES) THEN
        RUN comprcpt (ROWID(fg-rctd)).
        
        
END.
      
  
IF v-auto-add-tag AND fg-rctd.tag EQ "" THEN 
DO:
    
    
    RUN get-next-tag (INPUT fg-rctd.i-no, OUTPUT v-next-tag).
    RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(fg-rctd)).
    fg-rctd.tag = v-next-tag.
    
    
END.
FIND CURRENT fg-rctd NO-LOCK NO-ERROR.
FIND CURRENT itemfg NO-LOCK NO-ERROR.

/* **********************  Internal Procedures  *********************** */

PROCEDURE bin-qty-used:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    DEF INPUT PARAMETER iprBinRow AS ROWID NO-UNDO.
    DEF OUTPUT PARAMETER opdQty AS DECIMAL NO-UNDO.
    DEF BUFFER bf-fg-bin  FOR fg-bin.
    DEF BUFFER bf-fg-rctd FOR fg-rctd.
    DEF VAR ldQty AS DECIMAL NO-UNDO.
  
    FIND FIRST bf-fg-bin NO-LOCK WHERE ROWID(bf-fg-bin) EQ iprBinRow
        NO-ERROR.
    IF NOT AVAIL bf-fg-bin THEN
        RETURN.

    /* Subtract from bin qty the quantity of previous tags used */
    ldQty = bf-fg-bin.qty.
    FOR EACH bf-fg-rctd NO-LOCK
        WHERE bf-fg-rctd.i-no EQ bf-fg-bin.i-no
        AND bf-fg-rctd.job-no EQ bf-fg-bin.job-no
        AND bf-fg-rctd.job-no2 EQ bf-fg-bin.job-no2
        AND bf-fg-rctd.loc     EQ bf-fg-bin.loc
        AND bf-fg-rctd.loc-bin EQ bf-fg-bin.loc-bin
        AND bf-fg-rctd.tag     EQ bf-fg-bin.tag
        AND bf-fg-rctd.rita-code EQ fg-rctd.rita-code
        AND bf-fg-rctd.t-qty LT 0
        :
    
        /* These component records will be negative so add them to reduce total */
        ldQty = ldQty + bf-fg-rctd.t-qty.
    END.
  
    opdQty = ldQty.


END PROCEDURE.

PROCEDURE comprcpt:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.



    DEF BUFFER b-fg-rctd FOR fg-rctd.
    DEF BUFFER b-itemfg  FOR itemfg.

    DEF    VAR      li             AS INT       NO-UNDO.
    DEF    VAR      li-factor      AS INT       NO-UNDO.

    DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFGSetAssembly AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFGSetAssembly AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lGetBin        AS LOGICAL   NO-UNDO.


    DO TRANSACTION:
        {sys/inc/autopost.i}
  {sys/inc/fgsetrec.i}
        {sys/inc/tspostfg.i}
        {sys/inc/fgrecpt.i}
    END.
    RUN sys/ref/nk1look.p (INPUT cocode,
        INPUT "FGSetAssembly",
        INPUT "L",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFGSetAssembly,
        OUTPUT lFound).
    IF lFound THEN
        lFGSetAssembly = cFGSetAssembly EQ "YES".
    RUN sys/ref/nk1look.p (INPUT cocode,
        INPUT "FGSetAssembly",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFGSetAssembly,
        OUTPUT lFound).

    FIND fg-rctd NO-LOCK WHERE ROWID(fg-rctd) EQ ip-rowid NO-ERROR.

    IF AVAIL fg-rctd THEN
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no
            NO-ERROR.

    IF AVAIL itemfg                     AND
        itemfg.isaset                    AND
        NOT tspostfg-log                 AND
        itemfg.alloc NE NO               AND
        (itemfg.alloc EQ ?          OR
        fgrecpt-char NE "AUTOPOST" OR
        TRIM(fg-rctd.job-no) EQ "")     THEN 
    DO:

        RUN fg/fullset.p (ROWID(itemfg)).
   
        FOR EACH tt-fg-set,
  
            FIRST b-itemfg
            WHERE b-itemfg.company EQ cocode
            AND b-itemfg.i-no    EQ tt-fg-set.part-no
            AND ROWID(b-itemfg)  NE ROWID(itemfg)
            AND NOT CAN-FIND(FIRST fg-rcpts
            WHERE fg-rcpts.company EQ cocode
            AND fg-rcpts.i-no    EQ b-itemfg.i-no
            AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"))
            NO-LOCK:
  

            IF fg-rctd.job-no NE "" THEN
            DO:
                FIND FIRST job NO-LOCK WHERE
                    job.company EQ cocode AND
                    job.job-no EQ fg-rctd.job-no AND
                    job.job-no2 EQ fg-rctd.job-no2
                    NO-ERROR.

            END.
            ELSE
                .
       
            li = 0.
            FIND LAST b-fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
            IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT li THEN li = b-fg-rctd.r-no.
       
            FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.
            IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
       
            CREATE fg-rcpts.
            ASSIGN
                fg-rcpts.r-no       = li + 1
                fg-rcpts.company    = cocode
                fg-rcpts.i-no       = b-itemfg.i-no
                fg-rcpts.i-name     = b-itemfg.i-name
                fg-rcpts.trans-date = fg-rctd.rct-date
                fg-rcpts.linker     = "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999").
       
            RELEASE fg-rcpts.
       
            CREATE b-fg-rctd.
            BUFFER-COPY fg-rctd EXCEPT rec_key TO b-fg-rctd
                ASSIGN
                b-fg-rctd.r-no       = li + 1
                b-fg-rctd.i-no       = b-itemfg.i-no
                b-fg-rctd.i-name     = b-itemfg.i-name
                b-fg-rctd.qty-case   = fg-rctd.qty-case * tt-fg-set.part-qty-dec
                b-fg-rctd.partial    = fg-rctd.partial  * tt-fg-set.part-qty-dec
                b-fg-rctd.t-qty      = fg-rctd.t-qty    * tt-fg-set.part-qty-dec
                b-fg-rctd.std-cost   = 0.
       
            IF b-fg-rctd.job-no NE "" THEN 
            DO:
                FIND FIRST job NO-LOCK 
                    WHERE job.company EQ cocode
                    AND job.job-no  EQ b-fg-rctd.job-no
                    AND job.job-no2 EQ b-fg-rctd.job-no2
                    NO-ERROR.
                IF AVAIL job THEN  
                    FIND FIRST reftable NO-LOCK
                        WHERE reftable.reftable EQ "jc/jc-calc.p"
                        AND reftable.company  EQ job.company
                        AND reftable.loc      EQ ""
                        AND reftable.code     EQ STRING(job.job,"999999999")
                        AND reftable.code2    EQ b-fg-rctd.i-no
                        NO-ERROR.
       
                IF AVAIL reftable THEN b-fg-rctd.std-cost = reftable.val[5].
            END.
       
            b-fg-rctd.ext-cost = b-fg-rctd.std-cost * b-fg-rctd.t-qty / 1000.
            /* Per Task # 11111303, tag should not be assigned to component */
            /* records regardless of fgsetrec                               */
            b-fg-rctd.tag      = "". 
            /*##BL - IF FGSETAssembly is on, use the char value as the bin*/
            /*##BL - If that bin doesn't exist, continue to find bin as usual*/
            lGetBin = YES.
     
            IF lFGSetAssembly THEN 
            DO:
                b-fg-rctd.loc-bin = cFGSetAssembly.
                FIND FIRST fg-bin EXCLUSIVE-LOCK 
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ b-fg-rctd.i-no
                    AND fg-bin.loc     EQ b-fg-rctd.loc
                    AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
                    AND fg-bin.tag     EQ b-fg-rctd.tag
                    AND fg-bin.job-no  EQ b-fg-rctd.job-no
                    AND fg-bin.job-no2 EQ b-fg-rctd.job-no2
                    NO-ERROR.

                /* Try again without tag */
                IF NOT AVAIL fg-bin THEN
                    FIND FIRST fg-bin EXCLUSIVE-LOCK
                        WHERE fg-bin.company EQ cocode
                        AND fg-bin.i-no    EQ b-fg-rctd.i-no
                        AND fg-bin.loc     EQ b-fg-rctd.loc
                        AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
                        AND fg-bin.job-no  EQ b-fg-rctd.job-no
                        AND fg-bin.job-no2 EQ b-fg-rctd.job-no2
                        NO-ERROR.

                /* Try again without job also */
                IF NOT AVAIL fg-bin THEN
                    FIND FIRST fg-bin EXCLUSIVE-LOCK
                        WHERE fg-bin.company EQ cocode
                        AND fg-bin.i-no    EQ b-fg-rctd.i-no
                        AND fg-bin.loc     EQ b-fg-rctd.loc
                        AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
                        NO-ERROR.
            
                IF AVAIL fg-bin THEN
                    lGetBin = NO.
           
            END.
     
            IF fgsetrec EQ "Item" AND lGetBin THEN 
            DO:
                ASSIGN
                    b-fg-rctd.loc     = ""
                    b-fg-rctd.loc-bin = "".
       
             
                IF autopost EQ "ShipTo" THEN 
                DO: /*get customer file from estimate blank file*/
                    FIND FIRST cust NO-LOCK 
                        WHERE cust.company EQ cocode
                        AND cust.cust-no EQ b-itemfg.cust-no
                        NO-ERROR.
                    IF AVAIL cust THEN 
                    DO: 
                        FIND FIRST shipto NO-LOCK
                            WHERE shipto.company EQ cocode
                            AND shipto.cust-no EQ cust.cust-no
                            NO-ERROR.
                        IF AVAIL shipto THEN 
                        DO:
                            FIND FIRST fg-bin NO-LOCK 
                                WHERE fg-bin.company EQ cocode
                                AND fg-bin.loc     EQ shipto.loc
                                AND fg-bin.loc-bin EQ shipto.loc-bin
                                AND fg-bin.i-no    EQ ""
                                NO-ERROR.
                            IF AVAIL fg-bin THEN 
                                ASSIGN
                                    b-fg-rctd.loc     = shipto.loc
                                    b-fg-rctd.loc-bin = shipto.loc-bin.
                        END.
                    END. /*if avail cust*/
                                
                    IF b-fg-rctd.loc EQ "" AND b-fg-rctd.loc-bin EQ "" THEN 
                    DO:
                        FIND FIRST fg-bin NO-LOCK 
                            WHERE fg-bin.company EQ cocode
                            AND fg-bin.loc     EQ b-itemfg.def-loc
                            AND fg-bin.loc-bin EQ b-itemfg.def-loc-bin
                            AND fg-bin.i-no    EQ ""
                            NO-ERROR.
                        IF AVAIL fg-bin THEN 
                            ASSIGN
                                b-fg-rctd.loc     = b-itemfg.def-loc
                                b-fg-rctd.loc-bin = b-itemfg.def-loc-bin.
                    END.
                END. /*if system default is shipto*/
              
                ELSE 
                DO:
                    FIND FIRST fg-bin NO-LOCK 
                        WHERE fg-bin.company EQ cocode
                        AND fg-bin.loc     EQ b-itemfg.def-loc
                        AND fg-bin.loc-bin EQ b-itemfg.def-loc-bin
                        AND fg-bin.i-no    EQ ""
                      NO-ERROR.
                    IF AVAIL fg-bin THEN 
                        ASSIGN
                            b-fg-rctd.loc     = b-itemfg.def-loc
                            b-fg-rctd.loc-bin = b-itemfg.def-loc-bin.
                END. /*else FGFILE*/
         
                /*if bin and warehouse are blank, goto cust "X" shipto file*/
                IF b-fg-rctd.loc EQ "" AND b-fg-rctd.loc-bin EQ "" THEN 
                DO:
                    FIND FIRST cust NO-LOCK 
                        WHERE cust.company EQ cocode
                        AND cust.active  EQ "X"
                       NO-ERROR.
                                     
                    IF AVAIL cust THEN 
                    DO:
                        FIND FIRST shipto NO-LOCK 
                            WHERE shipto.company EQ cocode
                            AND shipto.cust-no EQ cust.cust-no  
                           NO-ERROR.
                        IF AVAIL shipto THEN 
                        DO:
                            FIND FIRST fg-bin NO-LOCK 
                                WHERE fg-bin.company EQ cocode
                                AND fg-bin.loc     EQ shipto.loc
                                AND fg-bin.loc-bin EQ shipto.loc-bin
                                AND fg-bin.i-no    EQ ""
                               NO-ERROR.
                            IF AVAIL fg-bin THEN
                                ASSIGN
                                    b-fg-rctd.loc     = shipto.loc
                                    b-fg-rctd.loc-bin = shipto.loc-bin.
                        END.                                  
                    END.
                END.
            END.
       
            FIND FIRST fg-bin EXCLUSIVE-LOCK
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ b-fg-rctd.i-no
                AND fg-bin.loc     EQ b-fg-rctd.loc
                AND fg-bin.loc-bin EQ b-fg-rctd.loc-bin
                AND fg-bin.tag     EQ b-fg-rctd.tag
                AND fg-bin.job-no  EQ b-fg-rctd.job-no
                AND fg-bin.job-no2 EQ b-fg-rctd.job-no2
                NO-ERROR.
           
            ASSIGN
                b-fg-rctd.qty-case = IF AVAIL fg-bin AND fg-bin.case-count NE 0 THEN
                               fg-bin.case-count
                             ELSE
                             IF b-itemfg.case-count NE 0 THEN
                               b-itemfg.case-count
                             ELSE
                               b-fg-rctd.qty-case
                li-factor          = IF b-fg-rctd.t-qty GE 0 THEN 1 ELSE -1
        
                b-fg-rctd.t-qty    = b-fg-rctd.t-qty *
                             li-factor
                b-fg-rctd.cases    = TRUNC(b-fg-rctd.t-qty / b-fg-rctd.qty-case,0) *
                             li-factor
                b-fg-rctd.partial  = b-fg-rctd.t-qty MODULO b-fg-rctd.qty-case *
                             li-factor
                b-fg-rctd.t-qty    = b-fg-rctd.t-qty *
                             li-factor.
       
            IF AVAIL fg-bin THEN 
            DO:
                IF fg-bin.cases-unit EQ 0 OR fg-bin.cases-unit EQ ? THEN
                    fg-bin.cases-unit = 1.
       
                IF fg-bin.units-pallet EQ 0 OR fg-bin.units-pallet EQ ? THEN
                    fg-bin.units-pallet = 1.
       
                fg-bin.unit-count = fg-rctd.qty-case * fg-bin.cases-unit.
            END.
            FIND CURRENT fg-bin NO-LOCK NO-ERROR.
            RELEASE b-fg-rctd.
        END.
    END.

    IF AVAIL fg-rctd THEN RUN comprct1 (ROWID(fg-rctd)).



END PROCEDURE.

PROCEDURE comprct1:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


    DEF BUFFER b-fg-rctd   FOR fg-rctd.
    DEF BUFFER bf-fg-rctd  FOR fg-rctd.
    DEF BUFFER b-w-fg-rctd FOR fg-rctd.
    DEF BUFFER b-itemfg    FOR itemfg.
    DEF BUFFER use-job     FOR reftable.
    DEF BUFFER b-fg-rcpts  FOR fg-rcpts.

    DEF    VAR      li-max-qty     AS INT       NO-UNDO.
    DEF    VAR      v-part-qty     AS DEC       NO-UNDO.
    DEF    VAR      v-set-qty      AS DEC       NO-UNDO.
    DEF    VAR      v-cost         AS DEC       NO-UNDO.
    DEF    VAR      ldQty          AS DEC       NO-UNDO.
    DEF    VAR      li             AS INT       NO-UNDO.
    DEF    VAR      fg-uom-list    AS CHAR      NO-UNDO.

    DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFGSetAssembly AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFGSetAssembly AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lGetBin        AS LOGICAL   NO-UNDO.
    RUN sys/ref/nk1look.p (INPUT cocode,
        INPUT "FGSetAssembly",
        INPUT "L",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFGSetAssembly,
        OUTPUT lFound).
    IF lFound THEN
        lFGSetAssembly = cFGSetAssembly EQ "YES".
    RUN sys/ref/nk1look.p (INPUT cocode,
        INPUT "FGSetAssembly",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFGSetAssembly,
        OUTPUT lFound).


    RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
  
    FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAIL fg-rctd THEN
        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no
            NO-LOCK NO-ERROR.

    IF AVAIL itemfg                                    
        AND itemfg.isaset
        AND itemfg.alloc NE YES    /*any assembled set*/
        AND fg-rctd.rita-code EQ "R"                       
        AND NOT CAN-FIND(FIRST fg-rcpts
        WHERE fg-rcpts.company   EQ fg-rctd.company
        AND fg-rcpts.linker    EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
        AND fg-rcpts.rita-code EQ "set")
        THEN 
    DO:
  
        li-max-qty = fg-rctd.t-qty.
            
        RUN fg/checkset.w (?, ROWID(fg-rctd), fg-rctd.job-no, fg-rctd.job-no2,
            fg-rctd.loc, INPUT-OUTPUT li-max-qty).
  
        IF li-max-qty GE fg-rctd.t-qty THEN 
        DO:
      
            IF itemfg.alloc EQ ?  /*assembled w/part receipts*/                                                                   AND
                CAN-FIND(FIRST fg-rcpts
                WHERE fg-rcpts.company EQ fg-rctd.company
                AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")) THEN 
            DO:
    

                EMPTY TEMP-TABLE tt-del-fg-rctd.
                EMPTY TEMP-TABLE tt-del-fg-rcpts.

                FOR EACH fg-rcpts
                    WHERE fg-rcpts.company   EQ fg-rctd.company
                    AND fg-rcpts.linker    EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
                    AND fg-rcpts.rita-code NE "set"
                    NO-LOCK,
                    EACH b-fg-rctd
                    WHERE b-fg-rctd.company EQ fg-rcpts.company
                    AND b-fg-rctd.r-no    EQ fg-rcpts.r-no
                    NO-LOCK:
                    FIND FIRST b-itemfg WHERE b-itemfg.company EQ fg-rctd.company
                        AND b-itemfg.i-no EQ b-fg-rctd.i-no 
                        NO-LOCK NO-ERROR.
                    li = 0.
                    FIND LAST b-w-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
                    IF AVAIL b-w-fg-rctd AND b-w-fg-rctd.r-no GT li THEN li = b-w-fg-rctd.r-no.
    
                    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
                    IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
    
                    CREATE b-fg-rcpts.
                    BUFFER-COPY fg-rcpts EXCEPT rec_key TO b-fg-rcpts
                        ASSIGN
                        b-fg-rcpts.r-no      = li + 1
                        b-fg-rcpts.rita-code = "set".
    
                    RELEASE b-fg-rcpts.
         
                    CREATE b-w-fg-rctd.
                    BUFFER-COPY b-fg-rctd EXCEPT rec_key TO b-w-fg-rctd
                        ASSIGN
                        b-w-fg-rctd.r-no     = li + 1
                        b-w-fg-rctd.cases    = b-fg-rctd.cases * -1
                        b-w-fg-rctd.partial  = b-fg-rctd.partial * -1
                        b-w-fg-rctd.t-qty    = b-fg-rctd.t-qty * -1
                        b-w-fg-rctd.ext-cost = b-fg-rctd.ext-cost * -1.

                    /* Delete the negatives for a receipt of assembled set */
                    /* with parts since only the negative is needed        */
                    DEF VAR llDelRecs AS LOG NO-UNDO.
                    llDelRecs = FALSE.
                    IF AVAIL b-itemfg THEN 
                    DO:
                        IF fg-rctd.job-no NE "" THEN
                        DO:
                            FIND FIRST job WHERE
                                job.company EQ cocode AND
                                job.job-no EQ fg-rctd.job-no AND
                                job.job-no2 EQ fg-rctd.job-no2
                                NO-LOCK NO-ERROR.
    
                            IF AVAIL job THEN
                            DO:
                                FIND FIRST eb WHERE
                                    eb.company  EQ cocode AND
                                    eb.est-no   EQ job.est-no AND
                                    eb.stock-no EQ b-itemfg.i-no
                                    NO-LOCK NO-ERROR.
                                IF AVAIL eb THEN
                                DO:
                                    IF eb.pur-man EQ YES THEN llDelRecs = TRUE.
                                END.
                                ELSE
                                    IF b-itemfg.pur-man EQ YES THEN llDelRecs = TRUE.
                     
                            END.
                        END.
                        ELSE           
                            IF b-itemfg.pur-man EQ YES THEN llDelRecs = TRUE.
                    END.
                    IF llDelRecs THEN 
                    DO:

                        CREATE tt-del-fg-rctd.
                        ASSIGN 
                            tt-del-fg-rctd.fg-rctd-row = ROWID(b-fg-rctd).

                        CREATE tt-del-fg-rcpts.
                        ASSIGN 
                            tt-del-fg-rcpts.fg-rcpts-row = ROWID(fg-rcpts).

                    END.

                    RELEASE b-w-fg-rctd.
                END. /* for each fg-rcpts */

                /* Task 11111303 - This positive receipt of components for an assembled set */
                /* with parts is not needed, so delete them after they are used */
                /* to create the negative part of the transaction               */
                FOR EACH tt-del-fg-rctd.
                    FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ tt-del-fg-rctd.fg-rctd-row
                        EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL b-fg-rctd THEN
                        DELETE b-fg-rctd.
                    FIND fg-rcpts WHERE ROWID(fg-rcpts) EQ tt-del-fg-rcpts.fg-rcpts-row
                        EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL fg-rcpts THEN
                        DELETE fg-rcpts.
                END.
            END. /* alloc = ? */
            ELSE 
            DO:             
                RUN fg/fullset.p (ROWID(itemfg)).        

                FOR EACH tt-fg-set,

                    FIRST b-itemfg
                    WHERE b-itemfg.company EQ cocode      
                    AND b-itemfg.i-no    EQ tt-fg-set.part-no
                    AND b-itemfg.i-no    NE itemfg.i-no
                    NO-LOCK
        
                    BREAK BY b-itemfg.company:
         
                    EMPTY TEMP-TABLE tt-bin.

                    IF FIRST(b-itemfg.company) THEN fg-rctd.ext-cost = 0.
     
                    v-set-qty = fg-rctd.t-qty * tt-fg-set.part-qty-dec.
         
                    /* Detect if this was a delete and pull in the bins related to the original receipt */
                    /* Taking t-qty lt 0 to indicate that this is coming from delete receipt screen,    */
                    /* but could be delected through an input parameter if that is preferred            */
                    IF fg-rctd.t-qty LT 0 THEN 
                    DO:
             
                        FIND FIRST b-fg-rctd 
                            WHERE b-fg-rctd.company EQ fg-rctd.company
                            AND b-fg-rctd.tag     EQ fg-rctd.tag
                            AND b-fg-rctd.t-qty   GT 0
                            NO-LOCK NO-ERROR.
            
                        IF AVAIL b-fg-rctd THEN 
                        DO:

                            FOR EACH fg-rcpts
                                WHERE fg-rcpts.company   EQ b-fg-rctd.company
                                AND fg-rcpts.linker    EQ "fg-rctd: " + STRING(b-fg-rctd.r-no,"9999999999")
                                AND fg-rcpts.i-no      EQ b-itemfg.i-no
                                AND fg-rcpts.rita-code NE "set"
                                NO-LOCK,
                                EACH bf-fg-rctd
                                WHERE bf-fg-rctd.company EQ fg-rcpts.company
                                AND bf-fg-rctd.i-no    EQ b-itemfg.i-no
                                AND bf-fg-rctd.r-no    EQ fg-rcpts.r-no
                                AND bf-fg-rctd.rita-code EQ "P"
                                AND bf-fg-rctd.qty     LT 0
                                NO-LOCK:
                    
                                FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
                                    AND fg-bin.i-no EQ b-itemfg.i-no
                                    AND fg-bin.tag  EQ bf-fg-rctd.tag
                                    NO-LOCK NO-ERROR.
                                IF AVAIL fg-bin THEN 
                                DO: 
                                    CREATE tt-bin.
                                    ASSIGN 
                                        tt-bin.tt-bin-row = ROWID(fg-bin).
                        
                                END. /* avail bin */
                                ELSE 
                                DO:
                                    CREATE tt-bin.
                                    ASSIGN 
                                        tt-bin.tt-bin-row = ROWID(bf-fg-rctd).
                                END.
        
                            END. /* each fg-rcpts */

                        END. /* avail(b-fg-rctd) */

                        /* Same code as below but using specific bins, so v-set-qty may be reduced */
                        FOR EACH tt-bin,
                            FIRST fg-bin WHERE ROWID(fg-bin) EQ tt-bin.tt-bin-row
                            NO-LOCK
                            BY fg-bin.qty:
                
                            {fg/fg-post2.i b- fg-bin.qty}
                        END.
            
                        IF v-set-qty GT 0 THEN 
                        DO:
                            FOR EACH tt-bin,
                                FIRST bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ tt-bin.tt-bin-row
                                NO-LOCK 
                                BY bf-fg-rctd.qty:
                    /* 12181308 - searching for original matching transactions, per Joe, a bin existing should */
                    /*            not be a requirement, therefore, we may create the fg-post2.i records via    */
                    /*            bf-fg-rctd instead of fg-bin.                                                */
                   
                                {fg/fg-post2a.i b- }
                            END.
                        END.
                    END. /* if t-qty lt 0 */        
                    /* End code for delete of receipt */

                    IF v-set-qty GT 0 OR (v-set-qty LT 0 AND fg-rctd.t-qty LT 0) THEN 
                    DO:
            
                        FIND FIRST use-job NO-LOCK
                            WHERE use-job.reftable EQ "fg-rctd.use-job"
                            AND use-job.company  EQ STRING(fg-rctd.r-no,"9999999999")
                            NO-ERROR.
      
                        IF AVAIL use-job AND use-job.val[1] EQ 1 THEN 
                        DO:      
                            IF lFGSetAssembly THEN 
                            DO:              
                                /* Check for Assembly bin if required */
    
                                FOR EACH fg-bin
                                    WHERE fg-bin.company EQ cocode
                                    AND fg-bin.i-no    EQ b-itemfg.i-no
                                    AND fg-bin.job-no  EQ fg-rctd.job-no
                                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                                    AND fg-bin.loc     EQ fg-rctd.loc
                                    AND fg-bin.loc-bin EQ cFGSetAssembly
                                    AND fg-bin.qty     GT 0
                                    NO-LOCK BY fg-bin.qty DESC:     

                                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                                    IF NOT ldQty GT 0 THEN
                                        NEXT.

                                    {fg/fg-post2.i b- ldQty}
                                END. 
                            END.
                            IF v-set-qty GT 0 OR (v-set-qty LT 0 AND fg-rctd.t-qty LT 0) THEN 
                            DO:              
                                FOR EACH fg-bin
                                    WHERE fg-bin.company EQ cocode
                                    AND fg-bin.i-no    EQ b-itemfg.i-no
                                    AND fg-bin.job-no  EQ fg-rctd.job-no
                                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                                    AND fg-bin.qty     GT 0
                                    NO-LOCK BY fg-bin.qty DESC:           
                                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                                    IF NOT ldQty GT 0 THEN
                                        NEXT.
                                    {fg/fg-post2.i b- ldQty}
                                END. 
                            END.


                        END.
                        ELSE 
                        DO:
                            IF lFGSetAssembly THEN 
                            DO:          

                                /* Check for assembly bin if required */
                                FOR EACH fg-bin
                                    WHERE fg-bin.company EQ cocode
                                    AND fg-bin.i-no    EQ b-itemfg.i-no
                                    AND fg-bin.job-no  NE ""
                                    AND fg-bin.qty     GT 0
                                    AND fg-bin.loc     EQ fg-rctd.loc
                                    AND fg-bin.loc-bin EQ cFGSetAssembly
                                    NO-LOCK BY fg-bin.qty DESC:     
                                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                                    IF NOT ldQty GT 0 THEN
                                        NEXT.
                                    {fg/fg-post2.i b- ldQty}
                                END.  
                            END.
                
                            IF v-set-qty GT 0 THEN
                                FOR EACH fg-bin
                                    WHERE fg-bin.company EQ cocode
                                    AND fg-bin.i-no    EQ b-itemfg.i-no
                                    AND fg-bin.job-no  EQ ""
                                    AND fg-bin.qty     GT 0
                                    AND fg-bin.loc     EQ fg-rctd.loc
                                    AND fg-bin.loc-bin EQ cFGSetAssembly
                                    NO-LOCK BY fg-bin.qty DESC:                  
                                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                                    IF NOT ldQty GT 0 THEN
                                        NEXT.
                                    {fg/fg-post2.i b- ldQty}
                                END.

                        END. /* if fgsetassembly */

                        /* Normal check for non-assembly bins */
                        IF v-set-qty GT 0 THEN
                            FOR EACH fg-bin
                                WHERE fg-bin.company EQ cocode
                                AND fg-bin.i-no    EQ b-itemfg.i-no
                                AND fg-bin.job-no  NE ""
                                AND fg-bin.qty     GT 0
                                NO-LOCK BY fg-bin.qty DESC:    
                                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                                IF NOT ldQty GT 0 THEN
                                    NEXT.
                                {fg/fg-post2.i b- ldQty}
                            END.  
    
                        IF v-set-qty GT 0 THEN
                            FOR EACH fg-bin
                                WHERE fg-bin.company EQ cocode
                                AND fg-bin.i-no    EQ b-itemfg.i-no
                                AND fg-bin.job-no  EQ ""
                                AND fg-bin.qty     GT 0
                                NO-LOCK BY fg-bin.qty DESC:
                                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                                IF NOT ldQty GT 0 THEN
                                    NEXT.
                                {fg/fg-post2.i b- ldQty}
                            END.
    
                        IF v-set-qty GT 0 THEN
                            FOR EACH fg-bin
                                WHERE fg-bin.company EQ cocode
                                AND fg-bin.i-no    EQ b-itemfg.i-no
                                NO-LOCK BY fg-bin.qty DESC:     
                                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                                IF NOT ldQty GT 0 THEN
                                    NEXT.
                                {fg/fg-post2.i b- ldQty}
                            END.

                    END.
                    v-cost = fg-rctd.ext-cost / fg-rctd.t-qty.

                    IF LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 THEN 
                        fg-rctd.std-cost = v-cost.
                    ELSE
                        RUN sys/ref/convcuom.p("EA", fg-rctd.cost-uom, 0, 0, 0, 0,
                            v-cost, OUTPUT fg-rctd.std-cost).

                    DELETE tt-fg-set.
                END.
            END.
        END.
    END.




END PROCEDURE.

PROCEDURE create-loadtag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAM io-tag-no AS CHAR NO-UNDO.
DEF INPUT PARAM fg-rctd-row AS ROWID NO-UNDO.

DEF BUFFER b-loadtag FOR loadtag.
DEF BUFFER b-po-ordl FOR po-ordl.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-fg-rctd FOR fg-rctd.

DEFINE VARIABLE li                    AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-got-job            AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lv-out-cost           AS DECIMAL       DECIMALS 4 NO-UNDO.
DEFINE VARIABLE lv-out-qty            AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-from-uom           AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-cost-uom           AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-ord-qty            AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-ord-uom            AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-setup-included     AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lv-setup-per-cost-uom AS DECIMAL       NO-UNDO.
DEFINE VARIABLE v-bwt                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-len                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-wid                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-dep                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE dRFIDTag              AS DECIMAL       NO-UNDO.

DEF VAR v-fgrecpt   AS LOG NO-UNDO. 
DEF VAR tb_ret      AS LOG INIT YES NO-UNDO.
DEF VAR v-loadtag   AS CHAR NO-UNDO .
DEF VAR v-mult      AS INT NO-UNDO.
DEF VAR v-cas-lab   AS LOG NO-UNDO.
DEF VAR v-tags      AS DEC NO-UNDO.

FIND FIRST sys-ctrl
  WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "FGRECPT"
  NO-LOCK NO-ERROR.
ASSIGN
v-fgrecpt = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".

FIND FIRST sys-ctrl
  WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name    eq "LOADTAG"
  NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN
ASSIGN v-loadtag = sys-ctrl.char-fld
       v-mult    = sys-ctrl.int-fld
       v-cas-lab = sys-ctrl.log-fld
       v-tags    = sys-ctrl.dec-fld.

FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ fg-rctd-row NO-LOCK NO-ERROR.
FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ b-fg-rctd.i-no
                  NO-LOCK NO-ERROR.


CREATE loadtag.
ASSIGN
  loadtag.company      = cocode
  loadtag.tag-no       = io-tag-no
  loadtag.item-type    = NO /*FGitem*/
  loadtag.job-no       = b-fg-rctd.job-no
  loadtag.job-no2      = b-fg-rctd.job-no2
  /*
  loadtag.ord-no       = IF CAN-FIND(FIRST cust WHERE cust.company = cocode
                                                  AND cust.cust-no     = itemfg.cust-no
                                                  AND cust.active      = "X")
                         THEN 0 ELSE b-fg-rctd.ord-no
  */
  loadtag.i-no         = CAPS(b-fg-rctd.i-no)
  loadtag.i-name       = b-fg-rctd.i-name
  loadtag.qty          = b-fg-rctd.qty
  loadtag.qty-case     = b-fg-rctd.qty-case
  loadtag.case-bundle  = b-fg-rctd.cases-stack
  loadtag.pallet-count = IF b-fg-rctd.units-pallet GT 0 THEN 
                            b-fg-rctd.qty MOD b-fg-rctd.units-pallet
                         ELSE
                            0
  loadtag.partial      = IF b-fg-rctd.qty-case GT 0 THEN 
                            b-fg-rctd.qty MOD b-fg-rctd.qty-case
                         ELSE
                            0
  loadtag.sts          = "Printed" 
  loadtag.tag-date     = TODAY
  loadtag.tag-time     = TIME
  loadtag.misc-dec[1]  = b-fg-rctd.tot-wt
  /*
  loadtag.misc-dec[2]  = b-fg-rctd.pallt-wt
  loadtag.misc-char[2] = b-fg-rctd.lot
  */
  loadtag.po-no = INT(b-fg-rctd.po-no).

IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.

IF v-loadtag = "CentBox" THEN DO:
  ASSIGN loadtag.loc     = itemfg.def-loc
         loadtag.loc-bin = itemfg.def-loc-bin.
  FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
                      AND fg-bin.i-no    EQ itemfg.i-no
                      AND fg-bin.job-no  EQ b-fg-rctd.job-no
                      AND fg-bin.tag     EQ loadtag.tag-no
                    NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
  ASSIGN loadtag.loc     = fg-bin.loc
         loadtag.loc-bin = fg-bin.loc-bin.
  
END.
ELSE RUN fg/autopost.p (ROWID(itemfg), b-fg-rctd.job-no, b-fg-rctd.job-no2,
                        OUTPUT loadtag.loc , OUTPUT loadtag.loc-bin).

IF RFIDTag-log THEN DO:
  FIND FIRST oe-ctrl WHERE oe-ctrl.company = loadtag.company 
                     NO-ERROR.
  dRFIDTag = IF AVAIL oe-ctrl AND oe-ctrl.spare-char-1 <> ""
             THEN dec(oe-ctrl.spare-char-1) ELSE 111110000000000000000001.
  oe-ctrl.spare-char-1 = string(dRFIDTag + 1).
  CREATE rfidtag.
  ASSIGN rfidtag.company   = loadtag.company
         rfidtag.item-type = loadtag.item-type
         rfidtag.tag-no    = loadtag.tag-no
         rfidtag.rfidtag   = STRING(dRFIDTag).
  RELEASE oe-ctrl.
END.


FIND CURRENT loadtag NO-LOCK NO-ERROR.
FIND CURRENT b-fg-rctd NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE DeleteSetParts:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinker AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.

    FOR EACH fg-rcpts
        WHERE fg-rcpts.company EQ cocode
        AND fg-rcpts.linker  EQ ipcLinker 
        NO-LOCK :
        FOR EACH b-fg-rctd
            WHERE b-fg-rctd.company EQ cocode
            AND b-fg-rctd.r-no    EQ fg-rcpts.r-no USE-INDEX fg-rctd
            EXCLUSIVE-LOCK:
            DELETE b-fg-rctd.
        END.
    END.



END PROCEDURE.

PROCEDURE get-next-tag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipc-i-no AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER opc-next-tag AS CHAR NO-UNDO.
  DEF BUFFER bf-loadtag FOR loadtag.
  DEF VAR io-tag-no AS INT NO-UNDO.

  FIND LAST bf-loadtag NO-LOCK
      WHERE bf-loadtag.company     EQ cocode
        AND bf-loadtag.item-type   EQ NO
        AND bf-loadtag.is-case-tag EQ NO
        AND bf-loadtag.tag-no      BEGINS ipc-i-no
        AND SUBSTR(bf-loadtag.tag-no,1,15) EQ ipc-i-no
      USE-INDEX tag NO-ERROR.

  io-tag-no = (IF AVAIL bf-loadtag THEN INT(SUBSTR(bf-loadtag.tag-no,16,5)) ELSE 0) + 1.

  opc-next-tag = STRING(CAPS(ipc-i-no),"x(15)") + STRING(io-tag-no,"99999").

END PROCEDURE.
