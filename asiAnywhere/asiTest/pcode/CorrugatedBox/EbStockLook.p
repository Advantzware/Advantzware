/*------------------------------------------------------------------------
    File      : EbStockLook
    Purpose   :  Corrugated Customer Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 2 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEbStockLook NO-UNDO 
        FIELD vEst         AS CHAR
        FIELD vCust        AS CHAR
        FIELD vPart        AS CHAR
        FIELD vPartDscr    AS CHAR
        FIELD vStock         AS character
        FIELD vDscr2       AS CHAR
        FIELD vProcat     AS CHAR
        FIELD vLen        AS DECIMAL
        FIELD vWid        AS DECIMAL
        FIELD vDep        AS DECIMAL
        FIELD bnhj  AS CHAR
        
        .
    
DEFINE DATASET dsEbStockLook FOR ttEbStockLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmType      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEbStockLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmType        = ? THEN ASSIGN prmType    = 0.
IF prmEstimate    = ? THEN ASSIGN prmEstimate    = "".

DEFINE VAR v-count AS INT NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".  


if prmAction <> "search" then do:
    v-count = 0 . 

    FOR EACH eb WHERE eb.company = prmComp  AND ((eb.est-type ge 1 and eb.est-type le 4 and prmType ge 1 and prmType le 4) or 
        (eb.est-type gt 4 and prmType gt 4))  /*AND rowid(eb) ne ip-rowid*/  NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" 
        AND reftable.company  EQ eb.company 
        AND reftable.loc      EQ "" 
        AND reftable.code     EQ eb.stock-no 
        AND reftable.code2    EQ "A" NO-LOCK, 
        FIRST ef OF eb NO-LOCK, 
        FIRST est OF eb NO-LOCK :
        IF AVAIL eb  THEN DO:
        create ttEbStockLook.
        assign  
            ttEbStockLook.vEst            = eb.est-no
            ttEbStockLook.vCust           = eb.cust-no 
            ttEbStockLook.vPart           = eb.part-no
            ttEbStockLook.vPartDscr       = eb.part-dscr1 
            ttEbStockLook.vStock          = eb.stock-no
            ttEbStockLook.vDscr2          = eb.part-dscr2 
            ttEbStockLook.vProcat         = eb.procat
            ttEbStockLook.vLen            = eb.len 
            ttEbStockLook.vWid            = eb.wid
            ttEbStockLook.vDep            = eb.dep 
           .
                 
             v-count = v-count + 1 .
             IF v-count > 100 THEN LEAVE.

         END.	 /* IF AVAIL eb */
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "Est" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH eb WHERE  eb.company = prmComp  AND ((eb.est-type ge 1 and eb.est-type le 4 and prmType ge 1 and prmType le 4) or 
        (eb.est-type gt 4 and prmType gt 4))  /*AND rowid(eb) ne ip-rowid*/ AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText)  NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" 
        AND reftable.company  EQ eb.company 
        AND reftable.loc      EQ "" 
        AND reftable.code     EQ eb.stock-no 
        AND reftable.code2    EQ "A" NO-LOCK, 
        FIRST ef OF eb NO-LOCK, 
        FIRST est OF eb NO-LOCK :
        IF AVAIL eb  THEN DO:
        create ttEbStockLook.
        assign  
            ttEbStockLook.vEst            = eb.est-no
            ttEbStockLook.vCust           = eb.cust-no 
            ttEbStockLook.vPart           = eb.part-no
            ttEbStockLook.vPartDscr       = eb.part-dscr1 
            ttEbStockLook.vStock          = eb.stock-no
            ttEbStockLook.vDscr2          = eb.part-dscr2 
            ttEbStockLook.vProcat         = eb.procat
            ttEbStockLook.vLen            = eb.len 
            ttEbStockLook.vWid            = eb.wid
            ttEbStockLook.vDep            = eb.dep 
           .
             
                
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH eb WHERE  eb.company = prmComp  AND ((eb.est-type ge 1 and eb.est-type le 4 and prmType ge 1 and prmType le 4) or 
        (eb.est-type gt 4 and prmType gt 4))  /*AND rowid(eb) ne ip-rowid*/ AND eb.est-no BEGINS  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText)  NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" 
        AND reftable.company  EQ eb.company 
        AND reftable.loc      EQ "" 
        AND reftable.code     EQ eb.stock-no 
        AND reftable.code2    EQ "A" NO-LOCK, 
        FIRST ef OF eb NO-LOCK, 
        FIRST est OF eb NO-LOCK :
        IF AVAIL eb  THEN DO:
        create ttEbStockLook.
        assign  
            ttEbStockLook.vEst            = eb.est-no
            ttEbStockLook.vCust           = eb.cust-no 
            ttEbStockLook.vPart           = eb.part-no
            ttEbStockLook.vPartDscr       = eb.part-dscr1 
            ttEbStockLook.vStock          = eb.stock-no
            ttEbStockLook.vDscr2          = eb.part-dscr2 
            ttEbStockLook.vProcat         = eb.procat
            ttEbStockLook.vLen            = eb.len 
            ttEbStockLook.vWid            = eb.wid
            ttEbStockLook.vDep            = eb.dep 
           .
           
           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "Cust" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH eb WHERE eb.company = prmComp  AND ((eb.est-type ge 1 and eb.est-type le 4 and prmType ge 1 and prmType le 4) or 
        (eb.est-type gt 4 and prmType gt 4))  /*AND rowid(eb) ne ip-rowid*/ AND eb.cust-no = prmText  NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" 
        AND reftable.company  EQ eb.company 
        AND reftable.loc      EQ "" 
        AND reftable.code     EQ eb.stock-no 
        AND reftable.code2    EQ "A" NO-LOCK, 
        FIRST ef OF eb NO-LOCK, 
        FIRST est OF eb NO-LOCK :
        IF AVAIL eb  THEN DO:
        create ttEbStockLook.
        assign  
            ttEbStockLook.vEst            = eb.est-no
            ttEbStockLook.vCust           = eb.cust-no 
            ttEbStockLook.vPart           = eb.part-no
            ttEbStockLook.vPartDscr       = eb.part-dscr1 
            ttEbStockLook.vStock          = eb.stock-no
            ttEbStockLook.vDscr2          = eb.part-dscr2 
            ttEbStockLook.vProcat         = eb.procat
            ttEbStockLook.vLen            = eb.len 
            ttEbStockLook.vWid            = eb.wid
            ttEbStockLook.vDep            = eb.dep 
           .
            
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH eb WHERE eb.company = prmComp  AND ((eb.est-type ge 1 and eb.est-type le 4 and prmType ge 1 and prmType le 4) or 
        (eb.est-type gt 4 and prmType gt 4))  /*AND rowid(eb) ne ip-rowid*/ AND eb.cust-no BEGINS prmText  NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" 
        AND reftable.company  EQ eb.company 
        AND reftable.loc      EQ "" 
        AND reftable.code     EQ eb.stock-no 
        AND reftable.code2    EQ "A" NO-LOCK, 
        FIRST ef OF eb NO-LOCK, 
        FIRST est OF eb NO-LOCK :
        IF AVAIL eb  THEN DO:
        create ttEbStockLook.
        assign  
            ttEbStockLook.vEst            = eb.est-no
            ttEbStockLook.vCust           = eb.cust-no 
            ttEbStockLook.vPart           = eb.part-no
            ttEbStockLook.vPartDscr       = eb.part-dscr1 
            ttEbStockLook.vStock          = eb.stock-no
            ttEbStockLook.vDscr2          = eb.part-dscr2 
            ttEbStockLook.vProcat         = eb.procat
            ttEbStockLook.vLen            = eb.len 
            ttEbStockLook.vWid            = eb.wid
            ttEbStockLook.vDep            = eb.dep 
           .
           
           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*end of prmfield */


IF prmField = "FGItem" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH eb WHERE eb.company = prmComp  AND ((eb.est-type ge 1 and eb.est-type le 4 and prmType ge 1 and prmType le 4) or 
        (eb.est-type gt 4 and prmType gt 4))  /*AND rowid(eb) ne ip-rowid*/ AND eb.stock-no = prmText  NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" 
        AND reftable.company  EQ eb.company 
        AND reftable.loc      EQ "" 
        AND reftable.code     EQ eb.stock-no 
        AND reftable.code2    EQ "A" NO-LOCK, 
        FIRST ef OF eb NO-LOCK, 
        FIRST est OF eb NO-LOCK :
        IF AVAIL eb  THEN DO:
        create ttEbStockLook.
        assign  
            ttEbStockLook.vEst            = eb.est-no
            ttEbStockLook.vCust           = eb.cust-no 
            ttEbStockLook.vPart           = eb.part-no
            ttEbStockLook.vPartDscr       = eb.part-dscr1 
            ttEbStockLook.vStock          = eb.stock-no
            ttEbStockLook.vDscr2          = eb.part-dscr2 
            ttEbStockLook.vProcat         = eb.procat
            ttEbStockLook.vLen            = eb.len 
            ttEbStockLook.vWid            = eb.wid
            ttEbStockLook.vDep            = eb.dep 
           .
               
             
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
     FOR EACH eb WHERE eb.company = prmComp  AND ((eb.est-type ge 1 and eb.est-type le 4 and prmType ge 1 and prmType le 4) or 
        (eb.est-type gt 4 and prmType gt 4))  /*AND rowid(eb) ne ip-rowid*/ AND eb.stock-no BEGINS prmText  NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "FGSTATUS" 
        AND reftable.company  EQ eb.company 
        AND reftable.loc      EQ "" 
        AND reftable.code     EQ eb.stock-no 
        AND reftable.code2    EQ "A" NO-LOCK, 
        FIRST ef OF eb NO-LOCK, 
        FIRST est OF eb NO-LOCK :
        IF AVAIL eb  THEN DO:
        create ttEbStockLook.
        assign  
            ttEbStockLook.vEst            = eb.est-no
            ttEbStockLook.vCust           = eb.cust-no 
            ttEbStockLook.vPart           = eb.part-no
            ttEbStockLook.vPartDscr       = eb.part-dscr1 
            ttEbStockLook.vStock          = eb.stock-no
            ttEbStockLook.vDscr2          = eb.part-dscr2 
            ttEbStockLook.vProcat         = eb.procat
            ttEbStockLook.vLen            = eb.len 
            ttEbStockLook.vWid            = eb.wid
            ttEbStockLook.vDep            = eb.dep 
           .
               

           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/


END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


