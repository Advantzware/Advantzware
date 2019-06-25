            /*------------------------------------------------------------------------
    File        : lcpartlook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all Custpart

    Author(s)   : 
    Created     : Aug 20 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLcPartQuoteLook NO-UNDO 
    FIELD lc-custno  AS CHAR
    FIELD lc-partno  AS CHAR
    FIELD lc-ino     AS CHAR
    FIELD lc-iname   AS CHAR
    FIELD lc-style   AS CHAR
    FIELD lc-length  AS DECIMAL
    FIELD lc-width   AS DECIMAL
    FIELD lc-depth   AS DECIMAL
    FIELD lc-price   AS DECIMAL
    FIELD lc-uom     AS CHAR 
    FIELD lc-size    AS CHAR
    .

DEFINE DATASET dsLcPartQuoteLook FOR ttLcPartQuoteLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INT  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsLcPartQuoteLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.

DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEFINE VARIABLE custX AS CHARACTER NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 prmLoc   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

ASSIGN
    g_company = prmComp
    g_loc     = "MAIN"  .



 FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
  FIND FIRST cust NO-LOCK WHERE cust.company EQ prmComp
                              AND cust.active EQ 'X' NO-ERROR.
    custX = IF AVAILABLE cust THEN cust.cust-no ELSE ''.

if prmAction <> "search" then do:
    FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
        (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX) NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = itemfg.sell-price
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])                .
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "cust" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX) AND ASI.cust-part.cust-no EQ prmText NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])                  .
    END.	 /* FOR EACH item */  
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX) AND ASI.cust-part.cust-no BEGINS prmText NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])                 .
    END.	 /* FOR EACH item */        
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = cust*/  
    if prmField = "partno"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX) AND ASI.cust-part.part-no EQ prmText NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])                    .
    END.	 /* FOR EACH item */  
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
             FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX) AND ASI.cust-part.part-no BEGINS prmText NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])              .
    END.	 /* FOR EACH item */  
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "fgitem" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX) AND ASI.cust-part.i-no EQ prmText NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])                  .
    END.	 /* FOR EACH item */  
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
            FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX) AND ASI.cust-part.i-no BEGINS prmText NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])              .
    END.	 /* FOR EACH item */
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = i-no */
     IF prmField = "i-name" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX)  NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no AND itemfg.i-name EQ prmText NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])                 .
    END.	 /* FOR EACH item */
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH cust-part WHERE  cust-part.company EQ prmComp AND 
                (ASI.cust-part.cust-no EQ quotehd.cust-no OR ASI.cust-part.cust-no EQ custX)  NO-LOCK, 
        FIRST itemfg WHERE itemfg.company = cust-part.company 
            AND itemfg.i-no = cust-part.i-no AND itemfg.i-name BEGINS prmText NO-LOCK :

        create ttLcPartQuoteLook.
            assign                                         
               ttLcPartQuoteLook.lc-custno   =  cust-part.cust-no  
               ttLcPartQuoteLook.lc-partno   = cust-part.part-no
               ttLcPartQuoteLook.lc-ino      =  cust-part.i-no
               ttLcPartQuoteLook.lc-iname    =  itemfg.i-name
               ttLcPartQuoteLook.lc-style    = itemfg.style 
               ttLcPartQuoteLook.lc-length   =  itemfg.l-score[50] 
               ttLcPartQuoteLook.lc-width    =  itemfg.w-score[50] 
               ttLcPartQuoteLook.lc-depth    =  itemfg.d-score[50] 

               ttLcPartQuoteLook.lc-price     = (itemfg.sell-price)
               ttLcPartQuoteLook.lc-uom       = itemfg.sell-uom
               ttLcPartQuoteLook.lc-size      = STRING(itemfg.l-score[50]) + " x " +
                                                                   STRING(itemfg.w-score[50]) + " x " +
                                                                   STRING(itemfg.d-score[50])                .
    END.	 /* FOR EACH item */
        END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = stock-no */
END.  /* IF prmAction = search then do: */



