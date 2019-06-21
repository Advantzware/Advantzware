




/*------------------------------------------------------------------------
    File        : itemquotelook.p
    Purpose     : FGItem

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemQuoteLook NO-UNDO 
    FIELD vqno      AS INT
    FIELD vino      AS CHARACTER
    FIELD viname    AS CHARACTER
    FIELD vhand     AS DECIMAL
    FIELD vcust     AS CHARACTER
    FIELD vcustpart AS CHARACTER
    FIELD vdscr     AS CHARACTER
    FIELD vest      AS CHARACTER

    FIELD vpartdscr1     AS CHARACTER
    FIELD vpartdscr2     AS CHARACTER
    FIELD vprice         AS DECIMAL
    FIELD vuom           AS CHARACTER
    FIELD vtype          AS INT
    FIELD vdiscount      AS DECIMAL
    FIELD vqty           AS INT

    FIELD vino2      AS CHARACTER
    FIELD viname2    AS CHARACTER
    FIELD vcustpart2 AS CHARACTER
    FIELD vdscr2     AS CHARACTER
    FIELD vpartdscr3     AS CHARACTER
    FIELD vpartdscr4     AS CHARACTER
    FIELD vqtyunit       AS INT 
    FIELD vunitpallet       AS INT
    FIELD vpartial       AS DECIMAL
    .
                                           
    
DEFINE DATASET dsItemQuoteLook FOR ttItemQuoteLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemQuoteLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR vItem AS CHAR.
DEF VAR vName AS CHAR.

DEF VAR li-cnt AS DEC NO-UNDO.
DEF VAR li-cases AS DEC NO-UNDO.
DEFINE VAR v-count AS INT NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmQuote     = ? THEN ASSIGN prmQuote     = 0.
IF prmCust      = ? THEN ASSIGN prmCust      = "".
 
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEFINE VAR custcount AS CHAR NO-UNDO.

FOR EACH usercust WHERE
    usercust.user_id =  prmUser AND
    usercust.company = prmComp
     NO-LOCK:
    ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END. /* end of usercust*/

if prmAction <> "search" then do:
        v-count = 0.
            MAIN-LOOP:
        FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote OR prmQuote = 0) AND quotehd.cust-no = prmCust NO-LOCK ,
                  EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK BY quotehd.q-no DESC :
                 
                   create ttItemQuoteLook.
                   assign  
                       ttItemQuoteLook.vqno         = quotehd.q-no
                       ttItemQuoteLook.vest         = quotehd.est-no
                       ttItemQuoteLook.vcust        = quotehd.cust-no 
                       ttItemQuoteLook.vcustpart    = quoteitm.part-no
                       ttItemQuoteLook.vcustpart2    = quoteitm.part-no
                       ttItemQuoteLook.vino         = quoteitm.i-no
                       ttItemQuoteLook.vino2         = quoteitm.i-no
                       ttItemQuoteLook.vuom         = quoteitm.uom
                       ttItemQuoteLook.vdscr        = quoteitm.part-dscr1
                       ttItemQuoteLook.vdscr2        = quoteitm.part-dscr1
                       ttItemQuoteLook.vpartdscr1   = quoteitm.part-dscr2
                       ttItemQuoteLook.vpartdscr3   = quoteitm.part-dscr2
                       ttItemQuoteLook.vpartdscr2   = quoteitm.part-dscr3
                       ttItemQuoteLook.vpartdscr4   = quoteitm.part-dscr3
                       ttItemQuoteLook.vprice       = quoteitm.price
                       ttItemQuoteLook.vqty         = quoteitm.qty 
                       ttItemQuoteLook.vtype        = quoteitm.est-type .
                    
                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttItemQuoteLook.viname     = itemfg.i-name
                       ttItemQuoteLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttItemQuoteLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no NE 0  NO-LOCK NO-ERROR.

                   RUN est/getcscnt.p (ROWID(eb),
                         OUTPUT li-cnt,OUTPUT li-cases).

                   ASSIGN
                       /*ttItemQuoteLook.vqtyunit      = eb.cas-cnt 
                       ttItemQuoteLook.vunitpallet   = eb.cas-pal 
                       */
                        
                       ttItemQuoteLook.vqtyunit      = li-cnt 
                       ttItemQuoteLook.vunitpallet   = li-cases 
                       
                       .

                   ASSIGN
                        ttItemQuoteLook.vpartial = DEC(ttItemQuoteLook.vqty) MOD INT(ttItemQuoteLook.vqtyunit).

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttItemQuoteLook.vino         = eb.stock-no
                             ttItemQuoteLook.vino2        = eb.stock-no .

                    END.
                      v-count = v-count + 1.
                      IF v-count = 40 THEN LEAVE MAIN-LOOP.

        END.  /*FOR EACH Itemfg*/
   
    FOR EACH ttItemQuoteLook NO-LOCK:
        IF INDEX(ttItemQuoteLook.vino ,'"',1) > 0 THEN ASSIGN
            ttItemQuoteLook.vino  = REPLACE(ttItemQuoteLook.vino ,'"',":").
        IF INDEX(ttItemQuoteLook.viname ,'"',1) > 0 THEN ASSIGN
            ttItemQuoteLook.viname  = REPLACE(ttItemQuoteLook.viname ,'"',":").

        IF INDEX(ttItemQuoteLook.vdscr ,'"',1) > 0 THEN ASSIGN
            ttItemQuoteLook.vdscr  = REPLACE(ttItemQuoteLook.vdscr ,'"',":").
        IF INDEX(ttItemQuoteLook.vpartdscr1 ,'"',1) > 0 THEN ASSIGN
            ttItemQuoteLook.vpartdscr1  = REPLACE(ttItemQuoteLook.vpartdscr1 ,'"',":").
        IF INDEX(ttItemQuoteLook.vpartdscr2 ,'"',1) > 0 THEN ASSIGN
            ttItemQuoteLook.vpartdscr2  = REPLACE(ttItemQuoteLook.vpartdscr2 ,'"',":").
        IF INDEX(ttItemQuoteLook.vcustpart ,'"',1) > 0 THEN ASSIGN
            ttItemQuoteLook.vcustpart  = REPLACE(ttItemQuoteLook.vcustpart ,'"',":").
     END.
END.  /*ifif prmAction <> "search" */ 

    IF prmAction = "search" then do:
     
     if prmField = "Quote"  then do:
         if prmCondition = "EQUAL" then do:
            FOR EACH quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = INTEGER (prmText) AND quotehd.cust-no = prmCust NO-LOCK ,
                  EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK:
                  
                   create ttItemQuoteLook.
                   assign  
                       ttItemQuoteLook.vqno         = quotehd.q-no
                       ttItemQuoteLook.vest         = quotehd.est-no
                       ttItemQuoteLook.vcust        = quotehd.cust-no 
                       ttItemQuoteLook.vcustpart    = quoteitm.part-no
                       ttItemQuoteLook.vcustpart2    = quoteitm.part-no
                       ttItemQuoteLook.vino         = quoteitm.i-no
                       ttItemQuoteLook.vino2         = quoteitm.i-no
                       ttItemQuoteLook.vuom         = quoteitm.uom
                       ttItemQuoteLook.vdscr        = quoteitm.part-dscr1
                       ttItemQuoteLook.vdscr2        = quoteitm.part-dscr1
                       ttItemQuoteLook.vpartdscr1   = quoteitm.part-dscr2
                       ttItemQuoteLook.vpartdscr3   = quoteitm.part-dscr2
                       ttItemQuoteLook.vpartdscr2   = quoteitm.part-dscr3
                       ttItemQuoteLook.vpartdscr4   = quoteitm.part-dscr3
                       ttItemQuoteLook.vprice       = quoteitm.price
                       ttItemQuoteLook.vqty         = quoteitm.qty 
                       ttItemQuoteLook.vtype        = quoteitm.est-type  .

                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttItemQuoteLook.viname     = itemfg.i-name
                       ttItemQuoteLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttItemQuoteLook.vdiscount      = cust.disc .
                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1 NO-LOCK NO-ERROR.
                   ASSIGN
                       ttItemQuoteLook.vqtyunit      = eb.cas-cnt
                       ttItemQuoteLook.vunitpallet   = eb.cas-pal 
                       .
                   ASSIGN
                        ttItemQuoteLook.vpartial = DEC(ttItemQuoteLook.vqty) MOD INT(ttItemQuoteLook.vqtyunit).

                   IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttItemQuoteLook.vino         = eb.stock-no
                             ttItemQuoteLook.vino2        = eb.stock-no .

                    END.
            END.

            FOR EACH ttItemQuoteLook NO-LOCK:
                IF INDEX(ttItemQuoteLook.vino ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vino  = REPLACE(ttItemQuoteLook.vino ,'"',":").
                IF INDEX(ttItemQuoteLook.viname ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.viname  = REPLACE(ttItemQuoteLook.viname ,'"',":").
                
                IF INDEX(ttItemQuoteLook.vdscr ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vdscr  = REPLACE(ttItemQuoteLook.vdscr ,'"',":").
                IF INDEX(ttItemQuoteLook.vpartdscr1 ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vpartdscr1  = REPLACE(ttItemQuoteLook.vpartdscr1 ,'"',":").
                IF INDEX(ttItemQuoteLook.vpartdscr2 ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vpartdscr2  = REPLACE(ttItemQuoteLook.vpartdscr2 ,'"',":").
                IF INDEX(ttItemQuoteLook.vcustpart ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vcustpart  = REPLACE(ttItemQuoteLook.vcustpart ,'"',":").

            END.

          END. 
         end.  /* if prmField = state  */


   if prmField = "Est"  then do:
         if prmCondition = "EQUAL" then do:
             
            FOR EACH quotehd WHERE quotehd.company = prmComp AND quotehd.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText)  
                AND quotehd.cust-no = prmCust NO-LOCK ,
                  EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no 
                AND quoteitm.company = prmComp NO-LOCK:
                   create ttItemQuoteLook.
                   assign  
                       ttItemQuoteLook.vqno         = quotehd.q-no
                       ttItemQuoteLook.vest         = quotehd.est-no
                       ttItemQuoteLook.vcust        = quotehd.cust-no 
                       ttItemQuoteLook.vcustpart    = quoteitm.part-no
                       ttItemQuoteLook.vcustpart2    = quoteitm.part-no
                       ttItemQuoteLook.vino         = quoteitm.i-no
                       ttItemQuoteLook.vino2         = quoteitm.i-no
                       ttItemQuoteLook.vuom         = quoteitm.uom
                       ttItemQuoteLook.vdscr        = quoteitm.part-dscr1
                       ttItemQuoteLook.vdscr2        = quoteitm.part-dscr1
                       ttItemQuoteLook.vpartdscr1   = quoteitm.part-dscr2
                       ttItemQuoteLook.vpartdscr3   = quoteitm.part-dscr2
                       ttItemQuoteLook.vpartdscr2   = quoteitm.part-dscr3
                       ttItemQuoteLook.vpartdscr4   = quoteitm.part-dscr3
                       ttItemQuoteLook.vprice       = quoteitm.price
                       ttItemQuoteLook.vqty         = quoteitm.qty 
                       ttItemQuoteLook.vtype        = quoteitm.est-type  .

                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttItemQuoteLook.viname     = itemfg.i-name
                       ttItemQuoteLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttItemQuoteLook.vdiscount      = cust.disc .
                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1 NO-LOCK NO-ERROR.
                   ASSIGN
                       ttItemQuoteLook.vqtyunit      = eb.cas-cnt
                       ttItemQuoteLook.vunitpallet   = eb.cas-pal 
                       .
                   ASSIGN
                        ttItemQuoteLook.vpartial = DEC(ttItemQuoteLook.vqty) MOD INT(ttItemQuoteLook.vqtyunit).

                   IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttItemQuoteLook.vino         = eb.stock-no
                             ttItemQuoteLook.vino2        = eb.stock-no .

                    END.
            END.

            FOR EACH ttItemQuoteLook NO-LOCK:
                IF INDEX(ttItemQuoteLook.vino ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vino  = REPLACE(ttItemQuoteLook.vino ,'"',":").
                IF INDEX(ttItemQuoteLook.viname ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.viname  = REPLACE(ttItemQuoteLook.viname ,'"',":").
                
                IF INDEX(ttItemQuoteLook.vdscr ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vdscr  = REPLACE(ttItemQuoteLook.vdscr ,'"',":").
                IF INDEX(ttItemQuoteLook.vpartdscr1 ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vpartdscr1  = REPLACE(ttItemQuoteLook.vpartdscr1 ,'"',":").
                IF INDEX(ttItemQuoteLook.vpartdscr2 ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vpartdscr2  = REPLACE(ttItemQuoteLook.vpartdscr2 ,'"',":").
                IF INDEX(ttItemQuoteLook.vcustpart ,'"',1) > 0 THEN ASSIGN
                    ttItemQuoteLook.vcustpart  = REPLACE(ttItemQuoteLook.vcustpart ,'"',":").

            END.

          END. 
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */   

