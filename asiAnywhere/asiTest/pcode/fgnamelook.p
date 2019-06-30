




/*------------------------------------------------------------------------
    File        : fgnamelook.p
    Purpose     : FGItem

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFGNameLook NO-UNDO 
    FIELD vino          AS CHARACTER
    FIELD viname        AS CHARACTER
    
    FIELD vhand          AS DECIMAL
    FIELD vcust          AS CHARACTER
    FIELD vcustpart      AS CHARACTER
    FIELD vdscr          AS CHARACTER
    FIELD vmat           AS LOGICAL
    FIELD vest           AS CHARACTER

    FIELD vpartdscr1     AS CHARACTER
    FIELD vpartdscr2     AS CHARACTER
    FIELD vprice         AS DECIMAL
    FIELD vuom           AS CHARACTER
    FIELD vcasecount     AS INTEGER
    FIELD vcasepall      AS INTEGER
    FIELD vtotprice      AS DECIMAL
    FIELD vcost          AS DECIMAL
    FIELD vtype          AS CHARACTER
    FIELD vdiscount      AS DECIMAL
    FIELD vino2          AS CHARACTER
    FIELD viname2        AS CHARACTER
    FIELD vcustpart2     AS CHARACTER
    FIELD vdscr2         AS CHARACTER
    FIELD vpartdscr3     AS CHARACTER
    FIELD vpartdscr4     AS CHARACTER
    .
                                           
    
DEFINE DATASET dsFGNameLook FOR ttFGNameLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmQty       AS INT  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFGNameLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR vItem AS CHAR.
DEF VAR vName AS CHAR.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmQuote     = ? THEN ASSIGN prmQuote     = 0.
IF prmQty       = ? THEN ASSIGN prmQty       = 0.

 DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
DEFINE VAR li-pnt AS INT NO-UNDO.
DEFINE VAR custype AS CHAR NO-UNDO.
DEF VAR itemcount AS CHAR NO-UNDO.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
DEF VAR lv-date AS CHAR NO-UNDO.
ASSIGN
 lv-date = STRING(YEAR(TODAY),"9999") +
           STRING(MONTH(TODAY),"99")  +
           STRING(DAY(TODAY),"99")  
    cocode = prmComp .
{sys/inc/sellpric.i}
FIND FIRST cust WHERE cust.company = prmComp  AND  cust.cust-no = prmCustomer NO-LOCK NO-ERROR.
ASSIGN
    custype = cust.TYPE .


if prmAction <> "search" then do:

    IF prmQuote > 0 THEN DO:
         
        FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                  EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK BY quotehd.q-no DESC :
                 
                   create ttFGNameLook.
                   assign

                       ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .
                    
                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .

                    END.
                     

        END.  /*FOR EACH Itemfg*/
   
    FOR EACH ttFGNameLook NO-LOCK:
        IF INDEX(ttFGNameLook.vino ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vino  = REPLACE(ttFGNameLook.vino ,'"',":").
        IF INDEX(ttFGNameLook.viname ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.viname  = REPLACE(ttFGNameLook.viname ,'"',":").

        IF INDEX(ttFGNameLook.vdscr ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vdscr  = REPLACE(ttFGNameLook.vdscr ,'"',":").
        IF INDEX(ttFGNameLook.vpartdscr1 ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vpartdscr1  = REPLACE(ttFGNameLook.vpartdscr1 ,'"',":").
        IF INDEX(ttFGNameLook.vpartdscr2 ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vpartdscr2  = REPLACE(ttFGNameLook.vpartdscr2 ,'"',":").
        IF INDEX(ttFGNameLook.vcustpart ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vcustpart  = REPLACE(ttFGNameLook.vcustpart ,'"',":").
     END.
    END.  /* if prmquote > 0*/
   

    ELSE DO:
    
        FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :
       FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no  NO-LOCK:
         
        FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
            reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ itemfg.i-no    AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR. 
            
                       create ttFGNameLook.
                       assign
                           ttFGNameLook.vino          = itemfg.i-no
                           ttFGNameLook.viname       = itemfg.i-name
                           ttFGNameLook.vino2          = itemfg.i-no
                           ttFGNameLook.viname2       = itemfg.i-name
                           ttFGNameLook.vhand        = itemfg.q-onh
                           ttFGNameLook.vcust      = itemfg.cust-no
                           ttFGNameLook.vdscr     = itemfg.i-dscr
                           ttFGNameLook.vest     = itemfg.est-no
                           ttFGNameLook.vmat      = itemfg.stocked  

                           ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall       = itemfg.case-pall 
                            ttFGNameLook.vtype          = itemfg.type-code .
                           
                     
                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                           

                        IF AVAIL oe-prmtx THEN DO:
                            li-pnt = 1.                            
                            DO WHILE  li-pnt LE 10: 
                                IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                                    ASSIGN
                                        ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                                        ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                                    IF AVAIL oe-prmtx  THEN  LEAVE.
                                    END.
                                    li-pnt = li-pnt + 1. 
                                 END.  
                           END.
                       IF NOT AVAIL oe-prmtx  THEN DO:
                           ASSIGN
                               ttFGNameLook.vprice          = itemfg.sell-price
                                ttFGNameLook.vuom            = itemfg.sell-uom .
                      END.

                      IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                    FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.
                    
        END.  /*FOR EACH Itemfg*/
    END.
    FOR EACH ttFGNameLook NO-LOCK:
        IF INDEX(ttFGNameLook.vino ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vino  = REPLACE(ttFGNameLook.vino ,'"',":").
        IF INDEX(ttFGNameLook.viname ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.viname  = REPLACE(ttFGNameLook.viname ,'"',":").

        IF INDEX(ttFGNameLook.vdscr ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vdscr  = REPLACE(ttFGNameLook.vdscr ,'"',":").
        IF INDEX(ttFGNameLook.vpartdscr1 ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vpartdscr1  = REPLACE(ttFGNameLook.vpartdscr1 ,'"',":").
        IF INDEX(ttFGNameLook.vpartdscr2 ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vpartdscr2  = REPLACE(ttFGNameLook.vpartdscr2 ,'"',":").
        IF INDEX(ttFGNameLook.vcustpart ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vcustpart  = REPLACE(ttFGNameLook.vcustpart ,'"',":").
     END.

    END.  /* else do*/

   
END.  /*ifif prmAction <> "search" */ 

    IF prmAction = "search" then do:
          /***************************quote***********************/
        IF prmQuote > 0 THEN DO:

             if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                 EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp AND quoteitm.i-no = prmText NO-LOCK BY quotehd.q-no DESC :
             
                   create ttFGNameLook.
                   assign

                       ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .

                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .

                    END.

        END.  /*FOR EACH quote */
          END. /*equal */

          IF prmCondition = "BEGIN" then do:
             FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                 EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp AND quoteitm.i-no BEGINS prmText NO-LOCK BY quotehd.q-no DESC :
             
                   create ttFGNameLook.
                   assign

                        ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .
                    
                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .

                    END.
                     

        END.  /*FOR EACH quote */
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = item  */
           


if prmField = "name"  then do:
         if prmCondition = "EQUAL" then do:
            FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                 EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp AND quoteitm.part-dscr1 = prmText NO-LOCK BY quotehd.q-no DESC :
             
                   create ttFGNameLook.
                   assign

                        ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .
                    
                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .
                    END.

        END.  /*FOR EACH quote */
      END. /*equal*/

          IF prmCondition = "BEGIN" then do:
            FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                 EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp AND quoteitm.part-dscr1 BEGINS prmText NO-LOCK BY quotehd.q-no DESC :
             
                   create ttFGNameLook.
                   assign

                       ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .
                    
                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .

                    END.
                     

        END.  /*FOR EACH quote */
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = name  */

         if prmField = "CustPart"  then do:
         if prmCondition = "EQUAL" then do:
            FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                 EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp AND quoteitm.part-no = prmText NO-LOCK BY quotehd.q-no DESC :
             
                   create ttFGNameLook.
                   assign

                       ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .
                    
                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .
                    END.

        END.  /*FOR EACH quote */
          END. /*equal*/

          IF prmCondition = "BEGIN" then do:
              

              FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                 EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp AND quoteitm.part-no BEGINS prmText NO-LOCK BY quotehd.q-no DESC :
             
                   create ttFGNameLook.
                   assign

                        ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .

                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .
                    END.
                     

        END.  /*FOR EACH quote */
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = part */
           
if prmField = "viewitemfg" then do:

    FOR EACH quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQuote ) AND quotehd.cust-no = prmCustomer NO-LOCK ,
                 EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp AND quoteitm.i-no = prmText NO-LOCK BY quotehd.q-no DESC :
             
                   create ttFGNameLook.
                   assign

                        ttFGNameLook.vcust        = quotehd.cust-no 
                       ttFGNameLook.vcustpart    = quoteitm.part-no
                       ttFGNameLook.vcustpart2    = quoteitm.part-no
                       ttFGNameLook.vino         = quoteitm.i-no
                       ttFGNameLook.vino2         = quoteitm.i-no
                       ttFGNameLook.vuom         = quoteitm.uom
                       ttFGNameLook.viname        = quoteitm.part-dscr1
                       ttFGNameLook.viname2        = quoteitm.part-dscr1
                       ttFGNameLook.vdscr       = quoteitm.part-dscr2
                       ttFGNameLook.vdscr2      = quoteitm.part-dscr2
                       ttFGNameLook.vpartdscr1  = quoteitm.part-dscr3
                       ttFGNameLook.vpartdscr3  = quoteitm.part-dscr3
                       ttFGNameLook.vprice       = quoteitm.price
                       ttFGNameLook.vhand         = quoteitm.qty 
                       ttFGNameLook.vtype        = STRING(quoteitm.est-type) .
                    
                   FIND FIRST itemfg WHERE itemfg.i-no = quoteitm.i-no NO-LOCK NO-ERROR.
                   IF AVAIL itemfg  THEN  DO:
                   ASSIGN 
                       ttFGNameLook.viname     = itemfg.i-name
                       ttFGNameLook.viname2     = itemfg.i-name.
                   END.
                   FIND FIRST cust WHERE cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vdiscount      = cust.disc .

                   FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1  NO-LOCK NO-ERROR.
                   ASSIGN
                       ttFGNameLook.vcasecount      = eb.cas-cnt .

                    IF quoteitm.i-no = "" THEN DO:
                        ASSIGN
                             ttFGNameLook.vino         = eb.stock-no
                             ttFGNameLook.vino2        = eb.stock-no .

                    END.
                     

        END.  /*FOR EACH quote */
     END.  /*FOR EACH Itemfg*/  

 END.  /*if prmQuote > 0 */  
       /********************************item ************************/
        ELSE DO:
             if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :   
             FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no AND itemfg.i-no = prmText   NO-LOCK :
                  
                 FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
                   reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ itemfg.i-no    AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR. 
                 FIND FIRST ttFGNameLook WHERE ttFGNameLook.vino = itemfg.i-no NO-LOCK NO-ERROR.
                  IF AVAIL ttFGNameLook THEN NEXT.
                 create ttFGNameLook.
                 assign                                     
                            ttFGNameLook.vino      = itemfg.i-no
                            ttFGNameLook.viname    = itemfg.i-name
                            ttFGNameLook.vino2      = itemfg.i-no
                            ttFGNameLook.viname2    = itemfg.i-name
                            ttFGNameLook.vhand     = itemfg.q-onh
                            ttFGNameLook.vcust     = itemfg.cust-no
                            ttFGNameLook.vdscr     = itemfg.i-dscr
                            ttFGNameLook.vmat      = itemfg.stocked 
                            ttFGNameLook.vest     = itemfg.est-no 
                     
                            ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall        = itemfg.case-pall
                            ttFGNameLook.vtype          = itemfg.type-code .

                       
                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                           

                        IF AVAIL oe-prmtx THEN DO:
                            li-pnt = 1.                            
                            DO WHILE  li-pnt LE 10: 
                                IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                                    ASSIGN
                                    ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                                    ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                                    IF AVAIL oe-prmtx  THEN  LEAVE.
                                    END.
                                    li-pnt = li-pnt + 1. 
                                 END.  
                           END.
                       IF NOT AVAIL oe-prmtx  THEN DO:
                           ASSIGN
                               ttFGNameLook.vprice          = itemfg.sell-price
                               ttFGNameLook.vuom            = itemfg.sell-uom.
                      END.
                      IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                       FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.

             END.
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
              FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :   
                 FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no AND itemfg.i-no BEGINS prmText  NO-LOCK:
                      
                  FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
                   reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ itemfg.i-no    AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR. 
                  FIND FIRST ttFGNameLook WHERE ttFGNameLook.vino = itemfg.i-no NO-LOCK NO-ERROR.
                  IF AVAIL ttFGNameLook THEN NEXT.
                      create ttFGNameLook.
                      assign   
                            ttFGNameLook.vino      = itemfg.i-no
                            ttFGNameLook.viname    = itemfg.i-name
                            ttFGNameLook.vino2      = itemfg.i-no
                            ttFGNameLook.viname2    = itemfg.i-name
                            ttFGNameLook.vhand     = itemfg.q-onh
                            ttFGNameLook.vcust     = itemfg.cust-no
                            ttFGNameLook.vdscr     = itemfg.i-dscr
                            ttFGNameLook.vmat      = itemfg.stocked 
                            ttFGNameLook.vest     = itemfg.est-no 
                          ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                           ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall        = itemfg.case-pall
                            ttFGNameLook.vtype          = itemfg.type-code .

                      
                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                          

                    IF AVAIL oe-prmtx THEN DO:
                        li-pnt = 1.                            
                      DO WHILE  li-pnt LE 10: 
                          IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                              ASSIGN
                              ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                              ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                             
                              IF AVAIL oe-prmtx  THEN  LEAVE.
                              END.
                          li-pnt = li-pnt + 1. 
                          
                      END.  
                    END.
                    IF NOT AVAIL oe-prmtx  THEN DO:
                        ASSIGN
                        ttFGNameLook.vprice          = itemfg.sell-price
                        ttFGNameLook.vuom            = itemfg.sell-uom .
                    END.
                    IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                    FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                           ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.

                  end.  /*FOR EACH state wher*/
          END.
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           


if prmField = "name"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :   
             FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no AND itemfg.i-name = prmText  NO-LOCK :
                  
                 FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
                   reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ itemfg.i-no    AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR.
                 FIND FIRST ttFGNameLook WHERE ttFGNameLook.vino = itemfg.i-no NO-LOCK NO-ERROR.
                  IF AVAIL ttFGNameLook THEN NEXT.
                 create ttFGNameLook.
                 assign                                     
                            ttFGNameLook.vino      = itemfg.i-no
                            ttFGNameLook.viname    = itemfg.i-name
                            ttFGNameLook.vino2          = itemfg.i-no
                            ttFGNameLook.viname2    = itemfg.i-name
                            ttFGNameLook.vhand     = itemfg.q-onh
                            ttFGNameLook.vcust     = itemfg.cust-no
                            ttFGNameLook.vdscr     = itemfg.i-dscr
                            ttFGNameLook.vmat      = itemfg.stocked  
                            ttFGNameLook.vest     = itemfg.est-no
                             ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                           ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall        = itemfg.case-pall
                           ttFGNameLook.vtype          = itemfg.type-code .

                 
                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                           

                        IF AVAIL oe-prmtx THEN DO:
                            li-pnt = 1.                            
                            DO WHILE  li-pnt LE 10: 
                                IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                                    ASSIGN
                                    ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                                    ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                                    IF AVAIL oe-prmtx  THEN  LEAVE.
                                    END.
                                    li-pnt = li-pnt + 1. 
                                 END.  
                           END.
                       IF NOT AVAIL oe-prmtx  THEN DO:
                           ASSIGN
                               ttFGNameLook.vprice          = itemfg.sell-price
                               ttFGNameLook.vuom            = itemfg.sell-uom.
                      END.
                      IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                   FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.

             END.
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
              FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :   
                  FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no AND itemfg.i-name BEGINS prmText NO-LOCK :
                      
                      FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
                   reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ itemfg.i-no    AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR. 
                      FIND FIRST ttFGNameLook WHERE ttFGNameLook.vino = itemfg.i-no NO-LOCK NO-ERROR.
                        IF AVAIL ttFGNameLook THEN NEXT.
                      create ttFGNameLook.
                      assign   
                            ttFGNameLook.vino      = itemfg.i-no
                            ttFGNameLook.viname    = itemfg.i-name
                            ttFGNameLook.vino2          = itemfg.i-no
                            ttFGNameLook.viname2    = itemfg.i-name
                            ttFGNameLook.vhand     = itemfg.q-onh
                            ttFGNameLook.vcust     = itemfg.cust-no
                            ttFGNameLook.vdscr     = itemfg.i-dscr
                            ttFGNameLook.vmat      = itemfg.stocked 
                            ttFGNameLook.vest     = itemfg.est-no 
                          
                           ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall        = itemfg.case-pall
                            ttFGNameLook.vtype          = itemfg.type-code .

                      
                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                           

                        IF AVAIL oe-prmtx THEN DO:
                            li-pnt = 1.                            
                            DO WHILE  li-pnt LE 10: 
                                IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                                    ASSIGN
                                    ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                                    ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                                   
                                    IF AVAIL oe-prmtx  THEN  LEAVE.
                                    END.
                                    li-pnt = li-pnt + 1. 
                                 END.  
                           END.
                       IF NOT AVAIL oe-prmtx  THEN DO:
                           ASSIGN
                               ttFGNameLook.vprice          = itemfg.sell-price
                               ttFGNameLook.vuom            = itemfg.sell-uom.
                      END.
                      IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                  FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.


                  end.  /*FOR EACH state wher*/
              END.
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */

         if prmField = "CustPart"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :   
             FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no AND itemfg.part-no = prmText   NO-LOCK :
                 
                 FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
                   reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ itemfg.i-no    AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR. 
                 FIND FIRST ttFGNameLook WHERE ttFGNameLook.vino = itemfg.i-no NO-LOCK NO-ERROR.
                  IF AVAIL ttFGNameLook THEN NEXT.
                 create ttFGNameLook.
                 assign                                     
                            ttFGNameLook.vino      = itemfg.i-no
                            ttFGNameLook.viname    = itemfg.i-name
                            ttFGNameLook.vino2          = itemfg.i-no
                            ttFGNameLook.viname2    = itemfg.i-name
                            ttFGNameLook.vhand     = itemfg.q-onh
                            ttFGNameLook.vcust     = itemfg.cust-no
                            ttFGNameLook.vdscr     = itemfg.i-dscr
                            ttFGNameLook.vmat      = itemfg.stocked 
                            ttFGNameLook.vest     = itemfg.est-no 
                            ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                            ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall        = itemfg.case-pall
                            ttFGNameLook.vtype          = itemfg.type-code .

                 
                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                           

                        IF AVAIL oe-prmtx THEN DO:
                            li-pnt = 1.                            
                            DO WHILE  li-pnt LE 10: 
                                IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                                    ASSIGN
                                    ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                                    ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                                    IF AVAIL oe-prmtx  THEN  LEAVE.
                                    END.
                                    li-pnt = li-pnt + 1. 
                                 END.  
                           END.
                       IF NOT AVAIL oe-prmtx  THEN DO:
                           ASSIGN
                               ttFGNameLook.vprice          = itemfg.sell-price
                               ttFGNameLook.vuom            = itemfg.sell-uom .
                      END.
                      IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                   FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.

             END.
             END. .

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
               FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :   
                  FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no AND itemfg.part-no BEGINS prmText  NO-LOCK :
                       
                      FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
                   reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ itemfg.i-no    AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR. 
                      FIND FIRST ttFGNameLook WHERE ttFGNameLook.vino = itemfg.i-no NO-LOCK NO-ERROR.
                       IF AVAIL ttFGNameLook THEN NEXT.
                      create ttFGNameLook.
                      assign   
                            ttFGNameLook.vino      = itemfg.i-no
                            ttFGNameLook.viname    = itemfg.i-name
                            ttFGNameLook.vino2          = itemfg.i-no
                            ttFGNameLook.viname2    = itemfg.i-name
                            ttFGNameLook.vhand     = itemfg.q-onh
                            ttFGNameLook.vcust     = itemfg.cust-no
                            ttFGNameLook.vdscr     = itemfg.i-dscr
                            ttFGNameLook.vmat      = itemfg.stocked 
                            ttFGNameLook.vest     = itemfg.est-no 
                          ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                           ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall        = itemfg.case-pall
                            ttFGNameLook.vtype          = itemfg.type-code .

                      
                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                           

                        IF AVAIL oe-prmtx THEN DO:
                            li-pnt = 1.                            
                            DO WHILE  li-pnt LE 10: 
                                IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                                    ASSIGN
                                    ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                                    ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                                    IF AVAIL oe-prmtx  THEN  LEAVE.
                                    END.
                                    li-pnt = li-pnt + 1. 
                                 END.  
                           END.
                       IF NOT AVAIL oe-prmtx  THEN DO:
                           ASSIGN
                               ttFGNameLook.vprice          = itemfg.sell-price
                               ttFGNameLook.vuom            = itemfg.sell-uom.
                      END.
                      IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                   FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.


                  end.  /*FOR EACH state wher*/
               END.
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
if prmField = "viewitemfg" then do:

    FOR EACH  cust WHERE  cust.company = prmComp  AND ( cust.cust-no = prmCustomer OR cust.ACTIVE = "X" )  NO-LOCK :   
    FOR EACH  itemfg WHERE itemfg.company = prmComp AND itemfg.cust-no = cust.cust-no AND itemfg.i-no = prmText  NO-LOCK:
      
      FIND  FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"     AND 
            reftable.company  EQ itemfg.company AND  reftable.loc  EQ ""   AND reftable.code  EQ prmText   AND 
                 reftable.code2    ne "I" NO-LOCK NO-ERROR. 
                       create ttFGNameLook.
                       assign
                           ttFGNameLook.vino          = itemfg.i-no
                           ttFGNameLook.viname    = itemfg.i-name
                            ttFGNameLook.vino2          = itemfg.i-no
                           ttFGNameLook.viname2    = itemfg.i-name
                           ttFGNameLook.vhand        = itemfg.q-onh
                           ttFGNameLook.vcust      = itemfg.cust-no
                           ttFGNameLook.vdscr     = itemfg.i-dscr
                           ttFGNameLook.vest     = itemfg.est-no
                           ttFGNameLook.vmat      = itemfg.stocked  
                            ttFGNameLook.vpartdscr3      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr4      = itemfg.part-dscr2
                           ttFGNameLook.vdscr2     = itemfg.i-dscr
                           ttFGNameLook.vpartdscr1      = itemfg.part-dscr1
                           ttFGNameLook.vpartdscr2      = itemfg.part-dscr2
                           ttFGNameLook.vcasecount      = itemfg.case-count
                           ttFGNameLook.vcasepall       = itemfg.case-pall 
                            ttFGNameLook.vtype          = itemfg.type-code .

                        /****   pricematrix  *****/
                       FOR EACH oe-prmtx
                           WHERE oe-prmtx.company = prmComp
                           AND oe-prmtx.custype             EQ custype
                           AND oe-prmtx.cust-no             EQ prmCustomer
                           AND oe-prmtx.procat              EQ itemfg.procat
                           AND oe-prmtx.i-no                BEGINS itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                           AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                           BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                           LEAVE.
                           END.

                           IF sellpric-cha NE "LastPric" THEN DO:
                               IF NOT AVAIL oe-prmtx THEN
                                   FOR EACH oe-prmtx
                                   WHERE oe-prmtx.company = prmComp
                                   AND oe-prmtx.custype             EQ custype
                                   AND oe-prmtx.cust-no             EQ ""
                                   AND oe-prmtx.procat              EQ itemfg.procat
                                   AND oe-prmtx.i-no                BEGINS itemfg.i-no
                                   AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                                   AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                                   BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                                   LEAVE.
                                 END.

                            IF NOT AVAIL oe-prmtx THEN
                                FOR EACH oe-prmtx
                                WHERE oe-prmtx.company = prmComp
                                AND oe-prmtx.custype             EQ custype
                                AND oe-prmtx.cust-no             EQ ""
                                AND oe-prmtx.procat              EQ itemfg.procat
                                AND SUBSTR(oe-prmtx.i-no,01,100) EQ ""
                                AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                                BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                                LEAVE.
                              END.

                            IF NOT AVAIL oe-prmtx THEN
                                FOR EACH oe-prmtx
                                WHERE oe-prmtx.company = prmComp
                                AND oe-prmtx.custype             EQ ""
                                AND oe-prmtx.cust-no             EQ ""
                                AND oe-prmtx.procat              EQ itemfg.procat
                                AND oe-prmtx.i-no                BEGINS itemfg.i-no
                                AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                                AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                                BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                                LEAVE.
                             END.

                           IF NOT AVAIL oe-prmtx THEN
                               FOR EACH oe-prmtx
                               WHERE oe-prmtx.company = prmComp
                               AND oe-prmtx.custype             EQ ""
                               AND oe-prmtx.cust-no             EQ ""
                               AND oe-prmtx.procat              EQ ""
                               AND oe-prmtx.i-no                BEGINS itemfg.i-no
                               AND SUBSTR(oe-prmtx.i-no,01,100) EQ itemfg.i-no
                               AND SUBSTR(oe-prmtx.i-no,101,8)  LE lv-date
                               BY SUBSTR(oe-prmtx.i-no,101,8) DESC:
                               LEAVE.
                            END.
                         END.



                        IF  AVAIL oe-prmtx THEN DO:
                            li-pnt = 1.                            
                            DO WHILE  li-pnt LE 10: 
                                IF  oe-prmtx.qty[li-pnt] >= prmQty THEN DO:
                                    ASSIGN
                                    ttFGNameLook.vprice = oe-prmtx.price[li-pnt]
                                    ttFGNameLook.vuom            = oe-prmtx.uom[li-pnt] .
                                    
                                    IF AVAIL oe-prmtx  THEN  LEAVE.
                                    END.
                                    li-pnt = li-pnt + 1. 
                                 END.  
                           END.
                      
                       IF NOT AVAIL oe-prmtx  THEN DO:
                           ASSIGN
                               ttFGNameLook.vprice          = itemfg.sell-price
                               ttFGNameLook.vuom            = itemfg.sell-uom.
                      END.
                      /**** end of pricematrix  ****/

                      

                      IF cust.ACTIVE <> "X" THEN DO:
                        ttFGNameLook.vdiscount   =  cust.disc .
                      END.
                   FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = prmCustomer AND
                     (cust-part.i-no = itemfg.i-no )  NO-LOCK NO-ERROR.
                    IF AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = cust-part.part-no 
                            ttFGNameLook.vcustpart2 = cust-part.part-no.
                    END.  /* end do cust-part */
                    IF NOT AVAIL cust-part THEN DO:
                        ASSIGN
                            ttFGNameLook.vcustpart = itemfg.part-no 
                            ttFGNameLook.vcustpart2 = itemfg.part-no.
                    END.

    END.
        END.  /*FOR EACH Itemfg*/  
END.  /*ifif view itemfg <> "search" */


        END.  /* else do */
          
    
   FOR EACH ttFGNameLook NO-LOCK:
        IF INDEX(ttFGNameLook.vino ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vino  = REPLACE(ttFGNameLook.vino ,'"',":").
        IF INDEX(ttFGNameLook.viname ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.viname  = REPLACE(ttFGNameLook.viname ,'"',":").

        IF INDEX(ttFGNameLook.vdscr ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vdscr  = REPLACE(ttFGNameLook.vdscr ,'"',":").
        IF INDEX(ttFGNameLook.vpartdscr1 ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vpartdscr1  = REPLACE(ttFGNameLook.vpartdscr1 ,'"',":").
        IF INDEX(ttFGNameLook.vpartdscr2 ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vpartdscr2  = REPLACE(ttFGNameLook.vpartdscr2 ,'"',":").
        IF INDEX(ttFGNameLook.vcustpart ,'"',1) > 0 THEN ASSIGN
            ttFGNameLook.vcustpart  = REPLACE(ttFGNameLook.vcustpart ,'"',":").
   END. /* for each ttfgnamelook*/

           
END.  /* IF prmAction = search then do: */   


