
/*------------------------------------------------------------------------
    File        : salestax_list.p
    Purpose     : Sales Tax Code
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttSalesTaxCodeList NO-UNDO
    FIELD taxgrp      AS CHAR
    FIELD code1       AS CHAR
    FIELD dscr1       AS CHAR
    FIELD rate1       AS DEC
    FIELD accl        AS CHAR
    FIELD frt1        AS CHAR
    FIELD code2       AS CHAR
    FIELD dscr2       AS CHAR
    FIELD rate2       AS DEC
    FIELD acc2        AS CHAR
    FIELD frt2        AS CHAR
    FIELD code3       AS CHAR  
    FIELD dscr3       AS CHAR 
    FIELD rate3       AS DEC  
    FIELD acc3        AS CHAR 
    FIELD frt3        AS CHAR 
    FIELD code4       AS CHAR 
    FIELD dscr4       AS CHAR 
    FIELD rate4       AS DEC  
    FIELD acc4        AS CHAR 
    FIELD frt4        AS CHAR 
    FIELD code5       AS CHAR 
    FIELD dscr5       AS CHAR 
    FIELD rate5       AS DEC  
    FIELD acc5        AS CHAR 
    FIELD frt5        AS CHAR
    FIELD tax         AS CHAR
    FIELD reckey      AS CHAR
    FIELD extra       AS CHAR 
    .

DEFINE DATASET dsSalesTaxCodeList FOR ttSalesTaxCodeList.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmtaxgrp   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcode1    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmdscr1    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmrate1    AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmaccl     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmfrt1     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcode2    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmdscr2    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmrate2    AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmacc2     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmfrt2     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcode3    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmdscr3    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmrate3    AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmacc3     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmfrt3     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcode4    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmdscr4    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmrate4    AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmacc4     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmfrt4     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcode5    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmdscr5    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmrate5    AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmacc5     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmfrt5     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmtax      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey   AS CHAR  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSalesTaxCodeList .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction        = ?  THEN ASSIGN prmAction    = "Select".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmtaxgrp        = ?  THEN ASSIGN prmtaxgrp    = "".
IF prmcode1         = ?  THEN ASSIGN prmcode1     = "".
IF prmdscr1         = ?  THEN ASSIGN prmdscr1     = "". 
IF prmrate1         = ?  THEN ASSIGN prmrate1     = 0. 
IF prmaccl          = ?  THEN ASSIGN prmaccl      = "".
IF prmfrt1          = ?  THEN ASSIGN prmfrt1      = "".
IF prmcode2         = ?  THEN ASSIGN prmcode2     = "".
IF prmdscr2         = ?  THEN ASSIGN prmdscr2     = "".
IF prmrate2         = ?  THEN ASSIGN prmrate2     = 0.
IF prmacc2          = ?  THEN ASSIGN prmacc2      = "".
IF prmfrt2          = ?  THEN ASSIGN prmfrt2      = "".
IF prmcode3         = ?  THEN ASSIGN prmcode3     = "".
IF prmdscr3         = ?  THEN ASSIGN prmdscr3     = "".
IF prmrate3         = ?  THEN ASSIGN prmrate3     = 0.
IF prmacc3          = ?  THEN ASSIGN prmacc3      = "".
IF prmfrt3          = ?  THEN ASSIGN prmfrt3      = "".
IF prmcode4         = ?  THEN ASSIGN prmcode4     = "".
IF prmdscr4         = ?  THEN ASSIGN prmdscr4     = "".
IF prmrate4         = ?  THEN ASSIGN prmrate4     = 0.
IF prmacc4          = ?  THEN ASSIGN prmacc4      = "". 
IF prmfrt4          = ?  THEN ASSIGN prmfrt4      = "".
IF prmcode5         = ?  THEN ASSIGN prmcode5     = "".
IF prmdscr5         = ?  THEN ASSIGN prmdscr5     = "".
IF prmrate5         = ?  THEN ASSIGN prmrate5     = 0.
IF prmacc5          = ?  THEN ASSIGN prmacc5      = "".
IF prmfrt5          = ?  THEN ASSIGN prmfrt5      = "".
IF prmtax           = ?  THEN ASSIGN prmtax       = "".
                                                  


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
         
     
 IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  g_company = prmComp
           cocode  = prmComp .

DEF BUFFER b-stax FOR stax.
DEFINE TEMP-TABLE tt-accounts NO-UNDO
    FIELD i-extent AS INTEGER
    FIELD c-code   AS CHARACTER
    FIELD c-acct   AS CHARACTER
    FIELD h-acct   AS HANDLE
    INDEX KEY AS PRIMARY UNIQUE i-extent.

 

IF prmAction = "GridSelect" THEN DO:
    FOR EACH stax WHERE stax.company = prmComp NO-LOCK: 
    
        CREATE ttSalesTaxCodeList.
        ASSIGN
            ttSalesTaxCodeList.taxgrp = stax.tax-group
            ttSalesTaxCodeList.code1  = stax.tax-code1[1]
            ttSalesTaxCodeList.dscr1  = stax.tax-dscr1[1]       
            ttSalesTaxCodeList.rate1  = stax.tax-rate1[1]      
            ttSalesTaxCodeList.accl   = stax.tax-acc1[1]   
            ttSalesTaxCodeList.frt1   = string(stax.tax-frt1[1])    
            ttSalesTaxCodeList.code2  = stax.tax-code1[2]       
            ttSalesTaxCodeList.dscr2  = stax.tax-dscr1[2]   
            ttSalesTaxCodeList.rate2  = stax.tax-rate1[2]   
            ttSalesTaxCodeList.acc2   = stax.tax-acc1[2]    
            ttSalesTaxCodeList.frt2   = string(stax.tax-frt1[2])         
            ttSalesTaxCodeList.code3  = stax.tax-code1[3]   
            ttSalesTaxCodeList.dscr3  = stax.tax-dscr1[3]   
            ttSalesTaxCodeList.rate3  = stax.tax-rate1[3]    
            ttSalesTaxCodeList.acc3   = stax.tax-acc1[3]         
            ttSalesTaxCodeList.frt3   = STRING(stax.tax-frt1[3])     
            ttSalesTaxCodeList.code4  = stax.tax-code1[4]    
            ttSalesTaxCodeList.dscr4  = stax.tax-dscr1[4]    
            ttSalesTaxCodeList.rate4  = stax.tax-rate1[4]        
            ttSalesTaxCodeList.acc4   = stax.tax-acc1[4]    
            ttSalesTaxCodeList.frt4   = string(stax.tax-frt1[4])    
            ttSalesTaxCodeList.code5  = stax.tax-code1[5]   
            ttSalesTaxCodeList.dscr5  = stax.tax-dscr1[5]        
            ttSalesTaxCodeList.rate5  = stax.tax-rate1[5]    
            ttSalesTaxCodeList.acc5   = stax.tax-acc1[5]    
            ttSalesTaxCodeList.frt5   = string(stax.tax-frt1[5])  
            ttSalesTaxCodeList.reckey = stax.rec_key  . 
    END.
                                        
                                        
     
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "GridSearch" THEN DO:
    FOR EACH stax WHERE stax.company = prmComp AND (stax.tax-group BEGINS prmtaxgrp OR prmtaxgrp = "") NO-LOCK: 
    
        CREATE ttSalesTaxCodeList.
        ASSIGN
            ttSalesTaxCodeList.taxgrp = stax.tax-group
            ttSalesTaxCodeList.code1  = stax.tax-code1[1]
            ttSalesTaxCodeList.dscr1  = stax.tax-dscr1[1]       
            ttSalesTaxCodeList.rate1  = stax.tax-rate1[1]      
            ttSalesTaxCodeList.accl   = stax.tax-acc1[1]   
            ttSalesTaxCodeList.frt1   = string(stax.tax-frt1[1])    
            ttSalesTaxCodeList.code2  = stax.tax-code1[2]       
            ttSalesTaxCodeList.dscr2  = stax.tax-dscr1[2]   
            ttSalesTaxCodeList.rate2  = stax.tax-rate1[2]   
            ttSalesTaxCodeList.acc2   = stax.tax-acc1[2]    
            ttSalesTaxCodeList.frt2   = string(stax.tax-frt1[2])         
            ttSalesTaxCodeList.code3  = stax.tax-code1[3]   
            ttSalesTaxCodeList.dscr3  = stax.tax-dscr1[3]   
            ttSalesTaxCodeList.rate3  = stax.tax-rate1[3]    
            ttSalesTaxCodeList.acc3   = stax.tax-acc1[3]         
            ttSalesTaxCodeList.frt3   = string(stax.tax-frt1[3])     
            ttSalesTaxCodeList.code4  = stax.tax-code1[4]    
            ttSalesTaxCodeList.dscr4  = stax.tax-dscr1[4]    
            ttSalesTaxCodeList.rate4  = stax.tax-rate1[4]        
            ttSalesTaxCodeList.acc4   = stax.tax-acc1[4]    
            ttSalesTaxCodeList.frt4   = string(stax.tax-frt1[4])    
            ttSalesTaxCodeList.code5  = stax.tax-code1[5]   
            ttSalesTaxCodeList.dscr5  = stax.tax-dscr1[5]        
            ttSalesTaxCodeList.rate5  = stax.tax-rate1[5]    
            ttSalesTaxCodeList.acc5   = stax.tax-acc1[5]    
            ttSalesTaxCodeList.frt5   = string(stax.tax-frt1[5]) 
            ttSalesTaxCodeList.reckey = stax.rec_key  . 
    END.

 END.

 IF prmAction = "Update" THEN DO:
     FIND FIRST stax WHERE stax.company = prmComp AND stax.rec_key = prmReckey NO-LOCK NO-ERROR. 

     
     prmtaxgrp = CAPS(prmtaxgrp).
    
    FOR EACH b-stax NO-LOCK
       WHERE b-stax.company EQ cocode
         AND b-stax.tax-group EQ prmtaxgrp :
        IF ROWID(b-stax) <> ROWID(stax)
        OR (NEW stax AND ROWID(stax) = ?) THEN DO:
           ASSIGN cError = "Sorry, Tax Group already exists..." .
            RETURN .
        END.
    END.

   

    IF prmcode1 <> "" THEN DO:
        
        prmcode1 = CAPS(prmcode1).

        IF  prmcode1 NE "" 
            AND prmcode1 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode1
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 1, try help..." .
            RETURN.
        END.
    END.

    IF prmcode2 <> "" THEN DO:
        
        prmcode2 = CAPS(prmcode2).

        IF  prmcode2 NE "" 
            AND prmcode2 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode2
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 2, try help..." .
            RETURN.
        END.
    END.

    IF prmcode3 <> "" THEN DO:
        
        prmcode3 = CAPS(prmcode3).

        IF  prmcode3 NE "" 
            AND prmcode3 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode3
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 3, try help..." .
            RETURN.
        END.
    END.

    IF prmcode4 <> "" THEN DO:
        
        prmcode4 = CAPS(prmcode4).

        IF  prmcode4 NE "" 
            AND prmcode4 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode4
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 4, try help..." .
            RETURN.
        END.
    END.

    IF prmcode5 <> "" THEN DO:
        
        prmcode5 = CAPS(prmcode5).

        IF  prmcode5 NE "" 
            AND prmcode5 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode5
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 5, try help..." .
            RETURN.
        END.
    END.

    if prmcode1 <> "" then do:
         IF prmaccl = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmaccl)
      then do:
         cError = "Invalid Account 1. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode2 <> "" then do:
         IF prmacc2 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc2)
      then do:
          cError = "Invalid Account 2. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode3 <> "" then do:
         IF prmacc3 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc3)
      then do:
         cError = "Invalid Account 3. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode4 <> "" then do:
         IF prmacc4 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc4)
      then do:
          cError = "Invalid Account 4. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode5 <> "" then do:
          IF prmacc5 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc5)
      then do:
         cError = "Invalid Account 5. Account Type must not be T. " .
         return .
      end.
     end.



 END. /* validate update */

 IF prmAction = "Update" THEN DO:
     IF prmcode1 = "" THEN
         prmaccl = "".
      IF prmcode2 = "" THEN
         prmacc2 = "".
       IF prmcode3 = "" THEN
         prmacc3 = "".
        IF prmcode4 = "" THEN
         prmacc4 = "".
         IF prmcode5 = "" THEN
         prmacc5 = "".
     FIND FIRST stax WHERE stax.company = prmComp AND stax.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL stax THEN
     ASSIGN
            stax.tax-code1[1]   = prmcode1 
            stax.tax-dscr1[1]   = prmdscr1 
            stax.tax-rate1[1]   = prmrate1 
            stax.tax-acc1[1]    = prmaccl  
            stax.tax-frt1[1]    = IF prmfrt1 = "True" THEN YES ELSE NO  
            stax.tax-code1[2]   = prmcode2 
            stax.tax-dscr1[2]   = prmdscr2 
            stax.tax-rate1[2]   = prmrate2 
            stax.tax-acc1[2]    = prmacc2  
            stax.tax-frt1[2]    = IF prmfrt2 = "True" THEN YES ELSE NO   
            stax.tax-code1[3]   = prmcode3 
            stax.tax-dscr1[3]   = prmdscr3 
            stax.tax-rate1[3]   = prmrate3 
            stax.tax-acc1[3]    = prmacc3  
            stax.tax-frt1[3]    = IF prmfrt3 = "True" THEN YES ELSE NO   
            stax.tax-code1[4]   = prmcode4 
            stax.tax-dscr1[4]   = prmdscr4 
            stax.tax-rate1[4]   = prmrate4 
            stax.tax-acc1[4]    = prmacc4  
            stax.tax-frt1[4]    = IF prmfrt4 = "True" THEN YES ELSE NO   
            stax.tax-code1[5]   = prmcode5 
            stax.tax-dscr1[5]   = prmdscr5 
            stax.tax-rate1[5]   = prmrate5 
            stax.tax-acc1[5]    = prmacc5  
            stax.tax-frt1[5]    = IF prmfrt5 = "True" THEN YES ELSE NO 
            stax.accum-tax      = IF prmtax = "True" THEN YES ELSE NO  . 
            ASSIGN
                prmAction = "View" .


 END.

 IF prmAction = "Add" THEN DO:
     
     prmtaxgrp = CAPS(prmtaxgrp).
    
    FOR EACH b-stax NO-LOCK
       WHERE b-stax.company EQ cocode
         AND b-stax.tax-group EQ prmtaxgrp:
        IF ROWID(b-stax) <> ROWID(stax)
        OR (NEW stax AND ROWID(stax) = ?) THEN DO:
           ASSIGN cError = "Sorry, Tax Group already exists..." .
            RETURN .
        END.
    END.

    

    IF prmcode1 <> "" THEN DO:
        
        prmcode1 = CAPS(prmcode1).

        IF  prmcode1 NE "" 
            AND prmcode1 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode1
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 1, try help..." .
            RETURN.
        END.
    END.

    IF prmcode2 <> "" THEN DO:
        
        prmcode2 = CAPS(prmcode2).

        IF  prmcode2 NE "" 
            AND prmcode2 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode2
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 2, try help..." .
            RETURN.
        END.
    END.

    IF prmcode3 <> "" THEN DO:
        
        prmcode3 = CAPS(prmcode3).

        IF  prmcode3 NE "" 
            AND prmcode3 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode3
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 3, try help..." .
            RETURN.
        END.
    END.

    IF prmcode4 <> "" THEN DO:
        
        prmcode4 = CAPS(prmcode4).

        IF  prmcode4 NE "" 
            AND prmcode4 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode4
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 4, try help..." .
            RETURN.
        END.
    END.

    IF prmcode5 <> "" THEN DO:
        
        prmcode5 = CAPS(prmcode5).

        IF  prmcode5 NE "" 
            AND prmcode5 NE prmtaxgrp
            AND NOT CAN-FIND(FIRST b-stax
                             WHERE b-stax.company   EQ cocode
                             AND b-stax.tax-group EQ prmcode5
                             AND ROWID(b-stax)    NE ROWID(stax))          
            THEN DO:
            cError = "Invalid Tax Code 5, try help..." .
            RETURN.
        END.
    END.

    if prmcode1 <> "" then do:
         IF prmaccl = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmaccl)
      then do:
         cError = "Invalid Account 1. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode2 <> "" then do:
         IF prmacc2 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc2)
      then do:
          cError = "Invalid Account 2. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode3 <> "" then do:
         IF prmacc3 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc3)
      then do:
         cError = "Invalid Account 3. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode4 <> "" then do:
         IF prmacc4 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc4)
      then do:
          cError = "Invalid Account 4. Account Type must not be T. " .
         return .
      end.
     end.
     if prmcode5 <> "" then do:
          IF prmacc5 = "" THEN DO:
             cError = "Invalid Account, try help".
             RETURN.
         END.
     if not can-find(first account where account.company = cocode and
                                         account.type <> "T" and
                                         account.actnum BEGINS prmacc5)
      then do:
         cError = "Invalid Account 5. Account Type must not be T. " .
         return .
      end.
     end.

 END. /* validate Add*/

 IF prmAction = "Add" THEN DO:

     CREATE stax.

     ASSIGN
            stax.company         = prmComp 
            stax.tax-group      = prmtaxgrp
            stax.tax-code1[1]   = prmcode1 
            stax.tax-dscr1[1]   = prmdscr1 
            stax.tax-rate1[1]   = prmrate1 
            stax.tax-acc1[1]    = prmaccl  
            stax.tax-frt1[1]    = IF prmfrt1 = "True" THEN YES ELSE NO  
            stax.tax-code1[2]   = prmcode2 
            stax.tax-dscr1[2]   = prmdscr2 
            stax.tax-rate1[2]   = prmrate2 
            stax.tax-acc1[2]    = prmacc2  
            stax.tax-frt1[2]    = IF prmfrt2 = "True" THEN YES ELSE NO   
            stax.tax-code1[3]   = prmcode3 
            stax.tax-dscr1[3]   = prmdscr3 
            stax.tax-rate1[3]   = prmrate3 
            stax.tax-acc1[3]    = prmacc3  
            stax.tax-frt1[3]    = IF prmfrt3 = "True" THEN YES ELSE NO   
            stax.tax-code1[4]   = prmcode4 
            stax.tax-dscr1[4]   = prmdscr4 
            stax.tax-rate1[4]   = prmrate4 
            stax.tax-acc1[4]    = prmacc4  
            stax.tax-frt1[4]    = IF prmfrt4 = "True" THEN YES ELSE NO   
            stax.tax-code1[5]   = prmcode5 
            stax.tax-dscr1[5]   = prmdscr5 
            stax.tax-rate1[5]   = prmrate5 
            stax.tax-acc1[5]    = prmacc5  
            stax.tax-frt1[5]    = IF prmfrt5 = "True" THEN YES ELSE NO 
            stax.accum-tax      = IF prmtax = "True" THEN YES ELSE NO  .

            ASSIGN
                prmReckey = stax.rec_key
                prmAction = "View" .

 END.

 IF prmAction = "Delete" THEN DO:
     FIND FIRST stax WHERE stax.company = prmComp 
         AND stax.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL stax THEN
         DELETE stax.

     FIND LAST stax WHERE stax.company = prmComp NO-LOCK NO-ERROR.
     IF AVAIL stax THEN
         ASSIGN
         prmAction = "View"
         prmReckey = stax.rec_key.

 END.

 IF prmAction = "View" THEN DO:
     FIND FIRST stax WHERE stax.company = prmComp 
         AND stax.rec_key = prmReckey NO-LOCK NO-ERROR.

     CREATE ttSalesTaxCodeList.
        ASSIGN
           ttSalesTaxCodeList.taxgrp = stax.tax-group
            ttSalesTaxCodeList.code1  = stax.tax-code1[1]
            ttSalesTaxCodeList.dscr1  = stax.tax-dscr1[1]       
            ttSalesTaxCodeList.rate1  = stax.tax-rate1[1]      
            ttSalesTaxCodeList.accl   = stax.tax-acc1[1]   
            ttSalesTaxCodeList.frt1   = string(stax.tax-frt1[1])    
            ttSalesTaxCodeList.code2  = stax.tax-code1[2]       
            ttSalesTaxCodeList.dscr2  = stax.tax-dscr1[2]   
            ttSalesTaxCodeList.rate2  = stax.tax-rate1[2]   
            ttSalesTaxCodeList.acc2   = stax.tax-acc1[2]    
            ttSalesTaxCodeList.frt2   = string(stax.tax-frt1[2])         
            ttSalesTaxCodeList.code3  = stax.tax-code1[3]   
            ttSalesTaxCodeList.dscr3  = stax.tax-dscr1[3]   
            ttSalesTaxCodeList.rate3  = stax.tax-rate1[3]    
            ttSalesTaxCodeList.acc3   = stax.tax-acc1[3]         
            ttSalesTaxCodeList.frt3   = STRING(stax.tax-frt1[3])     
            ttSalesTaxCodeList.code4  = stax.tax-code1[4]    
            ttSalesTaxCodeList.dscr4  = stax.tax-dscr1[4]    
            ttSalesTaxCodeList.rate4  = stax.tax-rate1[4]        
            ttSalesTaxCodeList.acc4   = stax.tax-acc1[4]    
            ttSalesTaxCodeList.frt4   = string(stax.tax-frt1[4])    
            ttSalesTaxCodeList.code5  = stax.tax-code1[5]   
            ttSalesTaxCodeList.dscr5  = stax.tax-dscr1[5]        
            ttSalesTaxCodeList.rate5  = stax.tax-rate1[5]    
            ttSalesTaxCodeList.acc5   = stax.tax-acc1[5]    
            ttSalesTaxCodeList.frt5   = string(stax.tax-frt1[5]) 
            ttSalesTaxCodeList.reckey = stax.rec_key  . 


 END.


