/*------------------------------------------------------------------------
    File        : VendorCrossReference.p
    Purpose     : Vendor Cross Reference
    Syntax      :

    Description : Return a Dataset of Vendor Cross Reference
    Author(s)   : 
    Created     : 25 Dec 2008 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVendorCrossReference NO-UNDO
        FIELD vCustNo            AS CHAR 
        FIELD vVendorCode        AS CHAR
        FIELD vCustName          AS CHAR
        FIELD vCompany           AS CHAR
        .
DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.
DEFINE DATASET dsVendorCrossReference FOR ttVendorCrossReference .

DEFINE INPUT PARAMETER prmUser          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustNo        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmVendorCode    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCustName      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmUpdateCustno  AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError          AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorCrossReference.

IF prmUser          = ? THEN ASSIGN prmUser = "".
IF prmAction        = ? THEN ASSIGN prmAction = "Select".
IF prmCustNo        = ? THEN ASSIGN prmCustNo     = "".
IF prmVendorCode    = ? THEN ASSIGN prmVendorCode = "".
IF prmCustName      = ? THEN ASSIGN prmCustName = "".
IF prmComp          = ? THEN ASSIGN prmComp = "".

DEF VAR v-usercust AS LOG NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

IF prmAction = "Select" THEN DO:
      MAIN-LOOP:
    FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp NO-LOCK:
         IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
            create ttVendorCrossReference.
            assign
                ttVendorCrossReference.vCustNo              = vend-code-cust-xref.cust-no
                ttVendorCrossReference.vVendorCode          = vend-code-cust-xref.vendor-code
                ttVendorCrossReference.vCustName            = vend-code-cust-xref.cust-name
                ttVendorCrossReference.vCompany             = vend-code-cust-xref.company

              .
       END. /*FOR EACH vend-code-cust-xref*/
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/


IF prmAction = "Search" THEN DO:
     MAIN-LOOP:
    FOR EACH vend-code-cust-xref WHERE (vend-code-cust-xref.cust-no BEGINS prmCustNo or prmCustNo = "" )                           
                           AND (vend-code-cust-xref.vendor-code BEGINS prmVendorCode OR prmVendorCode = "" )
                           AND (vend-code-cust-xref.cust-name  BEGINS prmCustName OR prmCustName = "" )
                           AND  vend-code-cust-xref.company = prmComp   NO-LOCK:
        IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
          CREATE ttVendorCrossReference.
            assign
                ttVendorCrossReference.vCustNo              = vend-code-cust-xref.cust-no
                ttVendorCrossReference.vVendorCode          = vend-code-cust-xref.vendor-code
                ttVendorCrossReference.vCustName            = vend-code-cust-xref.cust-name
                ttVendorCrossReference.vCompany             = vend-code-cust-xref.company
              .
    END. /*FOR EACH vend-code-cust-xref*/
END. /* IF prmAction = "Search" THEN DO: */

/*************************************************/ 




IF prmAction = "Add" THEN DO:
      
     IF CAN-FIND(FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = prmComp
                                             AND b-vend-code-cust-xref.cust-no = prmCustNo
                                             AND ROWID(b-vend-code-cust-xref) <> ROWID(vend-code-cust-xref)) THEN DO:
      cError =  "Suppliers A/R Code already exists    " .
      RETURN.
   END.

    FIND FIRST cust WHERE cust.company = prmComp
                                AND cust.cust-no = prmCustNo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cust THEN DO:  
          ASSIGN cError  = "Invalid Suppliers A/R Code     ".
          RETURN.
    END.

     IF CAN-FIND(FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = prmComp
                                             AND b-vend-code-cust-xref.vendor-code = prmVendorCode 
                                             AND ROWID(b-vend-code-cust-xref) <> ROWID(vend-code-cust-xref)) THEN DO:
         ASSIGN
             cError = "Customers A/P Code already exists    " .
         RETURN.
   END.

    CREATE vend-code-cust-xref.
        assign
            vend-code-cust-xref.cust-no          = prmCustNo
            vend-code-cust-xref.vendor-code      = prmVendorCode
            vend-code-cust-xref.cust-name        = prmCustName
            vend-code-cust-xref.company          = prmComp
              .   
            ASSIGN 
                prmAction = "View".
END. /* IF prmAction = "Add" THEN DO: */


IF prmAction = "Update" THEN DO:

    FIND  FIRST vend-code-cust-xref WHERE vend-code-cust-xref.company = prmComp 
                            AND vend-code-cust-xref.cust-no = prmUpdateCustno  NO-LOCK NO-ERROR.

     IF CAN-FIND(FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = prmComp
                                             AND b-vend-code-cust-xref.cust-no = prmCustNo
                                             AND ROWID(b-vend-code-cust-xref) <> ROWID(vend-code-cust-xref)) THEN DO:
      cError =  "Suppliers A/R Code already exists    " .
      RETURN.
   END.

    FIND FIRST cust WHERE cust.company = prmComp
                                AND cust.cust-no = prmCustNo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cust THEN DO:  
          ASSIGN cError  = "Invalid Suppliers A/R Code     ".
          RETURN.
    END.

 
     IF CAN-FIND(FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = prmComp
                                             AND b-vend-code-cust-xref.vendor-code = prmVendorCode 
                                             AND ROWID(b-vend-code-cust-xref) <> ROWID(vend-code-cust-xref)) THEN DO:
         ASSIGN
             cError = "Customers A/P Code already exists    " .
         RETURN.
   END.


    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.cust-no EQ prmUpdateCustno   
                           AND  vend-code-cust-xref.company = prmComp  EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL vend-code-cust-xref THEN          
            assign
                vend-code-cust-xref.cust-no          = prmCustNo
                vend-code-cust-xref.vendor-code      = prmVendorCode
                vend-code-cust-xref.cust-name        = prmCustName
                vend-code-cust-xref.company          = prmComp
              .
        ASSIGN 
            prmAction = "View"
            prmUpdateCustno  = prmCustNo .

END. /* IF prmAction = "Update" THEN DO: */

IF prmAction = "Delete" THEN DO:
 FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.cust-no EQ prmCustNo   
                           AND  vend-code-cust-xref.company = prmComp  EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL vend-code-cust-xref THEN
           DELETE vend-code-cust-xref.
        FOR EACH usercust WHERE usercust.user_id = prmUser AND  usercust.company = prmComp  NO-LOCK:
        FIND LAST vend-code-cust-xref WHERE vend-code-cust-xref.company = prmComp AND vend-code-cust-xref.cust-no = usercust.cust-no NO-LOCK NO-ERROR .
        IF AVAIL vend-code-cust-xref THEN DO:
            ASSIGN 
            prmCustNo =  vend-code-cust-xref.cust-no
            prmAction =  "View" .
        END.
        END.
END. /* IF prmAction = "Delete" THEN DO: */


IF prmAction = "View" THEN DO:
    FIND FIRST vend-code-cust-xref WHERE vend-code-cust-xref.cust-no EQ prmCustNo   
                           AND  vend-code-cust-xref.company = prmComp   NO-LOCK NO-ERROR.
          CREATE ttVendorCrossReference.
            assign
                ttVendorCrossReference.vCustNo              = vend-code-cust-xref.cust-no
                ttVendorCrossReference.vVendorCode          = vend-code-cust-xref.vendor-code
                ttVendorCrossReference.vCustName            = vend-code-cust-xref.cust-name
                ttVendorCrossReference.vCompany             = vend-code-cust-xref.company
              .    
END. /* IF prmAction = "View" THEN DO: */

