
/*------------------------------------------------------------------------
    File        : custsoldto.p
    Purpose     : Customer sold to

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE TEMP-TABLE ttcust1 NO-UNDO
    FIELD  vcustomer                  LIKE  soldto.cust-no
    FIELD  vsoldid                      LIKE  soldto.sold-id 
    FIELD  vsoldno                      LIKE  soldto.sold-no
    FIELD  vsoldname                    LIKE  soldto.sold-name              
    FIELD  vsoldcity                    LIKE  soldto.sold-city            
    FIELD  vsoldstate                   LIKE  soldto.sold-state       
    FIELD  vsoldzip                     LIKE  soldto.sold-zip  
    FIELD  vsoldaddr1                   LIKE  soldto.sold-addr[1]  
    FIELD  vsoldaddr2                   LIKE  soldto.sold-addr[2] 
    FIELD  vsoldreckey                  LIKE  soldto.rec_key                  

    
    .

DEFINE DATASET dscustsoldto FOR ttcust1.

    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer     AS CHARACTER  NO-UNDO. 
DEFINE INPUT PARAMETER prmsoldid         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsoldno         AS INTEGER    NO-UNDO. 
DEFINE INPUT PARAMETER prmsoldname       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsoldcity       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsoldstate      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsoldzip        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsoldaddr1      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsoldaddr2       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsoldreckey       AS CHARACTER  NO-UNDO.
  

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscustsoldto.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

DEFINE BUFFER buff-sold FOR soldto.
DEFINE BUFFER buff-ref FOR reftable.

     FOR EACH ttcust1:
        DELETE ttcust1.
    END.

     IF prmAction     = ?      THEN ASSIGN prmAction        = "Select".
     IF prmComp       = ?      THEN ASSIGN prmComp          = "".
     IF prmUser       = ?      THEN ASSIGN prmUser          = "".
     IF prmAction     = ?      THEN ASSIGN prmAction        = "".
     IF  prmsoldid      = ?      THEN ASSIGN  prmsoldid         = "".
     IF  prmsoldname    = ?      THEN ASSIGN  prmsoldname       = "".
     IF  prmsoldcity    = ?      THEN ASSIGN  prmsoldcity       = "".
     IF  prmsoldstate   = ?      THEN ASSIGN  prmsoldstate      = "".
     IF  prmsoldzip     = ?      THEN ASSIGN  prmsoldzip        = "".
     


     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.


     IF prmAction = "Search" THEN DO:
         FOR EACH buff-sold WHERE buff-sold.company = prmComp AND buff-sold.cust-no = prmCustomer AND buff-sold.sold-id = prmsoldid 
                        AND (buff-sold.sold-id   =  prmsoldid OR prmsoldid = "") 
                        AND (buff-sold.sold-name =  prmsoldname OR prmsoldname = "")  
                       
                        NO-LOCK:
             CREATE ttcust1.
             ASSIGN 
                 ttcust1.vsoldid           = buff-sold.sold-id    
                 ttcust1.vsoldname         = buff-sold.sold-name  
                 ttcust1.vsoldcity         = buff-sold.sold-city  
                 ttcust1.vsoldstate        = buff-sold.sold-state   
                 ttcust1.vsoldzip          = buff-sold.sold-zip   
                 ttcust1.vsoldreckey       = buff-sold.sold-zip
                  .
            

      END. /*FOR EACH buff-sold  */
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Select" THEN DO:
    FOR EACH buff-sold WHERE buff-sold.company = prmComp  AND buff-sold.cust-no = prmCustomer  
       NO-LOCK:
        CREATE ttcust1.
           ASSIGN 
                 ttcust1.vcustomer       = buff-sold.cust-no 
                 ttcust1.vsoldid           = buff-sold.sold-id
                 ttcust1.vsoldno           = buff-sold.sold-no
                 ttcust1.vsoldname         = buff-sold.sold-name    
                 ttcust1.vsoldcity         = buff-sold.sold-city    
                 ttcust1.vsoldstate        = buff-sold.sold-state      
                 ttcust1.vsoldzip          = buff-sold.sold-zip  
                 ttcust1.vsoldreckey       = buff-sold.sold-zip
                  .
            
            
    END. /*FOR EACH buff-sold  */
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "Update" THEN DO:
    FIND FIRST zipcode WHERE zipcode.city = prmsoldcity NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid City".
        RETURN. 
        END.
        FIND FIRST statecod WHERE statecod.statecod =  prmsoldstate  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE statecod THEN DO:
            ASSIGN cError  = "Invalid State".
            RETURN. 
            END.
            FIND FIRST zipcode WHERE zipcode.zipcode = prmsoldzip  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE zipcode THEN DO:
                ASSIGN cError  = "Invalid zip".
                RETURN. 
                END.
                END.


IF prmAction = "Update" THEN DO:
    FIND FIRST soldto WHERE soldto.sold-id = prmsoldid   AND soldto.company = prmComp AND soldto.cust-no = prmCustomer EXCLUSIVE-LOCK NO-ERROR.
       IF  AVAILABLE soldto  THEN DO:
             ASSIGN

                  soldto.sold-name      =   prmsoldname         
                  soldto.sold-city      =   prmsoldcity         
                  soldto.sold-state     =   prmsoldstate        
                  soldto.sold-zip       =   prmsoldzip
                  soldto.sold-addr[1]   =   prmsoldaddr1         
                  soldto.sold-addr[2]   =   prmsoldaddr2 .

        
   
      ASSIGN prmAction = "View".
      END.
     

      MESSAGE prmAction .
   END. /*IF prmAction = "Update" THEN DO:*/



IF prmAction = "Add" THEN DO:
 FIND FIRST zipcode WHERE zipcode.city = prmsoldcity NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid City".
        RETURN. 
        END.
        FIND FIRST statecod WHERE statecod.statecod =  prmsoldstate  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE statecod THEN DO:
            ASSIGN cError  = "Invalid State".
            RETURN. 
            END.
            FIND FIRST zipcode WHERE zipcode.zipcode = prmsoldzip  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE zipcode THEN DO:
                ASSIGN cError  = "Invalid zip".
                RETURN. 
                END.
END.



IF prmAction = "Add" THEN DO:
   
    FIND FIRST soldto  WHERE soldto.sold-id  = prmsoldid  AND soldto.company = prmComp AND soldto.cust-no = prmCustomer NO-LOCK NO-ERROR .
    
    IF  AVAILABLE soldto THEN DO:
        ASSIGN cError = "Sold To ID  Already Exists, Please Enter a Different Sold To ID".
            RETURN.
                
    END.
   /* IF NOT AVAILABLE soldto THEN DO: */
         FIND LAST soldto WHERE soldto.cust-no = prmCustomer  NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN DO:
        ASSIGN    prmsoldno = soldto.sold-no + 1.
    END.
    ELSE DO:
        ASSIGN  prmsoldno = 1.
    END. 

      
        CREATE soldto .
           
          ASSIGN
                  soldto.company        =   prmComp
                  soldto.cust-no        =   prmCustomer
                  soldto.sold-id        =   prmsoldid 
                  soldto.sold-no        =   prmsoldno
                  soldto.sold-name      =   prmsoldname         
                  soldto.sold-city      =   prmsoldcity         
                  soldto.sold-state     =   prmsoldstate        
                  soldto.sold-zip       =   prmsoldzip 
                  soldto.sold-addr[1]   =   prmsoldaddr1         
                  soldto.sold-addr[2]   =   prmsoldaddr2 
                        .
            
            

            

        ASSIGN prmAction = "View".
        MESSAGE "sandeep" prmAction prmComp prmCustomer prmsoldid prmsoldno prmsoldname .   

END.  /* end of add */

    




IF prmAction = "Delete" THEN DO:
    FIND soldto WHERE soldto.sold-id = prmsoldid   AND soldto.company = prmComp AND soldto.cust-no = prmCustomer  EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE soldto  THEN DO:

        DELETE soldto.
        
    END.
    FIND LAST soldto WHERE soldto.cust-no = prmCustomer NO-LOCK NO-ERROR.
    ASSIGN prmsoldid = soldto.sold-id. 
       ASSIGN prmAction = "View".
          MESSAGE "sandeep" prmAction prmsoldid.
 END. /*IF prmAction = "Delete" THEN DO:*/   




 IF prmAction = "View"  THEN DO:
     FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.sold-id = prmsoldid    AND soldto.cust-no = prmCustomer 
         NO-LOCK NO-ERROR.
     IF AVAILABLE soldto THEN DO:

         CREATE  ttcust1.
             ASSIGN 
                  ttcust1.vsoldid          = soldto.sold-id    
                  ttcust1.vsoldname        = soldto.sold-name  
                  ttcust1.vsoldcity        = soldto.sold-city  
                  ttcust1.vsoldstate       = soldto.sold-state  
                 ttcust1.vsoldzip         = soldto.sold-zip   

                 ttcust1.vsoldaddr1           = soldto.sold-addr[1]  
                 ttcust1.vsoldaddr2           = soldto.sold-addr[2]  .
                                 
                  
                                                                                    
    END.
MESSAGE  "praveen" prmAction .
END. /*IF prmAction = "View" THEN DO:*/

