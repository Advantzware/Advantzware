
/*------------------------------------------------------------------------
    File        : custlist.p
    Purpose     : Customer

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttcust2 NO-UNDO
    FIELD vc1 AS CHARACTER
    FIELD vcustno       LIKE cust.cust-no      FORMAT "X(8)"
    FIELD vcustname     LIKE cust.name         FORMAT "X(30)"
    FIELD vcity         LIKE cust.city         FORMAT "X(15)"
    FIELD vstate        LIKE cust.state        FORMAT "X(2)"
    FIELD vzip          LIKE cust.zip          FORMAT "X(10)"
    FIELD vtype         LIKE cust.type         FORMAT "X(8)"
    FIELD vsman         LIKE cust.sman         FORMAT "X(3)"
    FIELD vterr         LIKE cust.terr         FORMAT "X(3)"
    FIELD vreckey       LIKE cust.rec_key      FORMAT "X(20)"
    .

DEFINE DATASET dscust2 FOR ttcust2.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustname   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCity       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmState      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmZip        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmType       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSman       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTerr       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscust2.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
DEFINE BUFFER buff-cust FOR cust.

     FOR EACH ttcust2:
        DELETE ttcust2 .
    END.

     IF prmAction      = ?  THEN ASSIGN prmAction     = "Select".
     IF prmComp        = ?  THEN ASSIGN prmComp       = "".
     IF prmUser        = ?  THEN ASSIGN prmUser       = "".
     IF prmCustno      = ?  THEN ASSIGN prmCustno     = "".
     IF prmCustname    = ?  THEN ASSIGN prmCustname  = "".
     IF prmCity        = ?  THEN ASSIGN prmCity       = "".
     IF prmState       = ?  THEN ASSIGN prmState      = "".
     IF prmZip         = ?  THEN ASSIGN prmZip        = "".
     IF prmType        = ?  THEN ASSIGN prmType       = "".
     IF prmSman        = ?  THEN ASSIGN prmSman       = "".
     IF prmTerr        = ?  THEN ASSIGN prmTerr       = "".
     IF prmReckey      = ?  THEN ASSIGN prmReckey     = "".
    


    
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
         FOR EACH usercust WHERE usercust.user_id = prmUser AND usercust.company  = prmComp NO-LOCK :
         FOR EACH cust WHERE cust.company = usercust.company AND cust.cust-no = usercust.cust-no  
                        AND (cust.cust-no  =  prmCustno OR prmCustno = "") 
                        AND (cust.name  =  prmCustname OR prmCustname = "") AND (cust.city  =  prmCity OR prmCity = "") 
                        AND (cust.state  =  prmState OR prmState = "") AND (cust.zip  =  prmZip OR prmZip = "") 
                        AND (cust.type  =  prmType OR prmType = "") AND (cust.sman  =  prmSman OR prmSman = "") 
                        AND (cust.terr  =  prmTerr OR prmTerr = "") 
                        NO-LOCK:
             CREATE ttcust2.
             ASSIGN 
                 ttcust2.vcustno       = cust.cust-no 
                 ttcust2.vcustname      = cust.name 
                 ttcust2.vcity          = cust.city 
                 ttcust2.vstate         = cust.state   
                 ttcust2.vzip           = cust.zip
                 ttcust2.vtype          = cust.type  
                 ttcust2.vsman          = cust.sman
                 ttcust2.vterr          = cust.terr
                 ttcust2.vreckey        = cust.rec_key .
            

      END. /*FOR EACH buff-cust  */
         END.
        
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Select" THEN DO:
    FOR EACH usercust WHERE usercust.user_id = prmUser AND usercust.company  = prmComp NO-LOCK :
        FOR EACH cust WHERE cust.company = usercust.company AND cust.cust-no = usercust.cust-no  NO-LOCK :
        CREATE ttcust2.
           ASSIGN 
                 ttcust2.vcustno       = cust.cust-no 
                 ttcust2.vcustname      = cust.name 
                 ttcust2.vcity          = cust.city 
                 ttcust2.vstate         = cust.state   
                 ttcust2.vzip           = cust.zip
                 ttcust2.vtype          = cust.type  
                 ttcust2.vsman          = cust.sman
                 ttcust2.vterr          = cust.terr 
                 ttcust2.vreckey        = cust.rec_key .
            
    END. /*FOR EACH cust  */
    
    END.
END. /*IF prmAction = "Select" THEN DO:*/




