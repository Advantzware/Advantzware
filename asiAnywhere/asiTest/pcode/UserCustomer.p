


/*------------------------------------------------------------------------
    File        : UserCust.p
    Purpose     : UserCust

    Syntax      :

    Description : Return a Dataset of user Maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserCust NO-UNDO 
    BEFORE-TABLE beforeUserCust
    FIELD Usr_id    as char
    FIELD usr_name  AS CHAR
    FIELD Comp      AS CHARACTER 
    FIELD Cust      AS CHARACTER FORMAT  "x(8)"
    FIELD CustName  AS CHARACTER FORMAT  "x(30)"
    FIELD DefCust   AS LOGICAL INITIAL TRUE.

DEFINE DATASET dsUserCustomer FOR ttUserCust .

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustName  AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmCustDef   AS CHARACTER  NO-UNDO.     


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserCustomer.

DEFINE BUFFER bf-UserCust FOR usercust.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmAction   = ? THEN ASSIGN prmAction = "Select".
IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmCustDef   = ?  THEN ASSIGN prmCustDef = "".

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

 
FOR EACH ttUserCust:
    DELETE ttUserCust.
END.
MESSAGE "surender" prmAction prmUser prmComp prmCust prmCustDef.
IF prmAction = "delete" THEN DO:
  FIND FIRST bf-UserCust WHERE bf-UserCust.user_id = prmUser 
                           AND bf-UserCust.company = prmComp 
                           AND  bf-UserCust.cust-no = prmCust EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL bf-UserCust  THEN DO:
      DELETE bf-UserCust.        
  END.  /*IF AVAIL bf_UserCust */
  

   ASSIGN prmAction = "Select".
END. /*IF prmAction = "delete"*/
IF prmAction = "Update" THEN DO:
   FIND FIRST bf-UserCust WHERE bf-UserCust.user_id = prmUser 
                            AND bf-UserCust.company = prmComp 
                            AND bf-UserCust.cust-no = prmCust EXCLUSIVE-LOCK NO-ERROR.
   IF  AVAILABLE bf-UserCust  THEN DO:
       ASSIGN
          
          bf-UserCust.cust_default =  IF prmCustDef = "yes" THEN  TRUE ELSE FALSE.

     RELEASE bf-UserCust.
   END.

   ASSIGN prmAction = "Select".

END.
IF prmAction = "AddCustomer" THEN DO:
   FIND FIRST bf-UserCust WHERE bf-UserCust.user_id = prmUser 
                            AND bf-UserCust.company = prmComp 
                            AND bf-UserCust.cust-no = prmCust NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bf-UserCust  THEN DO:
       CREATE bf-UserCust.
       ASSIGN
          bf-UserCust.user_id         =  prmUser
           bf-UserCust.company         =  prmComp
          bf-UserCust.cust-no         =  prmCust
          bf-UserCust.cust_default =  IF prmCustDef = "yes" THEN TRUE ELSE FALSE.
RELEASE bf-UserCust.
   END.
   ASSIGN prmAction = "Select".
END.


IF prmAction = "Search" THEN DO:
MESSAGE "search"prmAction prmUser prmComp prmCust prmCustName.
    FOR EACH usercust WHERE usercust.user_id = prmUser
                        AND usercust.company  = prmComp 
                        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK :
       FIND FIRST cust WHERE cust.company = usercust.company 
                         AND cust.cust-no = usercust.cust-no 
                         AND (cust.NAME = prmCustName OR cust.NAME BEGINS prmCustName OR prmCustName <> "")  NO-LOCK NO-ERROR.
       CREATE ttUserCust.
           assign
               ttUserCust.Usr_id      = usercust.user_id
               ttUserCust.Cust        = usercust.cust-no
               ttUserCust.CustName    = cust.NAME
               ttUserCust.DefCust     = usercust.cust_default .
    END.
END.

IF prmAction = "Select" THEN DO:
    
    MESSAGE "bajaj"  prmAction prmUser prmComp  .
    FOR EACH usercust WHERE usercust.user_id = prmUser AND usercust.company  = prmComp NO-LOCK :
       FIND FIRST cust WHERE cust.company = usercust.company AND cust.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
       RUN CreateCustomer.
   END.  

END.  /*IF prmAction = "Select"*/

IF prmAction = "View" THEN DO:
    FIND FIRST usercust WHERE usercust.user_id = prmUser AND usercust.company = prmComp AND usercust.cust-no = prmCust NO-LOCK NO-ERROR.
    IF AVAIL usercust THEN DO:
    Find FIRST cust WHERE cust.company = usercust.company AND cust.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
       RUN CreateCustomer.
   END.        
END.  /*IF prmAction = "View  "*/

PROCEDURE  CreateCustomer:
    CREATE ttUserCust.
    assign
        ttUserCust.Usr_id      = usercust.user_id
        ttUserCust.Cust        = usercust.cust-no
        ttUserCust.CustName    = cust.NAME
        ttUserCust.DefCust     = usercust.cust_default .
    MESSAGE "test1" ttUserCust.Cust  .
END PROCEDURE.



