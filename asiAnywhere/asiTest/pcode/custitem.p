



/*------------------------------------------------------------------------
    File        : CustItem.p
    Purpose     : Customer Item

    Syntax      :

    Description : Return a Dataset of all Customer Item
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttbrwscustitem  NO-UNDO
    FIELD  vCustno   AS CHARACTER FORMAT "x(8)"     
    FIELD  vLoc      AS CHARACTER FORMAT "x(15)"     
    FIELD  vDscr     AS CHARACTER FORMAT "x(15)"     
    FIELD  vItem     AS CHARACTER FORMAT "x(15)"    
    FIELD  vQty      AS DECIMAL   FORMAT "->,>>>,>>9"
    FIELD  vConsum   AS DECIMAL   FORMAT "->>>,>>>,>>9"
    FIELD  vreckey   AS CHAR       FORMAT "x(20)"
    .
DEFINE DATASET dsbrwscustitem FOR ttbrwscustitem.

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmActCitem  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLoc       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQty       AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmConsum    AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmUpdateCust    AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmUpdateloc    AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmUpdateItem    AS CHAR    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbrwscustitem.
    DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.
DEFINE BUFFER bf-cust-itm FOR cust-itm.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE vEst AS CHAR NO-UNDO.
IF prmActCitem       = ? THEN ASSIGN prmActCitem     = "".
IF prmUser           = ? THEN ASSIGN prmUser     = "".
IF prmItem           = ? THEN ASSIGN prmItem     = "".
IF prmCust           = ? THEN ASSIGN prmCust     = "".
IF prmLoc            = ? THEN ASSIGN prmLoc     = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
FOR EACH  usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK :
END.
FOR EACH ttbrwscustitem:
    DELETE ttbrwscustitem.
END.


/* ********************  Preprocessor Definitions  ******************** */

IF prmActCitem = "SearchCustItem" THEN DO:
 FOR EACH  usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK :
FOR EACH cust-itm where cust-itm.company = prmComp AND cust-itm.cust-no = usercust.cust-no
                  AND (cust-itm.cust-no BEGINS prmCust OR cust-itm.cust-no = prmCust or prmCust = "")
                  AND (cust-itm.loc BEGINS prmLoc OR cust-itm.loc = prmLoc OR prmLoc = "")
                  AND (cust-itm.i-no BEGINS prmItem OR cust-itm.i-no = prmItem OR prmItem = "") NO-LOCK:
    create ttbrwscustitem.
    assign 
        ttbrwscustitem.vCustno       = cust-itm.cust-no
        ttbrwscustitem.vLoc          = cust-itm.loc
        ttbrwscustitem.vItem         = cust-itm.i-no
        ttbrwscustitem.vQty          = cust-itm.qty
        ttbrwscustitem.vConsum       = cust-itm.consum  
        ttbrwscustitem.vreckey       = cust-itm.rec_key
        .
END.   /*FOR EACH cust-itm*/
 END.    /* end  for each usercust */
END.    /*IF prmActItem = "SearchCustItem" */
/************************************************************************************/

IF prmActCitem = "deletecustitem" THEN DO:
     FIND FIRST bf-cust-itm where bf-cust-itm.company = prmComp AND bf-cust-itm.cust-no =  prmCust
         AND bf-cust-itm.loc = prmLoc AND bf-cust-itm.i-no =  prmItem   EXCLUSIVE-LOCK NO-ERROR.   
  IF AVAIL bf-cust-itm  THEN DO:
      DELETE bf-cust-itm.        
  END.  /*IF AVAIL bf_bf-cust-itm */
  MESSAGE "del" prmComp prmCust prmLoc prmItem.
  FIND FIRST usercust WHERE usercust.company = usercomp.company AND usercust.USER_id = usercomp.USER_id NO-LOCK NO-ERROR.
  MESSAGE "test" usercomp.USER_id.
  FIND LAST cust-itm WHERE cust-itm.company = prmComp  NO-LOCK NO-ERROR.
  IF AVAIL cust-itm THEN DO:
      ASSIGN
         
      prmCust = cust-itm.cust-no
      prmLoc =  cust-itm.loc
      prmItem = cust-itm.i-no
      prmActCitem = "view".

      MESSAGE "del1" prmComp prmCust prmLoc prmItem prmActCitem.
 
  END.   /*IF AVAIL cust-itm THEN DO:*/
END. /*IF prmActCitem = "deletecustitem"*/
/******************************************************************************************/

IF prmActCitem = "AddCustItem" THEN DO:
    
FIND FIRST cust where cust.company = prmComp  AND cust.cust-no  = prmCust  NO-LOCK NO-ERROR. 
    IF  NOT AVAIL cust THEN  DO:
        cError = "Invalid Customer Number. Please try help.".
        RETURN.
    END.

    FIND FIRST shipto where shipto.company = prmComp  AND shipto.cust-no  = prmCust AND shipto.ship-id  = prmLoc NO-LOCK NO-ERROR. 
    IF  NOT AVAIL shipto THEN  DO:
        cError = "Invalid Ship To. Please try help.".
        RETURN.
    END.

    FIND FIRST itemfg where itemfg.company = prmComp  AND itemfg.i-no  = prmItem NO-LOCK NO-ERROR. 
    IF  NOT AVAIL itemfg THEN  DO:
        cError = "Invalid FG Item Number. Please try help.".
        RETURN.
    END.  

    FIND FIRST bf-cust-itm where bf-cust-itm.company = prmComp   AND bf-cust-itm.cust-no  = prmCust  AND  
        bf-cust-itm.loc = prmLoc AND bf-cust-itm.i-no =  prmItem   NO-LOCK NO-ERROR. 
    IF  AVAIL bf-cust-itm THEN  DO:        
        cError = "Customer Inventory already Exist with Duplicate FG Item#".
        RETURN.
    END.
    IF NOT AVAIL bf-cust-itm THEN DO:
        create bf-cust-itm.         
        assign  
            bf-cust-itm.company         = prmComp    
            bf-cust-itm.cust-no         = prmCust    
            bf-cust-itm.loc             = prmLoc     
            bf-cust-itm.i-no            = prmItem    
            bf-cust-itm.qty             = prmQty     
            bf-cust-itm.consum          = prmConsum 
            bf-cust-itm.rec_key         = prmCust.

RELEASE bf-cust-itm.
END.   /*IF NOT AVAIL cust-itm THEN DO:*/
ASSIGN prmActCitem = "view".
END.    /*IF prmActItem = "AddCustItem" */
/************************************************************************************/
IF prmActCitem = "UpdateCustItem" THEN DO:

    FIND FIRST cust where cust.company = prmComp  AND cust.cust-no  = prmCust  NO-LOCK NO-ERROR. 
    IF  NOT AVAIL cust THEN  DO:
        cError = "Invalid Customer Number. Please try help.".
        RETURN.
    END.

    FIND FIRST shipto where shipto.company = prmComp  AND shipto.cust-no  = prmCust AND shipto.ship-id  = prmLoc NO-LOCK NO-ERROR. 
    IF  NOT AVAIL shipto THEN  DO:
        cError = "Invalid Ship To. Please try help.".
        RETURN.
    END.

    FIND FIRST itemfg where itemfg.company = prmComp  AND itemfg.i-no  = prmItem NO-LOCK NO-ERROR. 
    IF  NOT AVAIL itemfg THEN  DO:
        cError = "Invalid FG Item Number. Please try help.".
        RETURN.
    END.    
    
   
     
   FIND FIRST cust-itm where cust-itm.company = prmComp AND 
                          cust-itm.cust-no =  prmUpdateCust AND
                          cust-itm.loc = prmUpdateloc AND cust-itm.i-no = prmUpdateItem  EXCLUSIVE-LOCK NO-ERROR.   
    FIND FIRST bf-cust-itm where bf-cust-itm.company = prmComp   AND bf-cust-itm.cust-no  = prmCust  AND  
                             bf-cust-itm.loc = prmLoc AND bf-cust-itm.i-no =  prmItem AND ROWID(bf-cust-itm) <>  rowid(cust-itm)  NO-LOCK NO-ERROR. 

    IF AVAILABLE bf-cust-itm THEN DO:
        cError = "Customer Inventory already Exist with Duplicate FG Item#".
        RETURN.
    END.
   IF  AVAILABLE cust-itm  THEN DO:
        assign  
                 
            cust-itm.cust-no         = prmCust    
            cust-itm.loc             = prmLoc     
            cust-itm.i-no            = prmItem    
            cust-itm.qty             = prmQty 
            cust-itm.consum          = prmConsum  
            .        
  END.   /*IF AVAIL cust-itm THEN DO:*/
  ASSIGN prmActCitem = "view".
END.
/******************************************************************************************/
IF prmActCitem = "SelCustItem" THEN DO:
 FOR EACH  usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK :
FOR EACH cust-itm where cust-itm.company = usercust.company AND cust-itm.cust-no = usercust.cust-no     no-lock :
        create ttbrwscustitem.
        assign 
            ttbrwscustitem.vCustno       = cust-itm.cust-no
            ttbrwscustitem.vLoc          = cust-itm.loc
            ttbrwscustitem.vItem         = cust-itm.i-no
            ttbrwscustitem.vQty          = cust-itm.qty
            ttbrwscustitem.vConsum       = cust-itm.consum
            ttbrwscustitem.vreckey       = cust-itm.rec_key  .
END.   /*FOR EACH cust-itm*/
 END.  /* for each usercust*/
END.    /*IF prmActItem = "SelCustItem" */


IF prmActCitem = "view" THEN DO:
 MESSAGE "view" prmComp prmCust  prmLoc prmReckey.
FIND FIRST cust-itm where cust-itm.company = prmComp AND 
                           cust-itm.i-no = prmItem NO-LOCK NO-ERROR.
        create ttbrwscustitem.
        assign 
            ttbrwscustitem.vCustno       = cust-itm.cust-no
            ttbrwscustitem.vLoc          = cust-itm.loc
            ttbrwscustitem.vItem         = cust-itm.i-no
            ttbrwscustitem.vQty          = cust-itm.qty
            ttbrwscustitem.vConsum       = cust-itm.consum 
            ttbrwscustitem.vreckey       = cust-itm.rec_key
            .
           
        FIND FIRST shipto WHERE shipto.company =  prmComp  AND
                                shipto.cust-no = cust-itm.cust-no AND
                                shipto.ship-id = cust-itm.loc NO-LOCK NO-ERROR.
        IF AVAIL shipto  THEN
            ASSIGN
             ttbrwscustitem.vDscr         = shipto.ship-name .

END.    /*IF prmActItem = "SelCustItem" */






