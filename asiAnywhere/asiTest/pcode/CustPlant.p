 



/*------------------------------------------------------------------------
    File        : CustPlant.p
    Purpose     : Customer Plant/Warehouse

    Syntax      :

    Description : Return a Dataset of all Customer Plant\WareHouse
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttbrwsWarehouseCust  NO-UNDO
    FIELD  vCompany         AS CHAR       FORMAT "x(3)"     
    FIELD  vCust            AS CHAR       FORMAT "x(8)"     
    FIELD  vShipid          AS CHAR       FORMAT "x(8)"    
    FIELD  vPlantid         AS CHAR       FORMAT "x(30)"  
    FIELD  vVenderDeptCode  AS CHAR       FORMAT "x(30)"  
    FIELD  vPlantName       AS CHAR       FORMAT "x(30)" 
    FIELD  vVendorCode      AS CHAR       FORMAT "x(15)" 
    FIELD  vReckey          AS CHAR      
    FIELD  mnkfgh           AS CHAR       FORMAT "x(25)"
    .
DEFINE DATASET dsbrwsWarehouseCust FOR ttbrwsWarehouseCust.

DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmActPlant       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust           AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShipid         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPlantid          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDeptCode       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPlantName      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVanderCode      AS CHARACTER  NO-UNDO.

DEFINE OUTPUT PARAMETER cError           AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbrwsWarehouseCust.

DEF VAR prmComp AS CHAR NO-UNDO.
   DEF BUFFER b-shipto FOR shipto.  
   DEF BUFFER b-vend-code-cust-xref FOR vend-code-cust-xref.  

IF prmUser        = ? THEN ASSIGN  prmUser        = "".    
IF prmReckey      = ? THEN ASSIGN  prmReckey      = "".   
IF prmActPlant    = ? THEN ASSIGN  prmActPlant    = "".    
IF prmCust        = ? THEN ASSIGN  prmCust        = "".    
IF prmShipid      = ? THEN ASSIGN  prmShipid      = "".    
IF prmPlantid     = ? THEN ASSIGN  prmPlantid     = "".    
IF prmDeptCode    = ? THEN ASSIGN  prmDeptCode    = "".    
IF prmPlantName   = ? THEN ASSIGN  prmPlantName   = "".    
IF prmVanderCode  = ? THEN ASSIGN  prmVanderCode  = "".    

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
FOR EACH  usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK:
END.
FOR EACH ttbrwsWarehouseCust:
    DELETE ttbrwsWarehouseCust.
END.

/* ********************  Preprocessor Definitions  ******************** */


IF prmActPlant = "SelCustPlant" THEN DO:
FOR EACH  usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK:
FOR EACH vend-plant where vend-plant.company = prmComp AND vend-plant.cust-no = usercust.cust-no     no-lock :
     create ttbrwsWarehouseCust.
     assign
         ttbrwsWarehouseCust.vCust                = vend-plant.cust-no
         ttbrwsWarehouseCust.vShipid              = vend-plant.ship-id
         ttbrwsWarehouseCust.vPlantid             = vend-plant.plant-id
         ttbrwsWarehouseCust.vVenderDeptCode      = vend-plant.vendor-dept-code
         ttbrwsWarehouseCust.vPlantName           = vend-plant.plant-name
         ttbrwsWarehouseCust.vVendorCode          = vend-plant.vendor-code
         ttbrwsWarehouseCust.vReckey              = vend-plant.rec_key
         
         .
END.   /*FOR EACH shipto*/
END.  /* end user cust*/
END.    /*IF prmActItem = "SelCustItem" */


IF prmActPlant = "SearchCustPlant" THEN DO:

    FOR EACH  usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK:
FOR EACH vend-plant where vend-plant.company = prmComp AND vend-plant.cust-no = usercust.cust-no
                  AND (vend-plant.cust-no   BEGINS prmCust   OR vend-plant.cust-no   = prmCust    OR prmCust = "")
                  AND (vend-plant.ship-id   BEGINS prmShipid OR vend-plant.ship-id   = prmShipid  OR prmShipid = "")
                  AND (vend-plant.plant-id BEGINS prmPlantid  OR vend-plant.plant-id = prmPlantid   OR prmPlantid = "") NO-LOCK:
    create ttbrwsWarehouseCust.
    assign
        ttbrwsWarehouseCust.vCust                = vend-plant.cust-no
        ttbrwsWarehouseCust.vShipid              = vend-plant.ship-id
        ttbrwsWarehouseCust.vPlantid             = vend-plant.plant-id
        ttbrwsWarehouseCust.vVenderDeptCode      = vend-plant.vendor-dept-code
        ttbrwsWarehouseCust.vPlantName           = vend-plant.plant-name
        ttbrwsWarehouseCust.vVendorCode          = vend-plant.vendor-code
        ttbrwsWarehouseCust.vReckey              = vend-plant.rec_key
        
         .
END.   /*FOR EACH shipto*/
    END.  /* for each user cust */
END.    /*IF prmActItem = "SearchCustItem" */
/************************************************************************************/
IF prmActPlant = "DeleteCustPlant" THEN DO:
      FIND FIRST vend-plant where vend-plant.company = prmComp AND 
       vend-plant.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR. 
  
  IF AVAIL vend-plant  THEN DO:
      DELETE vend-plant.        
  END.  /*IF AVAIL bf_bf-shipto */
  
  FOR EACH  usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id NO-LOCK:
  FIND LAST vend-plant WHERE vend-plant.company = prmComp AND vend-plant.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
  IF AVAIL vend-plant THEN DO:
      ASSIGN
          prmReckey =  vend-plant.rec_key
          prmActPlant = "view".
        
  END.   /*IF AVAIL shipto THEN DO:*/
  END.   /*end user cust*/
END. /*IF prmActPlant = "deletecustitem"*/  
/*************************ADD*****************************************************************/
IF prmActPlant = "AddCustPlant" THEN DO:
 FIND FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = prmComp
                                         AND b-vend-code-cust-xref.cust-no = prmCust NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-code-cust-xref) THEN DO:
         cError =  "Invalid Suppliers A/R Code     " .
         RETURN .
      END.

      
      IF NOT CAN-FIND(FIRST b-shipto WHERE b-shipto.company = prmComp
                                       AND b-shipto.cust-no = prmCust
                                       AND b-shipto.ship-id = prmShipid) THEN DO:
         cError =  "Invalid Suppliers Ship To ID     "  .
         RETURN .
      END.

      FIND FIRST  vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp
          AND vend-code-cust-xref.cust-no = prmCust AND vend-code-cust-xref.vendor-code = prmVanderCode  NO-LOCK NO-ERROR.
      IF NOT AVAIL vend-code-cust-xref  THEN DO:
          ASSIGN
              cError = "Invalid Customers A/P Code.. Try Help" .
           RETURN.
      END.

END.


IF prmActPlant = "AddCustPlant" THEN DO:
   
        create vend-plant .         
        assign 
            vend-plant.company          = prmComp
            vend-plant.cust-no          = prmCust  
            vend-plant.ship-id          = prmShipid
            vend-plant.plant-id         = prmPlantid   
            vend-plant.vendor-dept-code = prmDeptCode    
            vend-plant.plant-name       = prmPlantName   
            vend-plant.vendor-code      = prmVanderCode
            vend-plant.rec_key          = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME).
            .

ASSIGN 
    prmReckey = vend-plant.rec_key  
    prmActPlant = "view".

END.    /*IF prmActItem = "AddCustItem" */  
/************************************************************************************/
IF prmActPlant = "UpdateCustPlant" THEN DO:
MESSAGE "testplant" prmCust prmShipid.
      FIND FIRST b-vend-code-cust-xref WHERE b-vend-code-cust-xref.company = prmComp
                                         AND b-vend-code-cust-xref.cust-no = prmCust NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-code-cust-xref) THEN DO:
         cError =  "Invalid Suppliers A/R Code     " .
         RETURN .
      END.

      
      IF NOT CAN-FIND(FIRST b-shipto WHERE b-shipto.company = prmComp
                                       AND b-shipto.cust-no = prmCust
                                       AND b-shipto.ship-id = prmShipid) THEN DO:
         cError =  "Invalid Suppliers Ship To ID     "  .
         RETURN .
      END.

       FIND FIRST  vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp
          AND vend-code-cust-xref.cust-no = prmCust AND vend-code-cust-xref.vendor-code = prmVanderCode  NO-LOCK NO-ERROR.
      IF NOT AVAIL vend-code-cust-xref  THEN DO:
          ASSIGN
              cError = "Invalid Customers A/P Code.. Try Help" .
           RETURN.
      END.
 

      
END.

IF prmActPlant = "UpdateCustPlant" THEN DO:
  
   FIND FIRST vend-plant where vend-plant.company = prmComp AND 
       vend-plant.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR. 

   IF AVAILABLE vend-plant  THEN DO:
        assign  
            vend-plant.cust-no          = prmCust  
            vend-plant.ship-id          = prmShipid
            vend-plant.plant-id         = prmPlantid   
            vend-plant.vendor-dept-code = prmDeptCode    
            vend-plant.plant-name       = prmPlantName   
            vend-plant.vendor-code      = prmVanderCode
           
            .
  END.   /*IF AVAIL shipto THEN DO:*/
  ASSIGN prmActPlant = "view".
END.


/**************************************************************************************************/
IF prmActPlant = "view" THEN DO:
   
        FIND FIRST vend-plant where vend-plant.company = prmComp AND vend-plant.rec_key = prmReckey NO-LOCK NO-ERROR.
        create ttbrwsWarehouseCust.
            assign
                ttbrwsWarehouseCust.vCust                = vend-plant.cust-no
                ttbrwsWarehouseCust.vShipid              = vend-plant.ship-id
                ttbrwsWarehouseCust.vPlantid             = vend-plant.plant-id
                ttbrwsWarehouseCust.vVenderDeptCode      = vend-plant.vendor-dept-code
                ttbrwsWarehouseCust.vPlantName           = vend-plant.plant-name
                ttbrwsWarehouseCust.vVendorCode          = vend-plant.vendor-code
                ttbrwsWarehouseCust.vReckey              = vend-plant.rec_key  .

        
 END.    /*IF prmActItem = "SelCustItem" */









