


/*------------------------------------------------------------------------
    File        : CustPlantLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all PartLook

    Author(s)   : Jyoti
    Created     : Oct 21 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustPlantCodeLook NO-UNDO 
    FIELD PlantCust         AS CHAR
    FIELD PlantId           AS CHAR
    FIELD DeptCode          AS CHAR
    FIELD PlantName         AS CHAR
    FIELD PlantShipId       AS CHAR
    FIELD VanderCode        AS CHAR
    FIELD bbcng             AS CHAR
    .


DEFINE DATASET dsCustPlantCodeLook FOR ttCustPlantCodeLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustNo      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustPlantCodeLook.


IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCustNo      = ? THEN ASSIGN prmCustNo      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH vend-plant WHERE vend-plant.company = prmComp AND vend-plant.cust-no = prmCustNo NO-LOCK:
        create ttCustPlantCodeLook.
            assign
                ttCustPlantCodeLook.PlantCust      = vend-plant.cust-no
                ttCustPlantCodeLook.PlantId        = vend-plant.plant-id
                ttCustPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                ttCustPlantCodeLook.PlantName      = vend-plant.plant-name
                ttCustPlantCodeLook.PlantShipId    = vend-plant.ship-id
                ttCustPlantCodeLook.VanderCode     = vend-plant.vendor-code
                             .  
    END.	 /* FOR EACH eb */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    if prmField = "Plantid"  then do:
        if prmCondition = "EQUAL" then do:
            
            FOR EACH vend-plant WHERE vend-plant.company = prmComp 
                AND vend-plant.cust-no = prmCustNo
                AND vend-plant.plant-id = prmText NO-LOCK:
                    create ttCustPlantCodeLook.
                        assign
                            ttCustPlantCodeLook.PlantCust      = vend-plant.cust-no
                            ttCustPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttCustPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttCustPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttCustPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttCustPlantCodeLook.VanderCode     = vend-plant.vendor-code
                             .
 
            END.	 /* FOR EACH eb */            
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
              FOR EACH vend-plant WHERE vend-plant.company = prmComp 
                AND vend-plant.cust-no = prmCustNo 
                AND vend-plant.plant-id BEGINS prmText NO-LOCK:
                    create ttCustPlantCodeLook.
                        assign
                            ttCustPlantCodeLook.PlantCust      = vend-plant.cust-no
                            ttCustPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttCustPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttCustPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttCustPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttCustPlantCodeLook.VanderCode     = vend-plant.vendor-code
                             .
    
              END.	 /* FOR EACH eb */    
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "Name" then do:
         if prmCondition = "EQUAL" then do:
               FOR EACH vend-plant WHERE vend-plant.company = prmComp 
                AND vend-plant.cust-no = prmCustNo                 
                AND vend-plant.plant-name = prmText  NO-LOCK:
                    create ttCustPlantCodeLook.
                        assign
                            ttCustPlantCodeLook.PlantCust      = vend-plant.cust-no
                            ttCustPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttCustPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttCustPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttCustPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttCustPlantCodeLook.VanderCode     = vend-plant.vendor-code
                             .
    
   
    END.	 /* FOR EACH eb */     
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:            
               FOR EACH vend-plant WHERE vend-plant.company = prmComp 
                AND vend-plant.cust-no = prmCustNo               
                AND vend-plant.plant-name BEGINS prmText  NO-LOCK:
                    create ttCustPlantCodeLook.
                        assign
                            ttCustPlantCodeLook.PlantCust      = vend-plant.cust-no
                            ttCustPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttCustPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttCustPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttCustPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttCustPlantCodeLook.VanderCode     = vend-plant.vendor-code
                             .
    
   
    END.	 /* FOR EACH eb */  
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = est-no */
    
END.  /* IF prmAction = search then do: */



