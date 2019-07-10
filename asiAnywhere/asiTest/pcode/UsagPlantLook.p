


/*------------------------------------------------------------------------
    File        : UsagPlantLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all PartLook

    Author(s)   : Jyoti
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUsagPlantCodeLook NO-UNDO 
    FIELD PlantCust        AS CHAR
    FIELD PlantId          AS CHAR
    FIELD DeptCode       AS CHAR
    FIELD PlantName        AS CHAR
    FIELD PlantShipId       AS CHAR
    FIELD VanderCode    AS CHAR
    FIELD bbbn           AS CHAR
    .


DEFINE DATASET dsUsagPlantCodeLook FOR ttUsagPlantCodeLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmFGItem    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmVanderCode AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUsagPlantCodeLook.


IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmFGItem    = ? THEN ASSIGN prmFGItem    = "".
IF prmVanderCode = ?  THEN ASSIGN prmVanderCode = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
  FOR EACH vend-whse-item WHERE  vend-whse-item.company = PrmComp
      AND vend-whse-item.vendor-code = prmVanderCode  AND vend-whse-item.fg-item-no = prmFGItem NO-LOCK,
            EACH vend-plant WHERE vend-plant.company = vend-whse-item.company 
                AND vend-plant.vendor-code = vend-whse-item.vendor-code 
                AND vend-plant.plant-id = vend-whse-item.vendor-plant-code    
                AND vend-plant.vendor-dept-code = vend-whse-item.vendor-dept-code NO-LOCK:
                    create ttUsagPlantCodeLook.
                        assign
                            ttUsagPlantCodeLook.PlantCust      = vend-whse-item.cust-no
                            ttUsagPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttUsagPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttUsagPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttUsagPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttUsagPlantCodeLook.VanderCode     = vend-whse-item.vendor-code
                             .
    
  
    END.	 /* FOR EACH eb */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "Plantid"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH vend-whse-item WHERE  vend-whse-item.company = PrmComp
      AND vend-whse-item.vendor-code = prmVanderCode  AND vend-whse-item.fg-item-no = prmFGItem AND vend-whse-item.vendor-plant-code = prmText NO-LOCK,
            EACH vend-plant WHERE vend-plant.company = vend-whse-item.company 
                AND vend-plant.vendor-code = vend-whse-item.vendor-code 
                AND vend-plant.plant-id = vend-whse-item.vendor-plant-code    
                AND vend-plant.vendor-dept-code = vend-whse-item.vendor-dept-code NO-LOCK:
                    create ttUsagPlantCodeLook.
                        assign
                            ttUsagPlantCodeLook.PlantCust      = vend-whse-item.cust-no
                            ttUsagPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttUsagPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttUsagPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttUsagPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttUsagPlantCodeLook.VanderCode     = vend-whse-item.vendor-code
                             .
    
  
    END.	 /* FOR EACH eb */     
       
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
              FOR EACH vend-whse-item WHERE  vend-whse-item.company = PrmComp
      AND vend-whse-item.vendor-code = prmVanderCode  AND vend-whse-item.fg-item-no = prmFGItem AND vend-whse-item.vendor-plant-code BEGINS prmText NO-LOCK,
            EACH vend-plant WHERE vend-plant.company = vend-whse-item.company 
                AND vend-plant.vendor-code = vend-whse-item.vendor-code 
                AND vend-plant.plant-id = vend-whse-item.vendor-plant-code    
                AND vend-plant.vendor-dept-code = vend-whse-item.vendor-dept-code NO-LOCK:
                    create ttUsagPlantCodeLook.
                        assign
                            ttUsagPlantCodeLook.PlantCust      = vend-whse-item.cust-no
                            ttUsagPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttUsagPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttUsagPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttUsagPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttUsagPlantCodeLook.VanderCode     = vend-whse-item.vendor-code
                             .
    
 
    END.	 /* FOR EACH eb */    
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "Name" then do:
         if prmCondition = "EQUAL" then do:
               FOR EACH vend-whse-item WHERE  vend-whse-item.company = PrmComp
      AND vend-whse-item.vendor-code = prmVanderCode  AND vend-whse-item.fg-item-no = prmFGItem  NO-LOCK,
            EACH vend-plant WHERE vend-plant.company = vend-whse-item.company 
                AND vend-plant.vendor-code = vend-whse-item.vendor-code 
                AND vend-plant.plant-id = vend-whse-item.vendor-plant-code    
                AND vend-plant.vendor-dept-code = vend-whse-item.vendor-dept-code 
                AND vend-plant.plant-name = prmText  NO-LOCK:
                    create ttUsagPlantCodeLook.
                        assign
                            ttUsagPlantCodeLook.PlantCust      = vend-whse-item.cust-no
                            ttUsagPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttUsagPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttUsagPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttUsagPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttUsagPlantCodeLook.VanderCode     = vend-whse-item.vendor-code
                             .
    
   
    END.	 /* FOR EACH eb */     
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
            
               FOR EACH vend-whse-item WHERE  vend-whse-item.company = PrmComp
      AND vend-whse-item.vendor-code = prmVanderCode  AND vend-whse-item.fg-item-no = prmFGItem  NO-LOCK,
            EACH vend-plant WHERE vend-plant.company = vend-whse-item.company 
                AND vend-plant.vendor-code = vend-whse-item.vendor-code 
                AND vend-plant.plant-id = vend-whse-item.vendor-plant-code    
                AND vend-plant.vendor-dept-code = vend-whse-item.vendor-dept-code 
                AND vend-plant.plant-name BEGINS prmText  NO-LOCK:
                    create ttUsagPlantCodeLook.
                        assign
                            ttUsagPlantCodeLook.PlantCust      = vend-whse-item.cust-no
                            ttUsagPlantCodeLook.PlantId        = vend-plant.plant-id
                            ttUsagPlantCodeLook.DeptCode       = vend-plant.vendor-dept-code
                            ttUsagPlantCodeLook.PlantName      = vend-plant.plant-name
                            ttUsagPlantCodeLook.PlantShipId    = vend-plant.ship-id
                            ttUsagPlantCodeLook.VanderCode     = vend-whse-item.vendor-code
                             .
    
   
    END.	 /* FOR EACH eb */  
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = est-no */
     

    
END.  /* IF prmAction = search then do: */



