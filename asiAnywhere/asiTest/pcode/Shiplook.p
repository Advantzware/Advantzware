

/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ShipLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItemNum   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsShipLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmItemNum      = ? THEN ASSIGN prmItemNum      = "".
IF prmField      = ? THEN ASSIGN prmField      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction <> "search" then do:
    /*FOR EACH Itemfg WHERE
        itemfg.company EQ prmComp AND
        itemfg.i-no = prmItemNum NO-LOCK,*/
      FOR EACH shipto WHERE 
             shipto.company = prmComp NO-LOCK:
             create ttShipLook.
             assign                                     
                ttShipLook.cust-no =  shipto.cust-no
                ttShipLook.ship-id    = shipto.ship-id
                ttShipLook.SName = shipto.ship-name 
                ttShipLook.Addr    = shipto.ship-addr[1]
                ttShipLook.City = shipto.ship-city
                ttShipLook.State    = shipto.ship-state
                ttShipLook.Zip = shipto.ship-zip
                ttShipLook.carrier    = shipto.carrier
                ttShipLook.Loc = shipto.loc.
    END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */
/*
MESSAGE "ship" prmAction prmComp prmItemNum.
IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR each Itemfg WHERE
               itemfg.company EQ prmComp AND
               itemfg.i-no = prmItemNum
               NO-LOCK,
               EACH shipto where
                    shipto.cust-no = itemfg.cust-no AND
                    shipto.company = itemfg.company 
                    no-lock:

               IF shipto.ship-id = prmText OR
                  shipto.ship-name = prmText THEN
               DO:
                  create ttShipLook.
                  assign                                     
                      ttShipLook.cust-no =  shipto.cust-no
                      ttShipLook.ship-id    = shipto.ship-id
                      ttShipLook.SName = shipto.ship-name 
                      ttShipLook.Addr    = shipto.ship-addr[1]
                      ttShipLook.City = shipto.ship-city
                      ttShipLook.State    = shipto.ship-state
                      ttShipLook.Zip = shipto.ship-zip
                      ttShipLook.carrier    = shipto.carrier
                      ttShipLook.Loc = shipto.loc.
               END.
           END.
        END.  /* IF prmCondition = "EQUAL"*/
        
        if prmCondition = "BEGIN" then do:
           FOR FIRST Itemfg WHERE
               itemfg.company EQ prmComp AND
               itemfg.i-no = prmItemNum
               NO-LOCK,
               EACH shipto where
                    shipto.cust-no = itemfg.cust-no AND
                    shipto.company = itemfg.company 
                    no-lock:

               IF shipto.ship-id begins prmText OR
                  shipto.ship-name begins prmText THEN
               DO:
                  create ttShipLook.
                  assign                                                              
                      ttShipLook.cust-no =  shipto.cust-no
                      ttShipLook.ship-id    = shipto.ship-id
                      ttShipLook.SName = shipto.ship-name 
                      ttShipLook.Addr    = shipto.ship-addr[1]
                      ttShipLook.City = shipto.ship-city
                      ttShipLook.State    = shipto.ship-state
                      ttShipLook.Zip = shipto.ship-zip
                      ttShipLook.carrier    = shipto.carrier
                      ttShipLook.Loc = shipto.loc.
               END.
               
           END.  /*FOR EACH itemfg */
        END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "ship-id" then do:
    if prmCondition = "EQUAL" then do:
        FOR FIRST Itemfg WHERE
            itemfg.company EQ prmComp AND
            itemfg.i-no = prmItemNum
            NO-LOCK,
            EACH shipto where
                 shipto.cust-no = itemfg.cust-no AND
                 shipto.company = itemfg.company AND
                 shipto.ship-id = prmText no-lock:
            create ttShipLook.
            assign                                                                
                ttShipLook.cust-no =  shipto.cust-no
                ttShipLook.ship-id    = shipto.ship-id
                ttShipLook.SName = shipto.ship-name 
                ttShipLook.Addr    = shipto.ship-addr[1]
                ttShipLook.City = shipto.ship-city
                ttShipLook.State    = shipto.ship-state
                ttShipLook.Zip = shipto.ship-zip
                ttShipLook.carrier    = shipto.carrier
                ttShipLook.Loc = shipto.loc.
        END.  /*for each itemfg*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
        FOR FIRST Itemfg WHERE
            itemfg.company EQ prmComp AND
            itemfg.i-no = prmItemNum
            NO-LOCK,
        EACH shipto where shipto.cust-no = itemfg.cust-no 
                          AND shipto.company = itemfg.company 
                          AND shipto.ship-id begins prmText no-lock:
            create ttShipLook.
            assign                                                                  
                ttShipLook.cust-no =  shipto.cust-no
                ttShipLook.ship-id    = shipto.ship-id
                ttShipLook.SName = shipto.ship-name 
                ttShipLook.Addr    = shipto.ship-addr[1]
                ttShipLook.City = shipto.ship-city
                ttShipLook.State    = shipto.ship-state
                ttShipLook.Zip = shipto.ship-zip
                ttShipLook.carrier    = shipto.carrier
                ttShipLook.Loc = shipto.loc.
        END.  /*for each itemfg*/
    END.  /*if prmCondition = "BEGIN"*/  
END.   /*  if prmField = "part-no" then do:*/

if prmField = "ship-name" then do:
    if prmCondition = "EQUAL" then do:
        FOR FIRST Itemfg WHERE
            itemfg.company EQ prmComp AND
            itemfg.i-no = prmItemNum NO-LOCK,
            EACH shipto where shipto.cust-no = itemfg.cust-no 
                          AND shipto.company = itemfg.company 
                          AND shipto.ship-name = prmText no-lock:
            create ttShipLook.
            assign                                                                
                ttShipLook.cust-no =  shipto.cust-no
                ttShipLook.ship-id    = shipto.ship-id
                ttShipLook.SName = shipto.ship-name 
                ttShipLook.Addr    = shipto.ship-addr[1]
                ttShipLook.City = shipto.ship-city
                ttShipLook.State    = shipto.ship-state
                ttShipLook.Zip = shipto.ship-zip
                ttShipLook.carrier    = shipto.carrier
                ttShipLook.Loc = shipto.loc.
        END.
    END.   /*if prmCondition = "EQUAL" */
    
    if prmCondition = "BEGIN" then do:
        FOR FIRST Itemfg WHERE
            itemfg.company EQ prmComp AND
            itemfg.i-no = prmItemNum
            NO-LOCK,
            EACH shipto where shipto.cust-no = itemfg.cust-no 
                          AND shipto.company = itemfg.company 
                          AND shipto.ship-name begins prmText no-lock:
            create ttShipLook.
            assign                                                                  
                ttShipLook.cust-no =  shipto.cust-no
                ttShipLook.ship-id    = shipto.ship-id
                ttShipLook.SName = shipto.ship-name 
                ttShipLook.Addr    = shipto.ship-addr[1]
                ttShipLook.City = shipto.ship-city
                ttShipLook.State    = shipto.ship-state
                ttShipLook.Zip = shipto.ship-zip
                ttShipLook.carrier    = shipto.carrier
                ttShipLook.Loc = shipto.loc.
       END.   /*for each itemfg*/
    END.  /*if prmCondition = "BEGIN" */     
END.   /*if prmField = "ship-name"*/
END. /* IF prmAction = search then do: */


  */
