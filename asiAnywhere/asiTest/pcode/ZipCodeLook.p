



/*------------------------------------------------------------------------
    File        : ZipCodeLook.p
    Purpose     : ZipCode

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttZipCodeLook NO-UNDO 
    FIELD vZip AS CHARACTER
    FIELD vPrefType AS CHARACTER
    FIELD vPref AS INTEGER
    FIELD vCity AS CHARACTER
    FIELD vState AS CHARACTER
    .
                                           
    
DEFINE DATASET dsZipCodeLook FOR ttZipCodeLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsZipCodeLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
        
    FOR EACH zipcode NO-LOCK:
                 create ttZipCodeLook.
                 assign                                     
                    ttZipCodeLook.vZip      = zipcode.zipcode
                    ttZipCodeLook.vPrefType = zipcode.pref_type
                    ttZipCodeLook.vPref     = zipcode.pref#
                    ttZipCodeLook.vCity     = zipcode.city
                    ttZipCodeLook.vState    = zipcode.state  

                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH zipcode  NO-LOCK:
                    IF (zipcode.zipcode = prmText OR zipcode.pref_type = prmText  
                        OR zipcode.pref# = int(prmText)  OR zipcode.city = prmText  
                        OR zipcode.state = prmText) THEN
                        DO:
                        create ttZipCodeLook.
                        assign                                     
                            ttZipCodeLook.vZip      = zipcode.zipcode
                            ttZipCodeLook.vPrefType = zipcode.pref_type
                            ttZipCodeLook.vPref     = zipcode.pref#
                            ttZipCodeLook.vCity     = zipcode.city
                            ttZipCodeLook.vState    = zipcode.state  .
                        END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH zipcode NO-LOCK :
                IF (zipcode.zipcode BEGINS prmText OR zipcode.pref_type BEGINS prmText 
                    OR zipcode.pref# = int(prmText)  OR zipcode.city BEGINS prmText 
                    OR zipcode.state BEGINS prmText) THEN
                    DO:
                    create ttZipCodeLook.
                    assign                                     
                        ttZipCodeLook.vZip      = zipcode.zipcode
                    ttZipCodeLook.vPrefType = zipcode.pref_type
                    ttZipCodeLook.vPref     = zipcode.pref#
                    ttZipCodeLook.vCity     = zipcode.city
                    ttZipCodeLook.vState    = zipcode.state . 
                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "zipcode"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH zipcode WHERE  zipcode.zipcode = prmText  NO-LOCK :
                 create ttZipCodeLook.
                 assign                                     
                    ttZipCodeLook.vZip      = zipcode.zipcode
                    ttZipCodeLook.vPrefType = zipcode.pref_type
                    ttZipCodeLook.vPref     = zipcode.pref#
                    ttZipCodeLook.vCity     = zipcode.city
                    ttZipCodeLook.vState    = zipcode.state  .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH zipcode WHERE zipcode.zipcode BEGINS prmText NO-LOCK :
                      create ttZipCodeLook.
                      assign   
                          ttZipCodeLook.vZip      = zipcode.zipcode
                          ttZipCodeLook.vPrefType = zipcode.pref_type
                          ttZipCodeLook.vPref     = zipcode.pref#
                          ttZipCodeLook.vCity     = zipcode.city
                          ttZipCodeLook.vState    = zipcode.state  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */


         if prmField = "city"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH zipcode WHERE  zipcode.city = prmText  NO-LOCK :
                 create ttZipCodeLook.
                 assign                                     
                    ttZipCodeLook.vZip      = zipcode.zipcode
                    ttZipCodeLook.vPrefType = zipcode.pref_type
                    ttZipCodeLook.vPref     = zipcode.pref#
                    ttZipCodeLook.vCity     = zipcode.city
                    ttZipCodeLook.vState    = zipcode.state  .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH zipcode WHERE zipcode.city BEGINS prmText NO-LOCK :
                      create ttZipCodeLook.
                      assign   
                          ttZipCodeLook.vZip      = zipcode.zipcode
                          ttZipCodeLook.vPrefType = zipcode.pref_type
                          ttZipCodeLook.vPref     = zipcode.pref#
                          ttZipCodeLook.vCity     = zipcode.city
                          ttZipCodeLook.vState    = zipcode.state  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = city  */
           
END.  /* IF prmAction = search then do: */

