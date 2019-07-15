




/*------------------------------------------------------------------------
    File        : BoardPoRepLook.p
    Purpose     : Board Po

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPoItemLookpo NO-UNDO 
    FIELD ponopo       AS INTEGER
    FIELD podatepo     AS DATE
    FIELD vendorpo     AS CHARACTER
    FIELD printpo      AS LOGICAL
    FIELD statpo       AS CHARACTER
    FIELD itempo       AS CHAR
    FIELD itemname     AS CHAR
   
    .
                                           
    
DEFINE DATASET dsPoItemLookpo FOR ttPoItemLookpo .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPoItemLookpo.
       
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
     FOR EACH po-ordl WHERE  po-ordl.company = prmComp  NO-LOCK, 
        FIRST po-ord WHERE po-ord.company eq po-ordl.company and 
        po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.i-no:
       
       create ttPoItemLookpo.
                 assign                                     
                    ttPoItemLookpo.ponopo    = po-ord.po-no
                    ttPoItemLookpo.podatepo  = po-ord.po-date
                    ttPoItemLookpo.vendorpo  = po-ord.vend-no
                    ttPoItemLookpo.printpo   = po-ord.printed
                    ttPoItemLookpo.statpo    = po-ord.stat 
                    ttPoItemLookpo.itempo    = po-ordl.i-no
                    ttPoItemLookpo.itemname  = po-ordl.i-name 
                   
                   .
                  END.  /*FOR EACH po-ord*/
        
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
       if prmField = "itemname"  then do:
         if prmCondition = "EQUAL" then do:
              
              FOR EACH po-ordl WHERE  po-ordl.company = prmComp  AND  po-ordl.i-name = prmtext NO-LOCK,
                  FIRST po-ord WHERE po-ord.company eq po-ordl.company and
                  po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.i-no  BY po-ordl.i-name  :
            
                  create ttPoItemLookpo.
                 assign                                     
                    ttPoItemLookpo.ponopo    = po-ord.po-no
                    ttPoItemLookpo.podatepo  = po-ord.po-date
                    ttPoItemLookpo.vendorpo  = po-ord.vend-no
                    ttPoItemLookpo.printpo    = po-ord.printed
                    ttPoItemLookpo.statpo    = po-ord.stat  
                      ttPoItemLookpo.itempo   = po-ordl.i-no
                    ttPoItemLookpo.itemname = po-ordl.i-name 
  .
            

          END. /*FOR EACH state*/
         END.
          IF prmCondition = "BEGIN" then do:
               FOR EACH po-ordl WHERE  po-ordl.company = prmComp  AND  po-ordl.i-name BEGINS prmtext NO-LOCK,
                  FIRST po-ord WHERE po-ord.company eq po-ordl.company and
                  po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.i-no  BY po-ordl.i-name  :
            
                   create ttPoItemLookpo.
                 assign                                     
                    ttPoItemLookpo.ponopo    = po-ord.po-no
                    ttPoItemLookpo.podatepo  = po-ord.po-date
                    ttPoItemLookpo.vendorpo  = po-ord.vend-no
                    ttPoItemLookpo.printpo    = po-ord.printed
                    ttPoItemLookpo.statpo    = po-ord.stat 
                      ttPoItemLookpo.itempo   = po-ordl.i-no
                    ttPoItemLookpo.itemname = po-ordl.i-name   .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */

         if prmField = "itempo"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH po-ordl WHERE  po-ordl.company = prmComp  AND  po-ordl.i-no = prmtext NO-LOCK,
                  FIRST po-ord WHERE po-ord.company eq po-ordl.company and
                  po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.i-no  BY po-ordl.i-name  :
            
                  create ttPoItemLookpo.
                 assign                                     
                    ttPoItemLookpo.ponopo    = po-ord.po-no
                    ttPoItemLookpo.podatepo  = po-ord.po-date
                    ttPoItemLookpo.vendorpo  = po-ord.vend-no
                    ttPoItemLookpo.printpo    = po-ord.printed
                    ttPoItemLookpo.statpo    = po-ord.stat  
                      ttPoItemLookpo.itempo   = po-ordl.i-no
                    ttPoItemLookpo.itemname = po-ordl.i-name 
  .
            

          END. /*FOR EACH state*/
         END.
          IF prmCondition = "BEGIN" then do:
              FOR EACH po-ordl WHERE  po-ordl.company = prmComp  AND  po-ordl.i-no BEGINS prmtext NO-LOCK,
                  FIRST po-ord WHERE po-ord.company eq po-ordl.company and
                  po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.i-no  BY po-ordl.i-name  :
            
                   create ttPoItemLookpo.
                 assign                                     
                    ttPoItemLookpo.ponopo    = po-ord.po-no
                    ttPoItemLookpo.podatepo  = po-ord.po-date
                    ttPoItemLookpo.vendorpo  = po-ord.vend-no
                    ttPoItemLookpo.printpo    = po-ord.printed
                    ttPoItemLookpo.statpo    = po-ord.stat 
                      ttPoItemLookpo.itempo   = po-ordl.i-no
                    ttPoItemLookpo.itemname = po-ordl.i-name   .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */

          if prmField = "pono"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH po-ordl WHERE  po-ordl.company = prmComp  AND  po-ordl.po-no = int(prmtext) NO-LOCK,
                  FIRST po-ord WHERE po-ord.company eq po-ordl.company and
                  po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.i-no  BY po-ordl.i-name  :
            
                  create ttPoItemLookpo.
                 assign                                     
                    ttPoItemLookpo.ponopo    = po-ord.po-no
                    ttPoItemLookpo.podatepo  = po-ord.po-date
                    ttPoItemLookpo.vendorpo  = po-ord.vend-no
                    ttPoItemLookpo.printpo    = po-ord.printed
                    ttPoItemLookpo.statpo    = po-ord.stat  
                      ttPoItemLookpo.itempo   = po-ordl.i-no
                    ttPoItemLookpo.itemname = po-ordl.i-name 
  .
            

          END. /*FOR EACH state*/
         END.
          IF prmCondition = "BEGIN" then do:
              FOR EACH po-ordl WHERE  po-ordl.company = prmComp  AND  po-ordl.po-no = INT(prmtext) NO-LOCK,
                  FIRST po-ord WHERE po-ord.company eq po-ordl.company and
                  po-ord.po-no eq po-ordl.po-no NO-LOCK BY po-ordl.i-no  BY po-ordl.i-name  :
            
                   create ttPoItemLookpo.
                 assign                                     
                    ttPoItemLookpo.ponopo    = po-ord.po-no
                    ttPoItemLookpo.podatepo  = po-ord.po-date
                    ttPoItemLookpo.vendorpo  = po-ord.vend-no
                    ttPoItemLookpo.printpo    = po-ord.printed
                    ttPoItemLookpo.statpo    = po-ord.stat 
                      ttPoItemLookpo.itempo   = po-ordl.i-no
                    ttPoItemLookpo.itemname = po-ordl.i-name   .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
       
END.  /* IF prmAction = search then do: */



