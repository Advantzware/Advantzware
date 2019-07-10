




/*------------------------------------------------------------------------
    File        : TypeOrdLook.p
    Purpose     : Type Code

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTypeOrdLook NO-UNDO 
    FIELD vtype AS CHARACTER
   .
                                           
    
DEFINE DATASET dstypeOrdLook FOR ttTypeOrdLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dstypeOrdLook.
       
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
        
    FOR EACH oe-ord NO-LOCK:
                 create ttTypeOrdLook.
                 assign                                     
                    ttTypeOrdLook.vtype      = oe-ord.TYPE . 
               
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH oe-ord  NO-LOCK:
                    IF (oe-ord.type = prmText ) THEN
                        DO:
                        create ttTypeOrdLook.
                        assign                                     
                             ttTypeOrdLook.vtype      = oe-ord.type
                             .
                        END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH oe-ord NO-LOCK :
                IF (oe-ord.type BEGINS prmText ) THEN
                    DO:
                    create ttTypeOrdLook.
                    assign                                     
                       ttTypeOrdLook.vtype      = oe-ord.type
                     . 
                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "code"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH oe-ord WHERE  oe-ord.type = prmText  NO-LOCK :
                 create ttTypeOrdLook.
                 assign                                     
                     ttTypeOrdLook.vtype      = oe-ord.type  .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH oe-ord WHERE oe-ord.type BEGINS prmText NO-LOCK :
                      create ttTypeOrdLook.
                      assign   
                          ttTypeOrdLook.vtype      = oe-ord.type .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */




