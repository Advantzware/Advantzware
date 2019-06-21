




/*------------------------------------------------------------------------
    File        : Ld_polook.p
    Purpose     : Loadtag Po

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPoLoadtagLookpo NO-UNDO 
    FIELD ponopo       AS INTEGER
    FIELD podatepo     AS DATE
    FIELD vendorpo     AS CHARACTER
    FIELD job          AS CHAR
    FIELD job2         AS INT
    FIELD itempo       AS CHAR
    FIELD itemname     AS CHAR
    FIELD ext          AS CHAR
   
    .
                                           
    
DEFINE DATASET dsPoLoadtagLookpo FOR ttPoLoadtagLookpo .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPoLoadtagLookpo.
       
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
     FOR EACH po-ord WHERE po-ord.company = prmComp and 
         asi.po-ord.opened  = yes NO-LOCK, 
         FIRST po-ordl WHERE po-ordl.company eq po-ord.company and 
         po-ordl.po-no eq po-ord.po-no 
         AND po-ordl.deleted = no NO-LOCK BY po-ord.po-no
                                          BY po-ord.vend-no
                                          BY po-ord.po-date :

         create ttPoLoadtagLookpo.
                 assign                                     
                    ttPoLoadtagLookpo.ponopo    = po-ord.po-no
                    ttPoLoadtagLookpo.podatepo  = po-ord.po-date
                    ttPoLoadtagLookpo.vendorpo  = po-ord.vend-no
                    ttPoLoadtagLookpo.job       = po-ordl.job-no
                    ttPoLoadtagLookpo.job2      = po-ordl.job-no2
                    ttPoLoadtagLookpo.itempo    = po-ordl.i-no
                    ttPoLoadtagLookpo.itemname  = po-ordl.i-name .
     END. /*FOR EACH po-ord*/
        
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do: 
        if prmField = "pono"  then do:
            if prmCondition = "EQUAL" then do:
                FOR EACH po-ord WHERE po-ord.company = prmComp 
                    AND  po-ordl.po-no = int(prmtext)
                    AND  asi.po-ord.opened  = yes NO-LOCK, 
                    FIRST po-ordl WHERE po-ordl.company eq po-ord.company and 
                    po-ordl.po-no eq po-ord.po-no 
                    AND po-ordl.deleted = no NO-LOCK BY po-ord.po-no
                                                     BY po-ord.vend-no
                                                     BY po-ord.po-date :

                    create ttPoLoadtagLookpo.
                    assign                                     
                    ttPoLoadtagLookpo.ponopo    = po-ord.po-no
                    ttPoLoadtagLookpo.podatepo  = po-ord.po-date
                    ttPoLoadtagLookpo.vendorpo  = po-ord.vend-no
                    ttPoLoadtagLookpo.job       = po-ordl.job-no
                    ttPoLoadtagLookpo.job2      = po-ordl.job-no2
                    ttPoLoadtagLookpo.itempo    = po-ordl.i-no
                    ttPoLoadtagLookpo.itemname  = po-ordl.i-name .
                END. /*FOR EACH po-ord*/
            END.
         end.  /* if prmField = state  */
         if prmField = "vend"  then do:
            if prmCondition = "EQUAL" then do:
                FOR EACH po-ord WHERE po-ord.company = prmComp 
                    AND  po-ord.vend-no = (prmtext)
                    AND  asi.po-ord.opened  = yes NO-LOCK, 
                    FIRST po-ordl WHERE po-ordl.company eq po-ord.company and 
                    po-ordl.po-no eq po-ord.po-no 
                    AND po-ordl.deleted = no NO-LOCK BY po-ord.po-no
                                                     BY po-ord.vend-no
                                                     BY po-ord.po-date :

                    create ttPoLoadtagLookpo.
                    assign                                     
                    ttPoLoadtagLookpo.ponopo    = po-ord.po-no
                    ttPoLoadtagLookpo.podatepo  = po-ord.po-date
                    ttPoLoadtagLookpo.vendorpo  = po-ord.vend-no
                    ttPoLoadtagLookpo.job       = po-ordl.job-no
                    ttPoLoadtagLookpo.job2      = po-ordl.job-no2
                    ttPoLoadtagLookpo.itempo    = po-ordl.i-no
                    ttPoLoadtagLookpo.itemname  = po-ordl.i-name .
                END. /*FOR EACH po-ord*/
            END.
            if prmCondition = "BEGINS" then do:
                FOR EACH po-ord WHERE po-ord.company = prmComp 
                    AND  po-ord.vend-no BEGINS (prmtext)
                    AND  asi.po-ord.opened  = yes NO-LOCK, 
                    FIRST po-ordl WHERE po-ordl.company eq po-ord.company and 
                    po-ordl.po-no eq po-ord.po-no 
                    AND po-ordl.deleted = no NO-LOCK BY po-ord.po-no
                                                     BY po-ord.vend-no
                                                     BY po-ord.po-date :

                    create ttPoLoadtagLookpo.
                    assign                                     
                    ttPoLoadtagLookpo.ponopo    = po-ord.po-no
                    ttPoLoadtagLookpo.podatepo  = po-ord.po-date
                    ttPoLoadtagLookpo.vendorpo  = po-ord.vend-no
                    ttPoLoadtagLookpo.job       = po-ordl.job-no
                    ttPoLoadtagLookpo.job2      = po-ordl.job-no2
                    ttPoLoadtagLookpo.itempo    = po-ordl.i-no
                    ttPoLoadtagLookpo.itemname  = po-ordl.i-name .
                END. /*FOR EACH po-ord*/
            END.
         end.  /* if prmField = state  */
    END.  /* IF prmAction = search then do: */



