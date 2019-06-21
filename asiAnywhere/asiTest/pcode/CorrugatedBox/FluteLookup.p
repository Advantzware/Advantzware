/*------------------------------------------------------------------------
    File        : FluteLook.p
    Purpose     : Flute

    Syntax      :

    Description : Return a Dataset of Flute

    Author(s)   : 
    Created     : 19 jan 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFluteLook NO-UNDO 
    FIELD vCode         AS CHARACTER
    FIELD vDescr        AS CHARACTER
    FIELD vThick        AS DECIMAL
    FIELD vClass        AS CHARACTER
    .
    
DEFINE DATASET dsFluteLook FOR ttFluteLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFluteLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmText      = ? THEN ASSIGN prmText      = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".



if prmAction <> "search" then do:
    FOR EACH flute NO-LOCK:
        FIND FIRST ttFluteLook WHERE ttFluteLook.vCode = flute.CODE NO-LOCK NO-ERROR.
        IF AVAIL ttFluteLook THEN NEXT.            
      
        IF flute.CODE <> "" THEN
                 create ttFluteLook.
                 assign                                     
                    ttFluteLook.vCode      = flute.CODE
                    ttFluteLook.vDescr     = flute.dscr
                    ttFluteLook.vThick     = flute.thickness 
                    ttFluteLook.vClass     = flute.CLASS
                     .
        END.  /*FOR EACH flute*/
END.  /*ifif prmAction <> "search" */




 IF prmAction = "search" then do:
     IF prmField = "flute"  then do:
         FOR EACH flute WHERE flute.CODE = prmText  NO-LOCK :
             FIND FIRST ttFluteLook WHERE ttFluteLook.vCode = flute.CODE NO-LOCK NO-ERROR.
               IF AVAIL ttFluteLook THEN NEXT.  
                 create ttFluteLook.
                 assign                                     
                    ttFluteLook.vCode      = flute.CODE
                    ttFluteLook.vDescr     = flute.dscr
                    ttFluteLook.vThick     = flute.thickness 
                    ttFluteLook.vClass     = flute.CLASS
                     .
         END. /*FOR EACH flute*/
     END. /*if prmfield = "code"*/
 END.  /* IF prmAction = search then do: */



