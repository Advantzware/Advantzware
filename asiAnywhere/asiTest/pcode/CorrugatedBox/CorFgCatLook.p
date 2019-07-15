
/*------------------------------------------------------------------------
    File         : CorFgcatLook
    Purpose     :  Category Lookup

    Syntax      :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : Feb 2 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorFgCatLook NO-UNDO 
        FIELD vProcat AS CHARACTER
        FIELD vDscr AS CHARACTER
        FIELD vDdd AS CHARACTER
    .

DEFINE DATASET dsCorFgCatLook FOR ttCorFgCatLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorFgCatLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH fgcat WHERE fgcat.company = prmComp NO-LOCK:
        IF AVAIL fgcat  THEN DO:
            create ttCorFgCatLook.
            assign                                     
                ttCorFgCatLook.vProcat    = fgcat.procat
                ttCorFgCatLook.vDscr      = fgcat.dscr 
                .

        END. /*if avail fgcat*/
    END. /* FOR EACH fgcat WHERE */
END.  /*if prmAction <> "search" */

IF prmAction = "search" then do:
    IF prmCondition = "EQUAL" then do:
             FOR EACH fgcat WHERE fgcat.company = prmComp AND fgcat.procat = prmText NO-LOCK:
                 IF AVAIL fgcat THEN DO:
                  create ttCorFgCatLook.
                  assign                                     
                      ttCorFgCatLook.vProcat    = fgcat.procat
                      ttCorFgCatLook.vDscr      = fgcat.dscr 
                      .
               END. /*if avail fgcat*/
           END.   /*FOR EACH fgcat*/
       END.   /* IF prmCondition = "EQUAL"*/

    IF prmCondition = "BEGIN" then do:
             FOR EACH fgcat WHERE fgcat.company = prmComp AND fgcat.procat BEGINS prmText NO-LOCK:
                 IF AVAIL fgcat THEN DO:
                  create ttCorFgCatLook.
                  assign                                     
                      ttCorFgCatLook.vProcat    = fgcat.procat
                      ttCorFgCatLook.vDscr      = fgcat.dscr 
                      .
               END. /*if avail fgcat*/
           END.   /*FOR EACH fgcat*/
       END.   /* IF prmCondition = "begin"*/
END. /*IF prmAction = "search" then do:*/
          

