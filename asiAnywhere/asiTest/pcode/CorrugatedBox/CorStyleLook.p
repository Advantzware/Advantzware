

/*------------------------------------------------------------------------
    File         : CorStyleLook
    Purpose     :  Style Lookup

    Syntax      :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : Feb 2 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorStyleLook NO-UNDO 
        FIELD vStyle     AS CHARACTER
        FIELD vStyleDscr AS CHARACTER
        FIELD vboard     AS CHAR
    .

DEFINE DATASET dsCorStyleLook FOR ttCorStyleLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInd       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorStyleLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmInd       = ? THEN ASSIGN prmInd       = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH style WHERE style.company = prmComp and style.style <> "" AND style.industry = prmInd NO-LOCK :
        IF AVAIL style  THEN DO:
            create ttCorStyleLook.
            assign                                     
                ttCorStyleLook.vStyle      = style.style
                ttCorStyleLook.vStyleDscr  = style.dscr  
                ttCorStyleLook.vboard      = style.material[1]
                .

        END. /*if avail style*/
    END. /* FOR EACH style WHERE */
END.  /*if prmAction <> "search" */

IF prmAction = "search" then do:
    IF prmCondition = "EQUAL" then do:
         FOR EACH style WHERE style.company = prmComp and style.style= prmText AND style.industry = prmInd NO-LOCK :     
             IF AVAIL style THEN DO:
                  create ttCorStyleLook.
                  assign                                     
                      ttCorStyleLook.vStyle      = style.style
                      ttCorStyleLook.vStyleDscr  = style.dscr 
                      ttCorStyleLook.vboard      = style.material[1]
                .
                      .
               END. /*if avail style*/
           END.   /*FOR EACH style*/
       END.   /* IF prmCondition = "EQUAL"*/

    IF prmCondition = "BEGIN" then do:
             FOR EACH style WHERE style.company = prmComp and style.style BEGINS prmText AND style.industry = prmInd NO-LOCK :     
                 IF AVAIL style THEN DO:
                  create ttCorStyleLook.
                  assign                                     
                     ttCorStyleLook.vStyle      = style.style
                     ttCorStyleLook.vStyleDscr  = style.dscr 
                      ttCorStyleLook.vboard      = style.material[1]
                .
                      .
               END. /*if avail style*/
           END.   /*FOR EACH style*/
       END.   /* IF prmCondition = "begin"*/
END. /*IF prmAction = "search" then do:*/
          


