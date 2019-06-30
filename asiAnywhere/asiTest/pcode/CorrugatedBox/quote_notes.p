
/*------------------------------------------------------------------------
    File        : quote_notes.p   Copy of(est/v-qtnote.w)
    Purpose     : Quote notes

    Syntax      :

    Description : Return a Dataset of all Quote Items

    Author(s)   : 
    Created     : Aug 26  2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttQuoteQtNotesItems NO-UNDO 
    FIELD qtqty-comment1  AS CHARACTER
    FIELD qtqty-comment2  AS CHARACTER       
    FIELD qtqty-comment3  AS CHARACTER 
    FIELD qtqty-comment4  AS CHARACTER 
    FIELD qtqty-comment5  AS CHARACTER 
    FIELD qtqty-quote     AS INT
    FIELD qtqty-date      AS CHAR
   

    .

DEFINE DATASET dsQuoteQtNotesItems FOR ttQuoteQtNotesItems.

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INTEGER  NO-UNDO.

DEFINE INPUT PARAMETER prmNote1     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmNote2     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmNote3     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmNote4     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmNote5     AS CHAR  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteQtNotesItems .
DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

IF prmUser       = ? THEN ASSIGN prmUser        = "".
IF prmAction     = ? THEN ASSIGN prmAction      = "".
IF prmQuote      = ? THEN ASSIGN prmQuote       = 0.
IF prmNote1      = ? THEN ASSIGN prmNote1          = "".
IF prmNote2      = ? THEN ASSIGN prmNote2          = "".
IF prmNote3      = ? THEN ASSIGN prmNote3          = "".
IF prmNote4      = ? THEN ASSIGN prmNote4          = "".
IF prmNote5      = ? THEN ASSIGN prmNote5          = "".


DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
 
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 prmLoc   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

ASSIGN
    cocode = prmComp 
    locode = prmLoc .
 
/******************update****************************************/

IF prmAction = "Update" THEN DO:
     
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
            ASSIGN
                quotehd.comment[1]    =  prmNote1   
                quotehd.comment[2]    =  prmNote2   
                quotehd.comment[3]    =  prmNote3   
                quotehd.comment[4]    =  prmNote4 
                quotehd.comment[5]    =  prmNote5  .
                
    END.

    ASSIGN
        prmAction = "Select" .

END.
 IF prmAction = "Select" THEN DO:   

    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        create ttQuoteQtNotesItems .
            ASSIGN 
                ttQuoteQtNotesItems.qtqty-comment1   =  quotehd.comment[1]
                ttQuoteQtNotesItems.qtqty-comment2   =  quotehd.comment[2]
                ttQuoteQtNotesItems.qtqty-comment3   =  quotehd.comment[3]
                ttQuoteQtNotesItems.qtqty-comment4   =  quotehd.comment[4]
                ttQuoteQtNotesItems.qtqty-comment5   =  quotehd.comment[5]
                ttQuoteQtNotesItems.qtqty-quote      =  quotehd.q-no
                ttQuoteQtNotesItems.qtqty-date       = STRING(quotehd.quo-date )
               .
        END.
   
END.


