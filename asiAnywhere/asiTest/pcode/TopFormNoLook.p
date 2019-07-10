/*------------------------------------------------------------------------
    File        : TopFormNoLook.p
    Purpose     : TopFormNoLook

    Syntax      :

    Description : Return a Dataset of all FormNo

    Author(s)   : 
    Created     : july 31, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTopFormNoLook NO-UNDO
    FIELD vEst AS CHAR FORMAT "x(8)":U
    FIELD vQty AS DECIMAL FORMAT "->>,>>9.99":U
    FIELD vForm AS INT FORMAT ">9":U 
    FIELD asds AS CHAR   
    FIELD sfd AS CHAR   
    FIELD assdtds AS CHAR   .

DEFINE DATASET dsTopFormNoLook FOR ttTopFormNoLook.

DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNo     AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopFormNoLook.

IF prmUser  = ? THEN ASSIGN prmUser = "".
IF prmComp  = ? THEN ASSIGN prmComp = "".
IF prmEstNo = ? THEN ASSIGN prmEstNo  = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FOR EACH ef WHERE ef.company = prmComp and ef.est-no = prmEstNo NO-LOCK:

    CREATE ttTopFormNoLook.
    ASSIGN 
        ttTopFormNoLook.vEst = ef.est-no
        ttTopFormNoLook.vQty = ef.eqty
        ttTopFormNoLook.vForm = ef.form-no
        .
END.
