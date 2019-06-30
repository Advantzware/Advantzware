

/*------------------------------------------------------------------------
    File        : BinItemUp.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BinItemUp.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemJobUp.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "Select" THEN DO:
    FIND FIRST itemfg where itemfg.company EQ prmComp AND itemfg.i-no = prmItemNum no-lock NO-ERROR.
    IF AVAILABLE itemfg THEN
    DO:
       create ttItemJobUp.
       assign
           ttItemJobUp.i-no             = itemfg.i-no
           ttItemJobUp.i-name           = itemfg.i-name
           ttItemJobUP.cust             = itemfg.cust-no
           ttItemJobUp.q-onh            = itemfg.q-onh
           ttItemJobUp.q-ono            = itemfg.q-ono
           ttItemJobUp.q-alloc          = itemfg.q-alloc
           ttItemJobUp.q-back           = itemfg.q-back
           ttItemJobUp.q-avail          = itemfg.q-avail
           ttItemJobUp.ord-level        = itemfg.ord-level 
           ttItemJobUp.ord-min          = itemfg.ord-min 
           ttItemJobUp.ord-max          = itemfg.ord-max
           .
    END.
END.





