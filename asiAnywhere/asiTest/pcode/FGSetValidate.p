

                                 
/*------------------------------------------------------------------------
    File        : FGSetValidate.p
    Purpose     : Validation for add order

    Syntax      :

    Description : Validation for add order

    Author(s)   : Sewa
    Created     : july 27, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmQty         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmNewFgItem   AS CHAR NO-UNDO.


DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
IF prmUser         = ?  THEN ASSIGN    prmUser          = "".
IF prmAction       = ?  THEN ASSIGN    prmAction        = "".
IF prmComp         = ?  THEN ASSIGN    prmComp          = "".
IF prmQty          = ?  THEN ASSIGN    prmQty           = 0.
IF prmNewFgItem    = ?  THEN ASSIGN    prmNewFgItem     = "".


DEFINE VAR prmLoc AS CHAR NO-UNDO.

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

def buffer b-fg-set for fg-set.

def var lv-i-name like itemfg.i-name no-undo.
def var lv-q-onh  like itemfg.q-onh no-undo.
def var lv-q-ono  like itemfg.q-ono no-undo.
def var lv-q-all  like itemfg.q-alloc no-undo.
def var lv-q-bak  like itemfg.q-back no-undo.
def var lv-q-avl  like itemfg.q-avail no-undo.

{fg/fullset.i NEW}

DEF WORKFILE w-fg-set LIKE fg-set.
 /* def var x as int no-undo.
  def var y as int no-undo.  */


    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmLoc  =  "MAIN" .
cocode = prmComp.
locode = prmLoc.


IF prmAction = "Add"  THEN DO:

  
    FIND FIRST itemfg
        WHERE itemfg.company EQ prmComp
          AND itemfg.i-no  EQ prmNewFgItem
        NO-LOCK NO-ERROR.

    IF NOT AVAIL itemfg THEN DO: 
        cError = "Invalid FG Item#, try help".
        RETURN.
    END.

    ELSE DO:
      RUN fg/fullset.p (ROWID(itemfg)).

      FIND FIRST tt-fg-set WHERE tt-fg-set.part-no EQ itemfg.i-no NO-ERROR.

      IF AVAIL tt-fg-set THEN
        cError  = "You may not add this FG Item because Set Header already have as a component of Set Part".
         RETURN.
    END.

END.
