/*------------------------------------------------------------------------
    File        : SetPart.p
    Purpose     : Order Mentations

    Syntax      :

    Description : Return a Dataset for fgitem

    Author(s)   : 
    Created     : 24  july 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttSetPartFGitem NO-UNDO
    FIELD vQty             AS INT
    FIELD vFGPart          AS CHAR
    FIELD vName            AS CHARACTER
    FIELD vOnHand          AS INT
    FIELD vPoJob           AS INT
    FIELD vToOrder         AS INT
    FIELD vBackOrder       AS INT
    FIELD vAvail           AS INT
    FIELD vReckey          LIKE fg-set.rec_key
    FIELD vSno             LIKE fg-set.s-no
    FIELD vLine            LIKE fg-set.LINE
    FIELD vSetno           LIKE fg-set.set-no

    FIELD itemfg           AS CHAR
    FIELD itemfgname       AS CHAR
    FIELD itemdscr         AS CHAR
    FIELD itemqhand        AS INT
        .

DEFINE DATASET dsSetPartFGitem FOR ttSetPartFGitem.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmQty         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmNewFgItem   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmName        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOnHand       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPoJob        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmToOrder      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBackOrder    AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmAvail          AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmFGItem       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSetPartFGitem .
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser         = ?  THEN ASSIGN    prmUser          = "".
IF prmAction       = ?  THEN ASSIGN    prmAction        = "".
IF prmComp         = ?  THEN ASSIGN    prmComp          = "".
IF prmQty          = ?  THEN ASSIGN    prmQty           = 0.
IF prmNewFgItem    = ?  THEN ASSIGN    prmNewFgItem     = "".
IF prmName         = ?  THEN ASSIGN    prmName          = "".
IF prmOnHand       = ?  THEN ASSIGN    prmOnHand        = 0.
IF prmPoJob        = ?  THEN ASSIGN    prmPoJob         = 0.
IF prmToOrder      = ?  THEN ASSIGN    prmToOrder       = 0.
IF prmBackOrder    = ?  THEN ASSIGN    prmBackOrder     = 0.
IF prmAvail        = ?  THEN ASSIGN    prmAvail         = 0.
IF prmReckey       = ?  THEN ASSIGN     prmReckey        = "".
IF prmFGItem       = ?  THEN ASSIGN     prmFGItem        = "".



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


/*************************main block****************************/
IF prmAction = "Itemfg" THEN DO:
    FIND FIRST itemfg  WHERE itemfg.company EQ prmComp 
        AND itemfg.i-no    EQ prmFGItem  NO-LOCK NO-ERROR.
    IF AVAIL itemfg  THEN DO:
        CREATE ttSetPartFGitem.
            ASSIGN
                 ttSetPartFGitem.itemfg             = itemfg.i-no
                 ttSetPartFGitem.itemfgname         = itemfg.i-name
                 ttSetPartFGitem.itemdscr           = itemfg.i-dscr
                 ttSetPartFGitem.itemqhand          =   itemfg.q-onh  .
                

    END.
END.
 

/*************************************function***************************************************/  

FUNCTION get-itemfg RETURNS CHARACTER
  () :
  DEF BUFFER b-itemfg FOR itemfg.

  ASSIGN
   lv-q-onh = 0
   lv-q-ono = 0
   lv-q-all = 0
   lv-q-bak = 0.

  FIND FIRST b-itemfg
      WHERE b-itemfg.company EQ fg-set.company
        AND b-itemfg.i-no    EQ fg-set.part-no
      NO-LOCK NO-ERROR.
  IF AVAIL b-itemfg THEN
    ASSIGN
     lv-q-onh = b-itemfg.q-onh
     lv-q-ono = b-itemfg.q-ono
     lv-q-all = b-itemfg.q-alloc
     lv-q-bak = b-itemfg.q-back.

  ASSIGN
     lv-q-avl = lv-q-onh + lv-q-ono - lv-q-all .

  RETURN (IF AVAIL b-itemfg THEN b-itemfg.i-name ELSE "").

END FUNCTION.


/*****************end function*****************/


IF prmAction = "Add" THEN DO:

    

  find last b-fg-set use-index s-no no-lock no-error.
  x = if avail b-fg-set then b-fg-set.s-no + 1 else 1.
  find last b-fg-set where b-fg-set.set-no = prmFGItem no-lock no-error.
  y = if avail b-fg-set then b-fg-set.line + 1 else 1.
  
  CREATE fg-set.
      assign
           fg-set.company      = prmComp
           fg-set.set-no       = prmFGItem
           fg-set.s-no         = x
           fg-set.line          = y
           fg-set.part-qty      = prmQty
           fg-set.part-no       = prmNewFgItem
         .
      ASSIGN
        prmReckey = fg-set.rec_key
        prmAction = "View".
END.


IF prmAction = "Delete" THEN DO:
    
   FIND FIRST fg-set WHERE fg-set.company = prmComp 
        AND fg-set.set-no = prmFGItem AND fg-set.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL fg-set THEN DO:
       DELETE fg-set.
   END.

    FIND LAST fg-set WHERE fg-set.company = prmComp 
        AND fg-set.set-no = prmFGItem  NO-LOCK NO-ERROR.
    IF AVAIL fg-set THEN
        ASSIGN
         prmReckey = fg-set.rec_key 
        prmAction  = "View"  .

END.

IF prmAction = "Update"  THEN DO:
    FIND FIRST fg-set WHERE fg-set.company = prmComp 
        AND fg-set.set-no = prmFGItem AND fg-set.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
        get-itemfg() .
        IF AVAIL fg-set  THEN DO:
            ASSIGN
                fg-set.part-qty = prmQty.
        END.
   
    ASSIGN
        prmAction = "View".

END.


IF prmAction = "Select"  THEN DO:
    FOR EACH fg-set WHERE fg-set.company = prmComp 
        AND fg-set.set-no = prmFGItem NO-LOCK :
        get-itemfg() .
        IF AVAIL fg-set  THEN DO:

            CREATE ttSetPartFGitem.
            ASSIGN
                 ttSetPartFGitem.vQty           = fg-set.part-qty
                 ttSetPartFGitem.vFGPart        = fg-set.part-no
                 ttSetPartFGitem.vName          = get-itemfg()  
                 ttSetPartFGitem.vOnHand        =   lv-q-onh
                 ttSetPartFGitem.vPoJob         =   lv-q-ono
                 ttSetPartFGitem.vToOrder       =   lv-q-all
                 ttSetPartFGitem.vBackOrder     =   lv-q-bak
                 ttSetPartFGitem.vAvail         =   lv-q-avl

                 ttSetPartFGitem.vReckey         =  fg-set.rec_key
                 ttSetPartFGitem.vSno       =  fg-set.s-no
                 ttSetPartFGitem.vLine     =   fg-set.LINE
                 ttSetPartFGitem.vSetno         =   fg-set.set-no
                                    .  

           
                
        END.
    END.
END.

IF prmAction = "View"  THEN DO:
  
    FOR EACH fg-set WHERE fg-set.company = prmComp 
        AND fg-set.set-no = prmFGItem AND fg-set.rec_key = prmReckey NO-LOCK :
        get-itemfg() .
        IF AVAIL fg-set  THEN DO:

            CREATE ttSetPartFGitem.
            ASSIGN
                 ttSetPartFGitem.vQty           = fg-set.part-qty
                 ttSetPartFGitem.vFGPart        = fg-set.part-no
                 ttSetPartFGitem.vName          = get-itemfg()  
                 ttSetPartFGitem.vOnHand        =   lv-q-onh
                 ttSetPartFGitem.vPoJob         =   lv-q-ono
                 ttSetPartFGitem.vToOrder       =   lv-q-all
                 ttSetPartFGitem.vBackOrder     =   lv-q-bak
                 ttSetPartFGitem.vAvail         =   lv-q-avl
                ttSetPartFGitem.vReckey         =  fg-set.rec_key
                 ttSetPartFGitem.vSno       =  fg-set.s-no
                 ttSetPartFGitem.vLine     =   fg-set.LINE
                 ttSetPartFGitem.vSetno         =   fg-set.set-no
                                    .   
        END.
        END.
END.


/*********************ptocdure***********************/
