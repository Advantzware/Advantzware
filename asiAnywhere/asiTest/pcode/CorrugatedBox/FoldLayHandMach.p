/*------------------------------------------------------------------------
    File      : FoldLayHandMach.p
    Purpose   :  Corrugated Machine Lookup
    Syntax    :

    Description : Return a Dataset of MachineLookup

    Author(s)   : 
    Created     : 21 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttFoldHandMachLookup NO-UNDO 
        FIELD lsh-len   AS DECIMAL
        FIELD lsh-wid   AS DECIMAL
        FIELD gsh-len   AS DECIMAL
        FIELD gsh-wid   AS DECIMAL
        FIELD nsh-len   AS DECIMAL
        FIELD nsh-wid   AS DECIMAL
        FIELD trim-l   AS DECIMAL
        FIELD trim-w   AS DECIMAL
        FIELD n-out   AS DECIMAL
        FIELD n-out-l   AS DECIMAL
        FIELD n-cuts   AS DECIMAL
        FIELD gsh-dep   AS DECIMAL
        FIELD nsh-dep   AS DECIMAL
        FIELD trim-d   AS DECIMAL
        FIELD n-out-d  AS DECIMAL
        FIELD die-in   AS DECIMAL
        FIELD num-wid  AS DECIMAL 
        FIELD num-len  AS DECIMAL
        FIELD num-up  AS DECIMAL
        FIELD roll AS CHAR 
        FIELD roll-wid AS DECIMAL 
        FIELD mdesc     AS CHAR
        FIELD bcal      AS DECIMAL 
        FIELD hjkjdj AS INT
        
        .
    
    DEFINE DATASET dsFoldHandMachLookup FOR ttFoldHandMachLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBoard     AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmStyle     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMach      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmXgrain    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRoll      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFront     AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSide      AS DECIMAL NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldHandMachLookup .

IF prmAction    = ? THEN ASSIGN prmAction     = "".
IF prmUser      = ? THEN ASSIGN prmUser       = "".
IF prmEstimate  = ? THEN ASSIGN prmEstimate   = "".
IF prmForm      = ? THEN ASSIGN prmForm       = 0.
IF prmBlankno   = ? THEN ASSIGN prmBlankno    = 0.
IF prmBoard     = ? THEN ASSIGN prmBoard      = "".
IF prmStyle     = ? THEN ASSIGN prmStyle      = "".
IF prmMach      = ? THEN ASSIGN prmMach       = "".
IF prmXgrain    = ? THEN ASSIGN prmXgrain     = "".
IF prmRoll      = ? THEN ASSIGN prmRoll       = "".
IF prmFront     = ? THEN ASSIGN prmFront      = 0.
IF prmSide      = ? THEN ASSIGN prmSide       = 0.

DEFINE VAR prmComp AS CHAR NO-UNDO.

{sys/inc/var.i NEW shared}

def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp 
    locode = "MAIN" .

 /**********************************prmAction when folding machine*******************/
IF prmAction = "FoldMachine"  THEN DO:
   
   FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
   FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.

     find xef where recid(xef) = recid(ef).
       find xeb where recid(xeb) = recid(eb).
       assign xef.m-code = prmMach
              xef.lsh-lock = no
              xef.lsh-len = dec(prmFront)
              xef.lsh-wid = dec(PrmSide)
              xef.n-out = 0
              xef.n-out-l = 0
              xef.board = prmBoard 
              xef.xgrain = prmXgrain
              xef.roll   = IF prmRoll = "Yes" THEN TRUE ELSE FALSE
              .
       
       run ce/calc-dim.p.
       find xef where recid(xef) = recid(ef).
       find xeb where recid(xeb) = recid(eb).
       CREATE ttFoldHandMachLookup .
       assign ttFoldHandMachLookup.lsh-len = ( xef.lsh-len )
              ttFoldHandMachLookup.lsh-wid = (xef.lsh-wid )
              ttFoldHandMachLookup.gsh-len = (xef.gsh-len )
              ttFoldHandMachLookup.gsh-wid = (xef.gsh-wid )
              ttFoldHandMachLookup.nsh-len = (xef.nsh-len)
              ttFoldHandMachLookup.nsh-wid = (xef.nsh-wid )
              ttFoldHandMachLookup.trim-l = (xef.trim-l )
              ttFoldHandMachLookup.trim-w = (xef.trim-w )
              ttFoldHandMachLookup.n-out = (xef.n-out)
              ttFoldHandMachLookup.n-cuts = (xef.n-cuts)              
              ttFoldHandMachLookup.die-in = (xef.die-in)
              ttFoldHandMachLookup.num-wid = (xeb.num-wid)
              ttFoldHandMachLookup.num-len = (xeb.num-len)
              ttFoldHandMachLookup.num-up = (xeb.num-up).

       FIND FIRST mach WHERE  mach.company = prmComp AND mach.m-code = prmMach  NO-LOCK NO-ERROR.
        IF AVAIL mach THEN DO:
             ASSIGN
                ttFoldHandMachLookup.mdesc =  mach.m-dscr    .
        END.
END.

IF prmAction = "FoldBoard" THEN DO:

    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
   FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.

           find xef where recid(xef) = recid(ef).
           find xeb where recid(xeb) = recid(eb).
           assign xef.m-code = prmMach
              xef.lsh-lock = no
              xef.board = prmBoard
              xef.xgrain = prmXgrain
              xef.roll   = IF prmRoll = "Yes" THEN TRUE ELSE FALSE .
              .
       
           run ce/calc-dim.p.
           find xef where recid(xef) = recid(ef).
           find xeb where recid(xeb) = recid(eb).
           CREATE ttFoldHandMachLookup .
           assign ttFoldHandMachLookup.lsh-len = ( xef.lsh-len )
              ttFoldHandMachLookup.lsh-wid = (xef.lsh-wid )
              ttFoldHandMachLookup.gsh-len = (xef.gsh-len )
              ttFoldHandMachLookup.gsh-wid = ( xef.gsh-wid )
              ttFoldHandMachLookup.nsh-len = ( xef.nsh-len )
              ttFoldHandMachLookup.nsh-wid = ( xef.nsh-wid )
              ttFoldHandMachLookup.trim-l = ( xef.trim-l )
              ttFoldHandMachLookup.trim-w = ( xef.trim-w )
              ttFoldHandMachLookup.n-out = (xef.n-out)
              ttFoldHandMachLookup.n-cuts = (xef.n-cuts)
              ttFoldHandMachLookup.roll = IF xef.roll THEN "Y" ELSE "N"
              ttFoldHandMachLookup.roll-wid = IF xef.roll THEN (xef.roll-wid) ELSE 0
             
              ttFoldHandMachLookup.num-wid = (xeb.num-wid)
              ttFoldHandMachLookup.num-len = (xeb.num-len)
              ttFoldHandMachLookup.num-up =  (xeb.num-up).

            FIND FIRST item WHERE item.company = prmComp AND ITEM.i-no = prmBoard NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN DO:
                ASSIGN
                    ttFoldHandMachLookup.bcal =  ITEM.cal   .
            END.

END.
