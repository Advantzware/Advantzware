/*------------------------------------------------------------------------
    File      : Sheet_Cal.p
    Purpose   :  Corrugated Machine Lookup
    Syntax    :

    Description : Return a Dataset of MachineLookup

    Author(s)   : 
    Created     : 03 dec 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttSheetCalFoldLayOut NO-UNDO 
        
       FIELD sheetCal AS CHAR 
        
        .
    
DEFINE DATASET dsSheetCalFoldLayOut FOR ttSheetCalFoldLayOut .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMach      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmRMitem     AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmSeaItem    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMaxYield   AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSheetCalFoldLayOut .

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmEstimate    = ? THEN ASSIGN prmEstimate    = "".
IF prmMach        = ? THEN ASSIGN prmMach        = "".
IF prmForm        = ? THEN ASSIGN prmForm        = 0.
IF prmBlankno     = ? THEN ASSIGN prmBlankno     = 0.
IF prmRMitem      = ? THEN ASSIGN prmRMitem      = "".
IF prmSeaItem     = ? THEN ASSIGN prmSeaItem     = "".
IF prmMaxYield    = ? THEN ASSIGN prmMaxYield    = "".


DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR vError AS CHAR NO-UNDO.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xest for est.
DEF var k_frac as dec init 6.25 no-undo.
def var lv-is-foam as log no-undo.
DEF TEMP-TABLE w-eb NO-UNDO LIKE eb.
DEF TEMP-TABLE w-ef NO-UNDO LIKE ef.

{cec/bestfitc.i NEW SHARED}
{sys/inc/var.i new shared}

def var ld-gsh-wid as dec no-undo.
def var ld-gsh-len as dec no-undo.
def var ld-gsh-dep as dec no-undo.
def var ld-nsh-wid as dec no-undo.
def var ld-nsh-len as dec no-undo.
def var ld-nsh-dep as dec no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp 
    locode = "MAIN" .


if prmAction =  "sheet-cal" then do:

    FIND FIRST mach WHERE mach.company EQ cocode AND mach.m-code = prmMach NO-LOCK NO-ERROR.
    IF NOT AVAIL mach OR prmMach =  "" THEN DO:
        ASSIGN
            cError =  prmMach + " invalid, try help..." .
            RETURN .
     END.
  
  FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
  FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
            
             find xest where xest.company = ef.company and
                     xest.est-no = ef.est-no
                     no-lock no-error.
             
             find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
       
           run cec/bestfitc.p (prmMach, 0, "",prmRMitem,prmSeaItem,prmMaxYield,OUTPUT vError).
        IF vError <> "" THEN DO:
            ASSIGN
                cError = vError .
                RETURN.
        END.

     FIND FIRST tt-ef NO-ERROR.
     FIND FIRST tt-eb NO-ERROR.

      find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).

        IF AVAIL tt-ef AND AVAIL tt-eb THEN
       assign 
              xef.board = tt-ef.board
              xef.brd-dscr = tt-ef.brd-dscr
              xef.m-code = tt-ef.m-code
              xef.weight = (tt-ef.weight)
              xef.i-code = tt-ef.i-code
              xef.lsh-len = ( tt-ef.lsh-len )
              xef.lsh-wid = ( tt-ef.lsh-wid )
              xef.roll-wid = (tt-ef.roll-wid)
              xef.roll = (tt-ef.roll)
              xef.gsh-len = (tt-ef.gsh-len )
              xef.gsh-wid = ( tt-ef.gsh-wid )
              xef.nsh-len = ( tt-ef.nsh-len )
              xef.nsh-wid = ( tt-ef.nsh-wid )
              xef.trim-l = ( tt-ef.trim-l )
              xef.trim-w = (tt-ef.trim-w )
              xef.n-out = (tt-ef.n-out)
              xef.n-cuts = (tt-ef.n-cuts)
              xeb.num-wid = (tt-eb.num-wid)
              xeb.num-len = (tt-eb.num-len)
              xeb.num-up = (tt-eb.num-up)
              .
    
END.  /*if prmAction <> "sheet cal" then do*/ 

