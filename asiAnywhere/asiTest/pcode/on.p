  
/*------------------------------------------------------------------------
    File        : QuoteDetail.p
    Purpose     : OrderOnHand

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{on.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction as Character no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderFg1.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.


IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        
        RUN build-qry .
        
        
        DATASET dsOrderFg1:FILL().
    END.
    
END CASE.    
PROCEDURE build-qry:

FIND FIRST oe-ordl where oe-ordl.ord-no = int(prmOrderNum) and oe-ordl.i-no = prmItemNum  no-lock No-Error.
For each itemfg no-Lock :
FIND FIRST fg-bin where fg-bin.company eq oe-ordl.company     
           no-lock no-error.
     If available fg-bin then do:
     assign
     ttOrderFg1.q-onh = itemfg.q-onh
     ttOrderFg1.q-ono = itemfg.q-ono
     ttOrderFg1.q-alloc = itemfg.q-alloc
     ttOrderFg1.q-back = itemfg.q-back
     ttOrderFg1.reorder = itemfg.ord-level
     ttOrderFg1.q-avail = (itemfg.q-onh +  itemfg.q-ono - itemfg.q-alloc)
     ttOrderFg1.reorderMin = itemfg.ord-min
     ttOrderFg1.reorderMax = itemfg.ord-max   .
End. /*If available fg-bin*/

       
        END.   /*FOR EACH itemfg*/
END PROCEDURE.
