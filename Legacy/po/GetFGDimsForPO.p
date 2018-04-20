
/*------------------------------------------------------------------------
    File        : GetFGDimsForPO.p
    Purpose     : Encapsulates logic for initializing the L W D for a FG Item on a PO.  Includes logic for NK1 POFGDims

    Syntax      :

    Description : Given a FG, procedure will return the L, W, D to use on a PO.	

    Author(s)   : BV
    Created     : Thu Apr 19 19:23:30 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriItemfg AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opdLength AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdWidth AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdDepth AS DECIMAL NO-UNDO.



/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fGetStyleType RETURNS CHARACTER 
	(ipcCompany AS CHARACTER,
	 ipcStyleCode AS CHARACTER) FORWARD.

FUNCTION fUseLWD RETURNS LOGICAL 
	( ipcCompany AS CHARACTER , 
	  ipcStyleType AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */
FIND FIRST itemfg NO-LOCK 
    WHERE ROWID(itemfg) EQ ipriItemfg
    NO-ERROR.
IF AVAILABLE itemfg THEN DO:
    IF fUseLWD(itemfg.company, fGetStyleType(itemfg.company, itemfg.style)) THEN 
        ASSIGN 
            opdLength = itemfg.l-score[50]
            opdWidth = itemfg.w-score[50]
            opdDepth = itemfg.d-score[50]
            .
    ELSE
        ASSIGN 
            opdLength = itemfg.t-len
            opdWidth = itemfg.t-wid
            opdDepth = 0  /*Refactor: itemfg.t-dep is field but does not appear to be used*/
            .
END.       

/* ************************  Function Implementations ***************** */

FUNCTION fGetStyleType RETURNS CHARACTER 
	(ipcCompany AS CHARACTER, 
	 ipcStyleCode AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose: Returns the Style Type given the Style Code
 Notes:
------------------------------------------------------------------------------*/	
DEFINE VARIABLE cStyleType AS CHARACTER NO-UNDO.

FIND FIRST style NO-LOCK 
    WHERE style.company EQ ipcCompany
    AND style.style EQ ipcStyleCode
    NO-ERROR.
IF AVAILABLE style THEN 
    cStyleType = style.type.
ELSE 
    cStyleType = "".
    
RETURN cStyleType.   
    
		
END FUNCTION.

FUNCTION fUseLWD RETURNS LOGICAL 
	( ipcCompany AS CHARACTER , 
	  ipcStyleType AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Finds the NK1 POFGDims and, given the style *type* of the item, returns whether 
 the system should return the inner box dimensions or the blank width and length
 Notes:  Blank Width and Length are current default
------------------------------------------------------------------------------*/	
DEFINE VARIABLE cUseLWD AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseLWD AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSpecificStyleType AS CHARACTER NO-UNDO.

 RUN sys\ref\nk1look.p (ipcCompany,
        'POFGDims',
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cSpecificStyleType,
        OUTPUT lUseLWD).

RUN sys\ref\nk1look.p (ipcCompany,
        'POFGDims',
        'L',
        NO,
        NO,
        '',
        '', 
        OUTPUT cUseLWD,
        OUTPUT lUseLWD).
        
lUseLWD = cUseLWD EQ "YES".
IF lUseLWD AND cSpecificStyleType NE "" THEN 
    lUseLWD = CAN-DO(cSpecificStyleType,ipcStyleType) OR ipcStyleType EQ "".
		
RETURN lUseLWD.
		
END FUNCTION.
