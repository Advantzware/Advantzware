
/*------------------------------------------------------------------------
    File        : NewFGFromEst.p
    Purpose     : Accept a blank (eb) and an FG Item# and returns the rowid of the new finished good.

    Syntax      :

    Description : Builds a new FG Item based on estimate.  If FG Item # is not provided, the Item will be auto-created based on FGItem# rules		

    Author(s)   : BV
    Created     : Sun Feb 25 13:22:12 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opriItemfg AS ROWID NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetFGNameFormat RETURNS CHARACTER 
    (ipcCompany AS CHARACTER ) FORWARD.

FUNCTION fGetFreightClass RETURNS LOGICAL 
    (ipcCompany AS CHARACTER ) FORWARD.

/* ***************************  Main Block  *************************** */
FIND FIRST eb 
    WHERE ROWID(eb) EQ ipriEb
    NO-ERROR.
IF NOT AVAILABLE eb THEN RETURN. 
FIND FIRST est NO-LOCK 
    WHERE est.company EQ eb.company 
    AND est.est-no EQ eb.est-no 
    NO-ERROR.
IF NOT AVAILABLE est THEN RETURN.
RUN pAddFGItem (BUFFER est, BUFFER eb, ipcFGItemID, OUTPUT opriItemfg).


/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddFGItem:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE PARAMETER BUFFER ipbf-eb  FOR eb.
    DEFINE INPUT PARAMETER ipcFGItem AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriItemFG AS ROWID.


    IF ipcFGItem EQ "" THEN 
        RUN pGetFGItem(BUFFER ipbf-eb, ipbf-est.est-type, OUTPUT ipcFGItem).
    IF ipcFGItem NE "" THEN 
        RUN pCreateFGItem(BUFFER ipbf-eb, ipbf-est.est-type, ipcFGItem, OUTPUT opriItemFG).
    

END PROCEDURE.

PROCEDURE pCreateFGItem:
    /*------------------------------------------------------------------------------
     Purpose:  Creates FG Item based on eb spec and provided item number.
                Returns the ROWID of the FG Item just created.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT PARAMETER ipiEstType AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemFGID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriItemFG AS ROWID NO-UNDO.

    FIND FIRST cust NO-LOCK   
        WHERE cust.company EQ ipbf-eb.company
        AND cust.cust-no EQ ipbf-eb.cust-no
        NO-ERROR.

    CREATE itemfg.
    ASSIGN
        itemfg.company    = ipbf-eb.company
        itemfg.loc        = ipbf-eb.loc
        itemfg.i-no       = ipcItemFGID
        itemfg.i-code     = "C"
        itemfg.i-name     = ipbf-eb.part-dscr1
        itemfg.part-dscr1 = ipbf-eb.part-dscr2
        itemfg.sell-uom   = "M"
        itemfg.part-no    = ipbf-eb.part-no
        itemfg.cust-no    = ipbf-eb.cust-no
        itemfg.cust-name  = IF AVAILABLE cust THEN cust.name ELSE ""
        itemfg.pur-uom    = IF ipbf-eb.pur-man THEN "EA" ELSE "M"
        itemfg.prod-uom   = IF ipbf-eb.pur-man THEN "EA" ELSE "M"
        itemfg.stocked    = YES
        itemfg.die-no     = ipbf-eb.die-no
        itemfg.plate-no   = ipbf-eb.plate-no
        itemfg.style      = ipbf-eb.style
        itemfg.procat     = ipbf-eb.procat
        itemfg.cad-no     = ipbf-eb.cad-no
        itemfg.upc-no     = ipbf-eb.upc-no
        itemfg.spc-no     = ipbf-eb.spc-no
        itemfg.isaset     = (ipiEstType EQ 2 OR ipiEstType EQ 6) AND
                     ipbf-eb.form-no EQ 0
        itemfg.pur-man    = ipbf-eb.pur-man 
        itemfg.alloc      = ipbf-eb.set-is-assembled.
    
    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").
    
    RUN pSetFreightClass( BUFFER itemfg, BUFFER ipbf-eb).

    /* gdm - 11190901 */
    IF ipbf-eb.ship-id NE "" THEN 
    DO:
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ ipbf-eb.company
            AND shipto.cust-no EQ ipbf-eb.cust-no
            AND shipto.ship-id EQ ipbf-eb.ship-id NO-ERROR.
        IF AVAILABLE shipto THEN ASSIGN itemfg.ship-meth = shipto.ship-meth.
    END.
    /* gdm - 11190901 end */

    IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

    {fg/set-inks1.i itemfg ipbf-eb}
 
    {sys/inc/fgcascnt.i itemfg ipbf-eb}
   
    {est/fgupdtax.i ipbf-eb}
    
    opriItemFG = ROWID(itemfg).
    RELEASE itemfg.
    

END PROCEDURE.

PROCEDURE pGetFGITem:
    /*------------------------------------------------------------------------------
     Purpose: Returns the FGItem based on auto generation rules
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT PARAMETER ipiEstType AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFGItem AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cFGFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf-eb FOR eb.

    cFGFormat = fGetFGNameFormat(ipbf-eb.company).
    IF cFGFormat = "Hughes" THEN 
        cFGFormat = "".
    ELSE IF cFGFormat = "Fibre" THEN 
            cFGFormat = "". 
    RUN fg/autofg.p (ROWID(ipbf-eb),
        cFGFormat, 
        ipbf-eb.procat,
        IF ipiEstType LE 4 THEN "F" ELSE "C",
        ipbf-eb.cust-no,
        OUTPUT opcFGItem).

END PROCEDURE.

PROCEDURE pSetFreightClass:
    /*------------------------------------------------------------------------------
     Purpose: Sets the FG Item Freight Class if NK1 FreightClass is Set
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.
    DEFINE PARAMETER BUFFER ipbf-eb     FOR eb.

    IF fGetFreightClass(ipbf-eb.company) 
        AND ipbf-eb.dest-code GT "" 
        AND ipbf-itemfg.frt-class = "" THEN 
    DO:
        FIND FIRST carr-mtx NO-LOCK  
            WHERE carr-mtx.company EQ ipbf-eb.company
            AND carr-mtx.loc    EQ  ipbf-eb.loc
            AND carr-mtx.carrier EQ ipbf-eb.carrier
            AND carr-mtx.del-zone EQ ipbf-eb.dest-code
            NO-ERROR.
        ipbf-itemfg.frt-class = ipbf-eb.dest-code.
        IF AVAILABLE carr-mtx THEN
            ipbf-itemfg.frt-class-dscr = carr-mtx.del-dscr.
    END.


END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fGetFGNameFormat RETURNS CHARACTER 
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the character value of FGITem#
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys\ref\nk1look.p (ipcCompany,
        "FGITEM#",
        "C",
        NO,
        NO,
        "",
        "", 
        OUTPUT cReturn,
        OUTPUT lFound).

    RETURN cReturn.
		
END FUNCTION.

FUNCTION fGetFreightClass RETURNS LOGICAL 
    (ipcCompany AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns yes/no if NK1 FGFreightClass is active
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys\ref\nk1look.p (ipcCompany,
        "FGFreightClass",
        "L",
        NO,
        NO,
        "",
        "", 
        OUTPUT cReturn,
        OUTPUT lFound).

    RETURN lFound AND cReturn EQ "YES".
		
END FUNCTION.
