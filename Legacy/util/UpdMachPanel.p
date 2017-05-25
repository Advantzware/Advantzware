
/*------------------------------------------------------------------------
    File        : UpdMachPanel.p
    Purpose     : 

    Syntax      :

    Description : Update machine panels based on char value of CEPanel

    Author(s)   : BV
    Created     : Wed May 24:36:35 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fCEPanels RETURNS LOGICAL 
	( ipcCompany AS CHARACTER  ) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pUpdateMachPanels.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pUpdateMachPanels:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH company NO-LOCK:
              
        IF fCEPanels(company.company) THEN DO:
            FOR EACH mach EXCLUSIVE-LOCK 
            WHERE mach.company EQ company.company:
                ASSIGN 
                    mach.max_hd_hd = mach.max-pan-w
                    mach.min_hd_hd = mach.min-pan-w
                    mach.max_slot_score = mach.max-pan-l
                    mach.min_slot_score = mach.min-pan-l
                    mach.max-pan-w = 0
                    mach.min-pan-w = 0
                    mach.max-pan-l = 0
                    mach.min-pan-l = 0
                    .
            END.
        END.
        FIND FIRST sys-ctrl EXCLUSIVE-LOCK
            WHERE sys-ctrl.company EQ company.company
            AND sys-ctrl.name EQ 'CEPanel'
            NO-ERROR.
        IF AVAILABLE sys-ctrl THEN 
            sys-ctrl.char-fld = "".
    END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fCEPanels RETURNS LOGICAL 
	( ipcCompany AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Checks the CEPanels NK1, cleans it up and returns True if data needs converted
 Notes:  Conversion only needed if the CEPanel Char Value is PminPmax
------------------------------------------------------------------------------*/	
		DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
		DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany,
                           "CEPANEL",
                            "L",
                            NO,
                            NO,
                            "",
                            "",
                            OUTPUT cResult,
                            OUTPUT lFound).
    IF cResult EQ "YES" THEN DO:
        RUN sys/ref/nk1look.p (ipcCompany,
                               "CEPANEL",
                                "C",
                                NO,
                                NO,
                                "",
                                "",
                                OUTPUT cResult,
                                OUTPUT lFound).
        lResult = cResult EQ "PminPmax".  
    END.
    ELSE 
        lResult = NO.
         
	RETURN lResult.

		
END FUNCTION.



