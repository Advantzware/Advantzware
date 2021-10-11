 
 /*------------------------------------------------------------------------
    File        : EstCostHeader
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : gshde
    Created     : Fri Oct 01 22:14:53 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS entities.EstCostHeader  INHERITS entities.Entity: 
    DEFINE TEMP-TABLE ttEstCostHeader 
        LIKE EstCostheader
        BEFORE-TABLE bttEstCostHeader.
    
    DEFINE DATASET dsEstCostHeader FOR ttEstCostHeader.
        
    DEFINE DATA-SOURCE srcEstCostHeader FOR estCostHeader.
    
    DEFINE VARIABLE oEstCostForm AS entities.EstCostForm NO-UNDO.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC EstCostHeader (  ):
		SUPER (DATASET dsEstCostHeader:HANDLE, DATA-SOURCE srcEstCostHeader:HANDLE).
		
        pInit().
	END CONSTRUCTOR.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC EstCostHeader ( ipcCompany AS CHARACTER, ipcJob AS CHARACTER, ipiJobID2 AS INTEGER ):
		
		SUPER (DATASET dsEstCostHeader:HANDLE, DATA-SOURCE srcEstCostHeader:HANDLE).
		
		pInit().
		
		THIS-OBJECT:Find("company eq '" + ipcCompany + "' and jobID eq '" + ipcJob + "' and jobID2 eq " + STRING(ipiJobID2)).

	END CONSTRUCTOR.


  	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC EstCostHeader ( ipiEstCostheaderiD AS INT64 ):
	    
	    SUPER (DATASET dsEstCostHeader:HANDLE, DATA-SOURCE srcEstCostHeader:HANDLE).
	    
	    THIS-OBJECT:Find("estCostHeaderID eq " + STRING(ipiEstCostheaderiD)).
        
        pInit().
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC entities.EstCostForm GetFormObject( ):
		
		RETURN oEstCostForm.

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetEstimates( OUTPUT TABLE FOR ttEstCostHeader):
        
    END METHOD.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL HasRecords(  ):
		
		RETURN TEMP-TABLE ttEstCostHeader:HAS-RECORDS.

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID LoadForms( ipiEstCostHeaderID AS INT64):
        
        oEstCostForm:Read("estCostHeaderID eq " + STRING(ipiEstCostHeaderID)).
            
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pInit(  ):
		
        oEstCostForm = NEW entities.EstCostForm().
        
        IF SEARCH ("entities\estCostHeader.json") NE ? THEN
            LoadAttributes ("entities\estCostHeader.json").

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC EstCostHeader ( ):
        IF VALID-OBJECT (oEstCostForm) THEN
            DELETE OBJECT oEstCostForm.
	END DESTRUCTOR.

END CLASS.