 
 /*------------------------------------------------------------------------
    File        : EstCostForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : gshde
    Created     : Fri Oct 01 22:30:19 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS entities.EstCostForm INHERITS entities.Entity: 
    DEFINE TEMP-TABLE ttEstCostForm
    LIKE EstCostForm
    BEFORE-TABLE bttEstCostForm.

    DEFINE DATASET dsEstCostForm FOR ttEstCostForm.
    
    DEFINE DATA-SOURCE srcEstCostForm FOR EstCostForm.

    DEFINE VARIABLE oEstCostBlank AS entities.EstCostBlank NO-UNDO.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC EstCostForm (  ):
        SUPER (DATASET dsEstCostForm:HANDLE, DATA-SOURCE srcEstCostForm:HANDLE).
        
        pInit().
    END CONSTRUCTOR.
    
     /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC EstCostForm ( ipiEstCostFormID AS INT64 ):
        
        SUPER (DATASET dsEstCostForm:HANDLE, DATA-SOURCE srcEstCostForm:HANDLE).
        
        THIS-OBJECT:Read("estCostFormID eq " + STRING(ipiEstCostFormID) ).
        
        pInit().
    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC entities.EstCostBlank GetBlankObject( ):
		
		RETURN oEstCostBlank.

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetForms( OUTPUT TABLE FOR ttEstCostForm):
        
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    METHOD PUBLIC LOGICAL HasRecords(  ):
        
        RETURN TEMP-TABLE ttEstCostForm:HAS-RECORDS.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID LoadBlanks( ipiEstCostFormID AS INT64):
        
        oEstCostBlank:Read("estCostFormID eq " + STRING(ipiEstCostFormID)).
            
    END METHOD.    

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pInit(  ):
		oEstCostBlank = NEW entities.EstCostBlank().
		
        LoadAttributes ().
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC EstCostForm ( ):
        IF VALID-OBJECT (oEstCostBlank) THEN
            DELETE OBJECT oEstCostBlank.
	END DESTRUCTOR.


END CLASS.