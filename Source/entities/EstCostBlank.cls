 
 /*------------------------------------------------------------------------
    File        : EstCostBlank
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : gshde
    Created     : Fri Oct 01 22:30:19 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS entities.EstCostBlank INHERITS entities.Entity: 
    DEFINE TEMP-TABLE ttEstCostBlank
    LIKE EstCostBlank
    BEFORE-TABLE bttEstCostBlank.

    DEFINE DATASET dsEstCostBlank FOR ttEstCostBlank.

    DEFINE VARIABLE oEstCostMaterial AS entities.EstCostMaterial  NO-UNDO.
    
    DEFINE DATA-SOURCE srcEstCostBlank FOR estCostBlank.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC EstCostBlank (  ):
        SUPER (DATASET dsEstCostBlank:HANDLE, DATA-SOURCE srcEstCostBlank:HANDLE).
        
        pInit().
    END CONSTRUCTOR.
        
  	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC EstCostBlank ( ipiEstCostFormID AS INT64 ):
	    
		SUPER (DATASET dsEstCostBlank:HANDLE, DATA-SOURCE srcEstCostBlank:HANDLE).
		
		THIS-OBJECT:Read("estCostFormID EQ " + STRING(ipiEstCostFormID) ).
		
        pInit().		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC entities.EstCostMaterial GetMaterialObject( ):
		
		RETURN oEstCostMaterial.

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetBlanks( OUTPUT TABLE FOR ttEstCostBlank):
        
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL HasRecords(  ):
        
        RETURN TEMP-TABLE ttEstCostBlank:HAS-RECORDS.

    END METHOD.

    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID LoadMaterials ( ipiEstCostBlanksID AS INT64):
        
        oEstCostMaterial:Read("estCostBlankID eq " + STRING(ipiEstCostBlanksID)).
            
    END METHOD.    

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pInit(  ):
		
		oEstCostMaterial = NEW entities.EstCostMaterial().

        LoadAttributes ().

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC EstCostBlank ( ):
        IF VALID-OBJECT (oEstCostMaterial) THEN
            DELETE OBJECT oEstCostMaterial.
	END DESTRUCTOR.


END CLASS.