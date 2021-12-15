 
 /*------------------------------------------------------------------------
    File        : EstCostMaterial
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : gshde
    Created     : Fri Oct 01 22:30:19 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS entities.EstCostMaterial INHERITS entities.Entity: 
    DEFINE TEMP-TABLE ttEstCostMaterial
    LIKE EstCostMaterial
    BEFORE-TABLE bttEstCostMaterial.

    DEFINE DATASET dsEstCostMaterial FOR ttEstCostMaterial.
    
    DEFINE DATA-SOURCE srcEstCostMaterial FOR EstCostMaterial.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC EstCostMaterial (  ):
        SUPER (DATASET dsEstCostMaterial:HANDLE, DATA-SOURCE srcEstCostMaterial:HANDLE).
        
        pInit().
    END CONSTRUCTOR.
        
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    CONSTRUCTOR PUBLIC EstCostMaterial ( ipiEstCostBlankID AS INT64 ):
        
        SUPER (DATASET dsEstCostMaterial:HANDLE, DATA-SOURCE srcEstCostMaterial:HANDLE).
        
        THIS-OBJECT:Read("estCostBlankID eq " + STRING(ipiEstCostBlankID) ).
        
        pInit().        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL HasRecords(  ):
        
        RETURN TEMP-TABLE ttEstCostMaterial:HAS-RECORDS.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID GetMaterials( OUTPUT TABLE FOR ttEstCostMaterial):
        
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID pInit(  ):
		
	END METHOD.

END CLASS.