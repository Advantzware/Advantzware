 
 /*------------------------------------------------------------------------
    File        : orderType
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : DEVA$!
    Created     : Sat Aug 28 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS oe.orderType: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
    
    {oe/ttOrderType.i}

	CONSTRUCTOR PUBLIC orderType (  ):
		SUPER ().
		
		RUN spGetSessionParam (INPUT "Company", OUTPUT cCompany).
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Delete( ipiOrderTypeID AS INTEGER ):
        DEFINE BUFFER bf-orderType FOR orderType.
		
		FIND FIRST bf-orderType EXCLUSIVE-LOCK
		     WHERE bf-orderType.orderTypeID EQ ipiOrderTypeID
		     NO-ERROR.
		IF NOT AVAILABLE bf-orderType THEN
		    RETURN "OrderType is invalid or locked".
		
		DELETE bf-orderType.
		
		RETURN "".
	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID pGetAll( OUTPUT TABLE ttOrderType ):
        DEFINE BUFFER bf-OrderType FOR orderType.       	
		
		FOR EACH bf-OrderType NO-LOCK
		    WHERE bf-OrderType.company EQ cCompany:
		    
		    CREATE ttOrderType.
		    BUFFER-COPY bf-OrderType TO ttOrderType.
            ASSIGN
			ttOrderType.rwRowID = ROWID(bf-OrderType).
  			
		END.
        
	END METHOD.

    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC CHARACTER Update( 
        ipiOrderTypeID          AS INTEGER, 
        ipcOrderTypeDescription AS CHARACTER, 
        ipcOrderTypeSource      AS CHARACTER,          
        ipiNumberSequence       AS INTEGER, 
        iplInactive             AS LOGICAL,         
        ipcEstimateType         AS CHARACTER,
		OUTPUT opiOrderTypeID AS INTEGER
        ):
                        
        DEFINE BUFFER bf-orderType FOR orderType.
        		
        FIND FIRST bf-orderType EXCLUSIVE-LOCK
             WHERE bf-orderType.orderTypeID EQ ipiOrderTypeID
             NO-ERROR.                            
        
        IF NOT AVAILABLE bf-orderType THEN DO:
            CREATE bf-orderType.
            ASSIGN
                bf-orderType.company      = cCompany                
                 .
        END.
        
        ASSIGN
		    bf-orderType.orderTypeDescription = ipcOrderTypeDescription
            bf-orderType.orderTypeSource      = ipcOrderTypeSource                        
            bf-orderType.numberSequence       = ipiNumberSequence
            bf-orderType.inactive             = iplInactive                       
            bf-orderType.estimateType         = ipcEstimateType
            . 			
		
        opiOrderTypeID = bf-orderType.orderTypeID.
		RELEASE bf-orderType.
        RETURN "".
        
	END METHOD.
    
END CLASS.