/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : NoParentRecordAvailableException
    Purpose     : Exception thrown from the EvaluateParentQuery method
                  of the SmartDatasetAdapter class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 14 19:16:11 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.NoParentRecordAvailableException 
    INHERITS EvaluateParentQueryException: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NoParentRecordAvailableException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC NoParentRecordAvailableException (poInnerException AS Progress.Lang.Error, 
	                                                     pcErrorString AS CHARACTER, 
	                                                     piMessageNumber AS INTEGER):
	                                                     
		SUPER (poInnerException, pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the NoParentRecordAvailableException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC NoParentRecordAvailableException (pcErrorString AS CHARACTER, 
	                                                     piMessageNumber AS INTEGER):
	                                                     
		SUPER (pcErrorString, piMessageNumber).
		
	END CONSTRUCTOR.

END CLASS.
