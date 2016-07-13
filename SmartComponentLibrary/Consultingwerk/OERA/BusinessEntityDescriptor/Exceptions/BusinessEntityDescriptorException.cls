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
    File        : BusinessEntityDescriptorException
    Purpose     : Exception thrown by the BusinessEntityDescritorService
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 12 13:53:17 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                               FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                                           FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.Exceptions.BusinessEntityDescriptorException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Entity associated with this error 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityDescriptorException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param pcBusinessEntityName The name of the Business Entity associated with this error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BusinessEntityDescriptorException (poInnerException AS Progress.Lang.Error, 
	                                                      pcErrorString AS CHARACTER,
	                                                      pcBusinessEntityName AS CHARACTER):
		SUPER (poInnerException, pcErrorString).
		
		ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName .
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityDescriptorException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcBusinessEntityName The name of the Business Entity associated with this error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BusinessEntityDescriptorException (poInnerException AS Progress.Lang.Error, 
	                                                      pcErrorString AS CHARACTER, 
	                                                      piMessageNumber AS INTEGER,
                                                          pcBusinessEntityName AS CHARACTER):
		SUPER (poInnerException, pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName .
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityDescriptorException class
        Notes:                          
        @param poInnerException The reference to the original error
        @param pcBusinessEntityName The name of the Business Entity associated with this error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BusinessEntityDescriptorException (poInnerException AS Progress.Lang.Error,
                                                          pcBusinessEntityName AS CHARACTER):
		SUPER (poInnerException).

        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName .
		
	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityDescriptorException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param pcBusinessEntityName The name of the Business Entity associated with this error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BusinessEntityDescriptorException (pcErrorString AS CHARACTER,
                                                          pcBusinessEntityName AS CHARACTER):
		SUPER (pcErrorString).

        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityDescriptorException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcBusinessEntityName The name of the Business Entity associated with this error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BusinessEntityDescriptorException (pcErrorString AS CHARACTER, 
	                                                      piMessageNumber AS INTEGER,
                                                          pcBusinessEntityName AS CHARACTER):
		SUPER (pcErrorString, piMessageNumber).
		
        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName .

	END CONSTRUCTOR.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityDescriptorException class
        Notes:                          
        @param pcBusinessEntityName The name of the Business Entity associated with this error
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC BusinessEntityDescriptorException (pcBusinessEntityName AS CHARACTER):
		SUPER ().
		
        ASSIGN THIS-OBJECT:BusinessEntityName = pcBusinessEntityName .

	END CONSTRUCTOR.

END CLASS.
