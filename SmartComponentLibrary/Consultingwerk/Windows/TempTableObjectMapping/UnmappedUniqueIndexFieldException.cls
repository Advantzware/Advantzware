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
    File        : UnmappedUniqueIndexFieldException
    Purpose     : Exception thrown by the TempTableToObjectMapper when the 
                  specified index field is not mapped
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 18 09:31:26 CET 2014
    Notes       : SCL-1039
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Windows.TempTableObjectMapping.UnmappedUniqueIndexFieldException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the index field that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the System.Type that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TypeName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the UnmappedUniqueIndexFieldException class
       Notes:
       @param pcErrorMessage The error message for the exception
       @param pcFieldName The field name for the exception
       @param pcTypeName The type name for the exception
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UnmappedUniqueIndexFieldException (pcErrorMessage AS CHARACTER,
	                                                      pcFieldName AS CHARACTER,
	                                                      pcTypeName AS CHARACTER):
		
		SUPER (pcErrorMessage, 0).
		
		ASSIGN THIS-OBJECT:FieldName = pcFieldName
		       THIS-OBJECT:TypeName  = pcTypeName .
		
	END CONSTRUCTOR.

END CLASS.