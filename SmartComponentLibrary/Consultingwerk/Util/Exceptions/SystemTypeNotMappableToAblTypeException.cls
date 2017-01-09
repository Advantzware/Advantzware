/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SystemTypeNotMappableToAblTypeException
    Purpose     : Exception thrown by DataTypeHelper:ToABLDataType (System.Type)
                  when the System.Type does not match an ABL primitive type
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 18 10:09:12 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.Exceptions.SystemTypeNotMappableToAblTypeException 
    INHERITS Exception {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the System.Type that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TypeName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the SystemTypeNotMappableToAblTypeException class
       Notes:
       @param pcErrorMessage The error message for the exception
       @param pcTypeName The type name for the exception
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC SystemTypeNotMappableToAblTypeException (pcErrorMessage AS CHARACTER,
	                                                            pcTypeName AS CHARACTER):
		SUPER (pcErrorMessage, 0).
	
	   ASSIGN THIS-OBJECT:TypeName = pcTypeName . 
		
	END CONSTRUCTOR.
		
END CLASS.
