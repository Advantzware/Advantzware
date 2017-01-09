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
    File        : MissingIndexFieldValueException
    Purpose     : Exception thrown by BufferHelper:UniqueFindPredicateWithValueBuffer
                  when neither the Value Buffer of the MissingValues dictionary
                  provides the value for a required unique index field
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 08 10:09:12 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.Exceptions.MissingIndexFieldValueException 
    INHERITS Exception {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the BufferName that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the FieldName that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the MissingIndexFieldValueException class
       Notes:   The pcBufferName and pcFieldName will be substituted for &1 and &2 in 
                the pcErrorMessage
       @param pcErrorMessage The error message for the exception
       @param pcBuffername The name of the buffer that caused the exception
       @param pcFieldName The name of the field that caused the exception
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC MissingIndexFieldValueException (pcErrorMessage AS CHARACTER,
	                                                    pcBufferName AS CHARACTER,
	                                                    pcFieldName AS CHARACTER):

		SUPER (SUBSTITUTE (pcErrorMessage, pcBufferName, pcFieldName), 0).
	
	    ASSIGN THIS-OBJECT:FieldName = pcFieldName . 
		
	END CONSTRUCTOR.
		
END CLASS.
