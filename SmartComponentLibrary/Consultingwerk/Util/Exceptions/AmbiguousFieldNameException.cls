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
    File        : AmbiguousFieldNameException
    Purpose     : Exception thrown by DatabaseHelper:UnabbreviatedFieldName
                  when an invalid field name has been passed
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

CLASS Consultingwerk.Util.Exceptions.AmbiguousFieldNameException 
    INHERITS Exception {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the FieldName that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the InvalidFieldNameException class
       Notes:
       @param pcErrorMessage The error message for the exception
       @param pcFieldName The name of the field that caused the exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AmbiguousFieldNameException (pcErrorMessage AS CHARACTER,
                                                    pcFieldName AS CHARACTER):

        SUPER (SUBSTITUTE (pcErrorMessage, pcFieldName), 0).
    
       ASSIGN THIS-OBJECT:FieldName = pcFieldName . 
        
    END CONSTRUCTOR.
        
END CLASS.
