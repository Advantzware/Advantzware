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
    File        : InvalidPropertyNameException
    Purpose     : Exception thrown by the TempTableFromTypeFactory when the 
                  specified property name is invalid
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 18 09:31:26 CET 2014
    Notes       : SCL-591
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Windows.TempTableObjectMapping.InvalidPropertyNameException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the property that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
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
       Purpose: Constructor for the InvalidPropertyNameException class
       Notes:
       @param pcErrorMessage The error message for the exception
       @param pcPropertyName The property name for the exception
       @param pcTypeName The type name for the exception
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidPropertyNameException (pcErrorMessage AS CHARACTER,
                                                     pcPropertyName AS CHARACTER,
                                                     pcTypeName AS CHARACTER):
        
        SUPER (pcErrorMessage, 0).
        
        ASSIGN THIS-OBJECT:PropertyName = pcPropertyName
               THIS-OBJECT:TypeName     = pcTypeName .
        
    END CONSTRUCTOR.

END CLASS.