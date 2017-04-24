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
    File        : InvalidExtentFieldInFieldMappingException
    Purpose     : Exception thrown by the TempTableToObjectMapper when the 
                  specified field mapping instructions contain EXTENT fields
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 18 09:31:26 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Windows.TempTableObjectMapping.InvalidExtentFieldInFieldMappingException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the mapped ABL buffer field 
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Returns the data type of the mapped ABL buffer field 
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldType AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET.     
    
    /*------------------------------------------------------------------------------
       Purpose: Returns the name of the property that caused the issue
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the InvalidPropertyNameException class
       Notes:
       @param pcErrorMessage The error message for the exception
       @param pcPropertyName The property name for the exception
       @param pcFieldName The field name for the exception
       @param pcFieldType The field type for the exception 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidExtentFieldInFieldMappingException (pcErrorMessage AS CHARACTER,
                                                                   pcPropertyName AS CHARACTER,
                                                                   pcFieldName AS CHARACTER,
                                                                   pcFieldType AS CHARACTER):
        
        SUPER (pcErrorMessage, 0).
        
        ASSIGN THIS-OBJECT:PropertyName = pcPropertyName
               THIS-OBJECT:FieldName    = pcFieldName 
               THIS-OBJECT:FieldType    = pcFieldType .
        
    END CONSTRUCTOR.

END CLASS.