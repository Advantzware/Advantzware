/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartDataObjectValidationError
    Purpose     : Error class for validation errors returned from an ADM2
                  SmartDataObject
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 03 06:38:17 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDataObjectValidationError 
    INHERITS AppError
    IMPLEMENTS Consultingwerk.SmartComponents.Base.IFieldValidationError: 
        
    DEFINE VARIABLE cFocusControlName AS CHARACTER NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectValidationError class
        Notes:
        @param pcReturnValue The return value associated with the error
        @param pcFieldName The name of the field that raised the error  
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataObjectValidationError (pcReturnValue AS CHARACTER, 
                                                       pcFieldName AS CHARACTER):
                                                   
        SUPER (pcReturnValue).
        
        ASSIGN cFocusControlName = pcFieldName.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectValidationError class
        Notes:
        @param pcReturnValue The return value associated with the error
        @param pcFieldName The name of the field that raised the error  
        @param pcFields The fields associated with the error   
        @param pcTables The tables associated with the error   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataObjectValidationError (pcReturnValue AS CHARACTER, 
                                                       pcFieldName AS CHARACTER,
                                                       pcFields AS CHARACTER,
                                                       pcTables AS CHARACTER):
                                                   
        SUPER (pcReturnValue).
        
        ASSIGN cFocusControlName       = pcFieldName
               THIS-OBJECT:ErrorFields = pcFields 
               THIS-OBJECT:ErrorTables = pcTables .
        
    END CONSTRUCTOR.

      /*------------------------------------------------------------------------------
        Purpose: Returns information about the fields attached to this error
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ErrorFields AS CHARACTER NO-UNDO 
    GET.
    SET.
    
      /*------------------------------------------------------------------------------
        Purpose: Returns information about the fields attached to this error
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY ErrorTables AS CHARACTER NO-UNDO 
    GET.
    SET.
    
      /*------------------------------------------------------------------------------
        Purpose: If the result of a field validation indicates a validation error,
                 the currently tested Control is indicated by the FocusControl. This
                 can be used to set the focus in the running application to the field with 
                 the invalid value. 
                 Enforced by Interface IFieldValidationError.
        Notes:   Always returns ? because this class of error does not know the 
                 actual reference to the databound field
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FocusControl AS System.Windows.Forms.Control NO-UNDO
    GET:
        RETURN ?.        
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of a data property (binding source field) that 
                 is flagged with an error from a validation routine.
                 Enforced by Interface IFieldValidationError.
        Notes:   FieldName should contain clientside fieldname, not serverside 
                 field name
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FocusControlName AS CHARACTER NO-UNDO
    GET:
        RETURN cFocusControlName .
    END GET.

END CLASS.
