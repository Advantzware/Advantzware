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
    File        : ClientValidationError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Mar 03 15:51:41 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.ClientValidationError 
    INHERITS AppError
    IMPLEMENTS IFieldValidationError: 
        
    DEFINE VARIABLE oFocusControl AS Control NO-UNDO.
    DEFINE VARIABLE cFocusControlName AS CHARACTER NO-UNDO . 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference of the Control to focus with this error 
        Notes:   If the result of a field validation indicates a validation error,
                 the currently tested Control is indicated by the FocusControl. This
                 can be used to set the focus in the running application to the field with 
                 the invalid value.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FocusControl AS System.Windows.Forms.Control NO-UNDO
    GET:
        RETURN oFocusControl.        
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of a data property (binding source field) that 
                 is flagged with an error from a validation routine.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FocusControlName AS CHARACTER NO-UNDO
    GET:
        RETURN cFocusControlName .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClientValidationError class
        Notes:   The Constructor creates a new instance of ClientValidationError with 
                 the ReturnValue property set to the value of the ErrorString 
                 parameter and sets oFocusControl to the given poFocusControl.
        @param pcReturnValue The return value for the error
        @param poFocusControl The reference to the focus control
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClientValidationError (pcReturnValue AS CHARACTER, 
                                              poFocusControl AS Control):
                                                   
        SUPER (pcReturnValue).
        
        ASSIGN oFocusControl = poFocusControl.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClientValidationError class
        Notes:   The Constructor creates a new instance of ClientValidationError and 
                 assigns the first message on the object with the values from the 
                 ErrorMessage and MessageNumber arguments. It also sets the 
                 NumMessages property to 1 and sets oFocusControl to the given 
                 poFocusControl.
        @param pcErrorString The error message for the error object
        @param piErrorNumber The error number for the error object
        @param poFocusControl The reference to the focus control
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClientValidationError (pcErrorString AS CHARACTER, 
                                              piErrorNumber AS INTEGER, 
                                              poFocusControl AS Control):
                                                   
        SUPER (INPUT pcErrorString, INPUT piErrorNumber).
        
        ASSIGN oFocusControl = poFocusControl.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClientValidationError class
        Notes:   The Constructor creates a new instance of ClientValidationError with 
                 an empty message list and sets oFocusControl to the given 
                 poFocusControl. 
        @param poFocusControl The reference to the focus control
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClientValidationError (poFocusControl AS Control):
        
        SUPER ().
        
        ASSIGN oFocusControl = poFocusControl.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClientValidationError class
        Notes:   The Constructor creates a new instance of ClientValidationError with 
                 the ReturnValue property set to the value of the ErrorString 
                 parameter and sets oFocusControl to the given poFocusControl.
        @param pcReturnValue The return value for the error
        @param pcFocusControlName The name of the Control (BindingSource Column name) 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClientValidationError (pcReturnValue AS CHARACTER, 
                                              pcFocusControlName AS CHARACTER):
                                                   
        SUPER (pcReturnValue).
        
        ASSIGN cFocusControlName = pcFocusControlName.
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClientValidationError class
        Notes:   The Constructor creates a new instance of ClientValidationError and 
                 assigns the first message on the object with the values from the 
                 ErrorMessage and MessageNumber arguments. It also sets the 
                 NumMessages property to 1 and sets oFocusControl to the given 
                 poFocusControl.
        @param pcErrorString The error message for the error object
        @param piErrorNumber The error number for the error object
        @param pcFocusControlName The name of the Control (BindingSource Column name) 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClientValidationError (pcErrorString AS CHARACTER, 
                                              piErrorNumber AS INTEGER, 
                                              pcFocusControlName AS CHARACTER):
                                                   
        SUPER (INPUT pcErrorString, INPUT piErrorNumber).
        
        ASSIGN cFocusControlName = pcFocusControlName .
        
    END CONSTRUCTOR.

END CLASS.
