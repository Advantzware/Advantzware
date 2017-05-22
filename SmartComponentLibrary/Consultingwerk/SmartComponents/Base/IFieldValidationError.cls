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
    File        : IFieldValidationError
    Purpose     : 
    Syntax      : 
    Description : Interface definition to be implemented by a field validation
                  error.
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Mar 03 15:44:19 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Base.IFieldValidationError:  

    /*------------------------------------------------------------------------------
        Purpose: If the result of a field validation indicates a validation error,
                 the currently tested Control is indicated by the FocusControl. This
                 can be used to set the focus in the running application to the field with 
                 the invalid value. 
        Notes:   Normally only FocusControl OR FocusControlName are set.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FocusControl AS System.Windows.Forms.Control NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of a data property (binding source field) that 
                 is flagged with an error from a validation routine.
        Notes:   Normally only FocusControl OR FocusControlName are set.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FocusControlName AS CHARACTER NO-UNDO
    GET.

END INTERFACE.