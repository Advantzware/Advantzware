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
    File        : FieldValidationError
    Purpose     : Performs formatting of field validation errors 
    Syntax      : 
    Description : Use this method when adding ADM2 compatible message strings
                  to the ERROR-STRING property of a before-table record
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 24 07:54:49 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.OERA.FieldValidationError: 
    
    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE FieldValidationError ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs formatting of field validation errors for ADM2 compatible
                 values in the ERROR-STRING attribute                                                                        
        Notes:                                       
        @param pcFieldName The name of the field the error message belongs to
        @param pcMessage The error message to be shown in the context of the given field
        @return The formatted error message
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER Format (pcFieldName AS CHARACTER, 
                                           pcMessage AS CHARACTER):
        
            RETURN SUBSTITUTE ("&1&3&2":U, 
                               pcFieldName,
                               pcMessage,
                               CHR (1)) .
    END METHOD.

END CLASS.