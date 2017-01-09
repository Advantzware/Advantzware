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
    File        : AssignKeyFieldErrorEventArgs
    Purpose     : Event argument for the AssignKeyFieldError event of the  
                  SmartDatasetAdapter
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 09 16:10:24 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs              FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.AssignKeyFieldErrorEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the buffer that caused the issue 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the field that caused the issue 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Error associated with this event 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Error AS Progress.Lang.Error NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AssignKeyFieldErrorEventArgs class
        Notes:   
        @param err The Error associated with this event
        @param pcBufferName The name of the buffer that cause the issue
        @param pcFieldName The name of the field that caused the issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AssignKeyFieldErrorEventArgs (err AS Progress.Lang.Error,
                                                     pcBufferName AS CHARACTER, 
                                                     pcFieldName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Error      = err 
               THIS-OBJECT:BufferName = pcBufferName 
               THIS-OBJECT:FieldName  = pcFieldName . 
                
    END CONSTRUCTOR.

END CLASS.
