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
    File        : CancelableFileEventArgs
    Purpose     : Event Argument class for the BeforeFileOpen and 
                  BeforeFileSave event of the BusinessEntityDatasetController
                 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 26 19:54:20 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.CancelableEventArgs.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.BusinessEntityDesigner.Services.CancelableFileEventArgs 
    INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the file being saved to or opened                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CancelableFileEventArgs class. Sets the 
                 FileName property                                                                        
        Notes:        
        @param pcFileName The file name for this event                                                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CancelableFileEventArgs (pcFileName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:FileName = pcFileName . 
        
    END CONSTRUCTOR.

END CLASS.