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
    File        : FileEventArgs
    Purpose     : Event Argument class for the AfterFileOpen and 
                  AfterFileSave event of the BusinessEntityDatasetController
                 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 26 19:58:07 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.EventArgs.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.BusinessEntityDesigner.Services.FileEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the file being saved to or opened                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FileEventArgs class. Sets the 
                 FileName property                                                                        
        Notes:   
        @param pcFileName The file name being saved or opened                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FileEventArgs (pcFileName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:FileName = pcFileName .
        
    END CONSTRUCTOR.

END CLASS.