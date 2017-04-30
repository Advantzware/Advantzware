/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : TempFileManager
    Purpose     : Service that supports management of temporary files. 
                  Temp files can be registered for delete on session shutdown
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 21 19:17:01 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Framework.TempFileManager 
    IMPLEMENTS ITempFileManager: 

    DEFINE TEMP-TABLE ttTempFiles NO-UNDO 
        FIELD TempFileName AS CHARACTER .

    /*------------------------------------------------------------------------------
        Purpose: Deletes all temp-files 
        Notes:   Typically called when ending the ABL session
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CleanUp ():
        
        FOR EACH ttTempFiles ON ERROR UNDO, NEXT:
            
            OS-DELETE VALUE (ttTempFiles.TempFileName) NO-ERROR .
        
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Registers a temp-file for delete when the session ends 
        Notes:   
        @param pcFileName The name of the temp-file to delete
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterTempFile (pcFileName AS CHARACTER):
        
        CREATE ttTempFiles .
        ASSIGN ttTempFiles.TempFileName = pcFileName .

    END METHOD .

END CLASS.
