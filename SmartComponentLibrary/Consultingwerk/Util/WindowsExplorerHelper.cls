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
    File        : WindowsExplorerHelper
    Purpose     : API's to interact with the Windows Explorer 
    Syntax      : Static methods only, Protected constructor to disallow 
                  instance creation
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 30 21:30:08 CET 2010
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Util.WindowsExplorerHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disabllow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED WindowsExplorerHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Locates the passed file in a Windows Explorer window (shell)                                                                      
        Notes:   Performs file lookup using the FILE-INFO handle                                                             
        @param pcFileName The filename to open in the Windows explorer          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowFileInExplorer (pcFileName AS CHARACTER):
        
        DEFINE VARIABLE cExplorerExe AS CHARACTER                           NO-UNDO.
        DEFINE VARIABLE oProcess     AS System.Diagnostics.ProcessStartInfo NO-UNDO . 
        
        FILE-INFO:FILE-NAME = pcFileName .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN DO:  
        
            ASSIGN cExplorerExe = SUBSTITUTE ("&1\explorer.exe":U, 
                                              System.Environment:GetEnvironmentVariable ("windir":U)) .
        
            oProcess = NEW System.Diagnostics.ProcessStartInfo (cExplorerExe) .
                
            oProcess:Arguments = "/select,":U + FILE-INFO:FULL-PATHNAME .
            
            System.Diagnostics.Process:Start (oProcess) .
        END.
        ELSE 
            UNDO, THROW NEW AppError (SUBSTITUTE ("The file name &1 is not valid."{&TRAN}, pcFileName), 0) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens a folder in a Windows Explorer window (shell)                                                                      
        Notes:                                                                
        @param pcFolderName The folder name to open in the Windows explorer          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowFolder (pcFolderName AS CHARACTER):
        
        DEFINE VARIABLE cExplorerExe AS CHARACTER                           NO-UNDO.
        DEFINE VARIABLE oProcess     AS System.Diagnostics.ProcessStartInfo NO-UNDO . 
        
        FILE-INFO:FILE-NAME = pcFolderName .
        
        IF FILE-INFO:FULL-PATHNAME > "":U THEN DO:  
        
            ASSIGN cExplorerExe = SUBSTITUTE ("&1\explorer.exe":U, 
                                              System.Environment:GetEnvironmentVariable ("windir":U)) .
        
            oProcess = NEW System.Diagnostics.ProcessStartInfo (cExplorerExe) .
                
            oProcess:Arguments = FILE-INFO:FULL-PATHNAME .
            
            System.Diagnostics.Process:Start (oProcess) .
        END.
        ELSE 
            UNDO, THROW NEW AppError (SUBSTITUTE ("The folder name &1 is not valid."{&TRAN}, pcFolderName), 0) . 

    END METHOD.
&ENDIF
END CLASS.
