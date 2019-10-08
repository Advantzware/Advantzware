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
    File        : DebugHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 10 13:57:31 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Enum.*     FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.DebugHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DebugHelper class                                                                         
        Notes:   Disallow instance creation                                                                     
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DebugHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Displays information about the current process                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DisplayProcessInformation ():
        
        DEFINE VARIABLE oProcess       AS System.Diagnostics.Process NO-UNDO . 
        DEFINE VARIABLE cMessage       AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE cMessageDetail AS CHARACTER                  NO-UNDO .
        DEFINE VARIABLE oResult        AS DialogResultEnum           NO-UNDO .
        
        oProcess = System.Diagnostics.Process:GetCurrentProcess() . 
        
        ASSIGN cMessage = SUBSTITUTE ("The current process is: &1&4Executable name: &2&4Current Directory:&3"{&TRAN},
                                      oProcess:Id,
                                      oProcess:ProcessName,
                                      System.Environment:CurrentDirectory,
                                      System.Environment:NewLine) 
                                      
               cMessageDetail = SUBSTITUTE ("Command line: &3&3&1&3&3SESSION:STARTUP-PARAMETERS&3&3&2"{&TRAN},
                                            System.Environment:CommandLine,
                                            SESSION:STARTUP-PARAMETERS,
                                            System.Environment:NewLine) .
        
        oResult = Consultingwerk.Util.MessageFormHelper:ShowMessage (cMessage, 
                                                                     "Process Information"{&TRAN},
                                                                     cMessageDetail,
                                                                     MessageFormImages:ImageInfo,
                                                                     "Kill"{&TRAN},
                                                                     DialogResultEnum:DialogResultAbort,
                                                                     "OK"{&TRAN},
                                                                     DialogResultEnum:DialogResultOK) .
                                                                     
        IF oResult = DialogResultEnum:DialogResultAbort THEN 
            
            Consultingwerk.Windows.DebugHelper:KillCurrentProcess (FALSE) .
                                                                     
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Kills the current process, optionally prompts the user                                                                        
        Notes:                                                         
        @param plAsk Logicla value indicating if the user should be prompted               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID KillCurrentProcess (plAsk AS LOGICAL):
        
        IF plAsk = TRUE THEN DO:
            MESSAGE "Are you sure?"{&TRAN}
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL .
            
            IF lOk = TRUE THEN . 
                ELSE RETURN .                 
        END.
        
        System.Diagnostics.Process:GetCurrentProcess():Kill ().

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Visualizes a .NET object in a Property Dialog
        Notes:   
        @param poObject The .NET object to be displayed in the property dialog. 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowClrObjectProperties (poObject AS System.Object):
        
        DEFINE VARIABLE oDialogForm AS Infragistics.Shared.UltraPropertyPageDialog NO-UNDO . 
        
        oDialogForm = NEW Infragistics.Shared.UltraPropertyPageDialog (poObject) . 
        
        WAIT-FOR oDialogForm:ShowDialog () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Visualizes a .NET object in a Property Dialog
        Notes:   
        @param poObject The .NET object to be displayed in the property dialog
        @param poParent The .NET Form to be used as the parent of the dialog 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowClrObjectProperties (poObject AS System.Object,
                                                       poParent AS System.Windows.Forms.IWin32Window):
        
        DEFINE VARIABLE oDialogForm AS Infragistics.Shared.UltraPropertyPageDialog NO-UNDO . 
        
        oDialogForm = NEW Infragistics.Shared.UltraPropertyPageDialog (poObject) . 
        
        WAIT-FOR oDialogForm:ShowDialog (poParent) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Starts the Progress debugger                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StartDebugger ():
        
        DEBUGGER:INITIATE () .
        DEBUGGER:SET-BREAK () .

    END METHOD.

END CLASS.
