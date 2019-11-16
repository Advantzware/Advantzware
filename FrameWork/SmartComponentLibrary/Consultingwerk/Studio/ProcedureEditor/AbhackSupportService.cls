/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : AbhackSupportService
    Purpose     : Launches ABHack for Business Entity Editor Form
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 29 18:39:12 CEST 2016
    Notes       : Requires version of ABHack customized for WinKit compatibility
                  (ACTIVE-WINDOW replaced with
                  Consultingwerk.WindowIntegrationKit.ActiveWindow:GetHandle())
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.ProcedureEditor.*     FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH.
USING Progress.Lang.*                             FROM PROPATH .

CLASS Consultingwerk.Studio.ProcedureEditor.AbhackSupportService
    IMPLEMENTS IAbhackSupportService:

    DEFINE VARIABLE hAbhack AS HANDLE NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Invoked when an Editor Form is activated
        Notes:   Used to "Load the global resources for that editor"
        @param poEditorForm The Editor that was activated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateEditor (poEditorForm AS System.Windows.Forms.Form):

        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.
        DEFINE VARIABLE hEditor AS HANDLE NO-UNDO.

        IF VALID-HANDLE (hAbhack) AND TYPE-OF (poEditorForm, IEmbeddedWindowForm) THEN DO:

            ASSIGN hWindow = CAST (poEditorForm, IEmbeddedWindowForm):EmbeddedWindow .

            IF NOT VALID-HANDLE (hWindow) THEN
                RETURN .

            RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).

            IF NOT VALID-HANDLE (hEditor) THEN
                RETURN .

            RUN loadGlobalResources IN hAbhack (hEditor).
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Closes ABHack
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseAbhack ():

        IF VALID-HANDLE (hAbhack) THEN
            RUN CloseProcedure IN hAbhack .

        ASSIGN hAbhack = ? .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes / Launches ABHack
        Notes:   Only supported for 32 bit prowin32.exe.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeAbhack():

        IF SessionHelper:ProcessBitness() = 32 AND FileHelper:Exists ("protools/abhack/abhackwin.w":U) THEN DO:

            RUN protools/abhack/abhackwin.w PERSISTENT SET hAbhack.
        END.

    END METHOD .

END CLASS.
