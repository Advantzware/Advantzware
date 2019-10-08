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
    File        : IAbhackSupportService
    Purpose     : Interface for services that launch ABHack for Business
                  Entity Editor Form
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 29 18:35:18 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY.

INTERFACE Consultingwerk.Studio.ProcedureEditor.IAbhackSupportService:

    /*------------------------------------------------------------------------------
        Purpose: Invoked when an Editor Form is activated
        Notes:   Used to "Load the global resources for that editor"
        @param poEditorForm The Editor that was activated
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateEditor (poEditorForm AS System.Windows.Forms.Form).

    /*------------------------------------------------------------------------------
        Purpose: Closes ABHack
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseAbhack ().

    /*------------------------------------------------------------------------------
        Purpose: Initializes / Launches ABHack
        Notes:   Only supported for 32 bit prowin32.exe.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeAbhack ().

END INTERFACE.


