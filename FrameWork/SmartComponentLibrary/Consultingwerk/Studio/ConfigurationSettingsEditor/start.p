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
    File        : start.p
    Purpose     : Starts the Configuration Editor Form

    Syntax      :

    Description :

    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu Dec 01 08:01:24 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

DEFINE VARIABLE oForm AS Consultingwerk.Studio.ConfigurationSettingsEditor.ConfigurationSettingsEditorForm NO-UNDO .

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
ASSIGN
    SESSION:APPL-ALERT-BOXES = TRUE
    SESSION:DEBUG-ALERT = TRUE
    SESSION:ERROR-STACK-TRACE = TRUE
    SESSION:SYSTEM-ALERT-BOXES = TRUE
    .

Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "Software~\Consultingwerk Ltd.~\Configuration Settings Editor~\":U .
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE .
Consultingwerk.Framework.FrameworkSettings:StoreRibbonQuickAccessToolbar = TRUE .
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
Consultingwerk.SmartComponents.Implementation.SmartDataBrowser:SaveColumnSettings = TRUE .
&ELSE
Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SaveColumnSettings = TRUE .
&ENDIF

IF PROVERSION BEGINS "10.":U THEN
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2007Black_ConsultingwerkStudio.isl":U .
ELSE
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2010Blue_ConsultingwerkStudio.isl":U .

IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
    Consultingwerk.Util.StyleLibraryHelper:LoadFromFile (FILE-INFORMATION:FULL-PATHNAME) .

oForm = NEW Consultingwerk.Studio.ConfigurationSettingsEditor.ConfigurationSettingsEditorForm () .

WAIT-FOR System.Windows.Forms.Application:Run (oForm) .

DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager .

/* Mike Fechner, Consultingwerk Ltd. 22.02.2011
   Quit, don't fall back to Procedure Editor */
QUIT .

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .
END CATCH.
