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
    File        : start.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 21 14:25:53 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

DEFINE VARIABLE oForm AS Consultingwerk.Studio.LegacyGuiMigration.Frame.AblFrameMigrationForm NO-UNDO .

DEFINE VARIABLE oTaskbarManager AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager NO-UNDO .

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
SESSION:APPL-ALERT-BOXES = TRUE .
SESSION:DEBUG-ALERT = TRUE .
SESSION:ERROR-STACK-TRACE = TRUE  .
SESSION:SYSTEM-ALERT-BOXES = TRUE .

Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "Software~\Consultingwerk Ltd.~\ABL Frame Migration~\":U .
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE .
Consultingwerk.Framework.FrameworkSettings:StoreRibbonQuickAccessToolbar = TRUE .
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
Consultingwerk.SmartComponents.Implementation.SmartDataBrowser:SaveColumnSettings = TRUE .
&ELSE
Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SaveColumnSettings = TRUE .
&ENDIF

IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO:
    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance .
    oTaskbarManager:ApplicationId = "de.consultingwerk.ablframemigration":U .
END.

IF PROVERSION BEGINS "10.":U THEN
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2007Black_ConsultingwerkStudio.isl":U .
ELSE
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2010Blue_ConsultingwerkStudio.isl":U .

IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
    Consultingwerk.Util.StyleLibraryHelper:LoadFromFile (FILE-INFORMATION:FULL-PATHNAME) .

oForm = NEW Consultingwerk.Studio.LegacyGuiMigration.Frame.AblFrameMigrationForm () .

WAIT-FOR System.Windows.Forms.Application:RUN (oForm) .

DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager .

/* Mike Fechner, Consultingwerk Ltd. 22.02.2011
   Quit, don't fall back to Procedure Editor */
QUIT .

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .
END CATCH.
