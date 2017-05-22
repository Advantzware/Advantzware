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
    Purpose     : Start procedure for BusinessEntityBrowserForm

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 14 22:46:17 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.Framework.*    FROM PROPATH .
USING Consultingwerk.OERA.Context.* FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

DEFINE VARIABLE oForm AS Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityBrowserForm NO-UNDO .

DEFINE VARIABLE oTaskbarManager        AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager NO-UNDO .

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
SESSION:APPL-ALERT-BOXES = TRUE .
SESSION:DEBUG-ALERT = TRUE .
SESSION:ERROR-STACK-TRACE = TRUE  .
SESSION:SYSTEM-ALERT-BOXES = TRUE .

IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO:
    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance .
    oTaskbarManager:ApplicationId = "de.consultingwerk.businessentitybrowserform":U .
END.

{Consultingwerk/Studio/initialize-studio-session.i}

IF PROVERSION BEGINS "10.":U THEN
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2007Black_ConsultingwerkStudio.isl":U .
ELSE
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2010Blue_ConsultingwerkStudio.isl":U .

IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
    Consultingwerk.Util.StyleLibraryHelper:LoadFromFile (FILE-INFORMATION:FULL-PATHNAME) .

/* Mike Fechner, Consultingwerk Ltd. 27.12.2013
   Enable tracing of Query Info on client */
{Consultingwerk/get-service.i Consultingwerk.OERA.IDataSourceQueryInfoProvider
                              "NEW Consultingwerk.OERA.DataSourceQueryInfoProvider()"} .

Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "Software~\Consultingwerk Ltd.~\Business Entity Browser~\":U .
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE .
Consultingwerk.Framework.FrameworkSettings:StoreRibbonQuickAccessToolbar = TRUE .
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
Consultingwerk.SmartComponents.Implementation.SmartDataBrowser:SaveColumnSettings = TRUE .
&ELSE
Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SaveColumnSettings = TRUE .
&ENDIF

/* Mike Fechner, Consultingwerk Ltd. 20.01.2013
   Perform Database Authentication if required */
RUN Consultingwerk/Windows/OpenEdgeLogin/_prostar.p .

oForm = NEW Consultingwerk.SmartComponents.Tools.BusinessEntityBrowser.BusinessEntityBrowserForm () .

WAIT-FOR System.Windows.Forms.Application:Run (oForm) .

DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager .

/* Mike Fechner, Consultingwerk Ltd. 15.02.2012
   Quit, don't fall back to Procedure Editor */
QUIT .

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .
END CATCH.
