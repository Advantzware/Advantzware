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
    Purpose     : Business Entity Designer startup procedure

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Dec 21 14:25:53 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*    FROM PROPATH .
USING Consultingwerk.OERA.Context.* FROM PROPATH .

{Consultingwerk/products.i}

DEFINE VARIABLE oForm AS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm NO-UNDO .

DEFINE VARIABLE oTaskbarManager AS Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager NO-UNDO .

/* ***************************  Main Block  *************************** */

/* Mike Fechner, Consultingwerk Ltd. 01.05.2013
   Best default for GUI apps */
SESSION:APPL-ALERT-BOXES = TRUE .
SESSION:DEBUG-ALERT = TRUE .
SESSION:ERROR-STACK-TRACE = TRUE  .
SESSION:SYSTEM-ALERT-BOXES = TRUE .

Consultingwerk.Framework.FrameworkSettings:BaseRegistryKey = "Software~\Consultingwerk Ltd.~\Business Entity Designer~\":U .
Consultingwerk.Framework.FrameworkSettings:StoreWindowPosition = TRUE .
Consultingwerk.Framework.FrameworkSettings:StoreRibbonQuickAccessToolbar = TRUE .
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
Consultingwerk.SmartComponents.Implementation.SmartDataBrowser:SaveColumnSettings = TRUE .
&ELSE
Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings:SaveColumnSettings = TRUE .
&ENDIF

IF Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:IsPlatformSupported THEN DO:
    oTaskbarManager = Microsoft.WindowsAPICodePack.Taskbar.TaskbarManager:Instance .
    oTaskbarManager:ApplicationId = "de.consultingwerk.businessentitydesigner":U .
END.

{Consultingwerk/Studio/initialize-studio-session.i}

FrameworkSettings:WaitStateManager = NEW NotifyIconWaitStateManager
                                        ("Consultingwerk/BusinessEntityDesigner/UI/Images/text_tree.ico":U,
                                         "Consultingwerk/BusinessEntityDesigner/UI/Images/text_tree_data.ico":U) .


IF PROVERSION BEGINS "10.":U THEN
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2007Black_ConsultingwerkStudio.isl":U .
ELSE
    FILE-INFORMATION:FILE-NAME = "Consultingwerk/Windows/Styles/Office2010Blue_ConsultingwerkStudio.isl":U .

IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
    Consultingwerk.Util.StyleLibraryHelper:LoadFromFile (FILE-INFORMATION:FULL-PATHNAME) .

/* Mike Fechner, Consultingwerk Ltd. 14.11.2010
   Install ERM4 license */
RUN Consultingwerk/BusinessEntityDesigner/create-erm-license.p .

/* Mike Fechner, Consultingwerk Ltd. 20.01.2013
   Perform Database Authentication if required */
RUN Consultingwerk/Windows/OpenEdgeLogin/_prostar.p .

/* Mike Fechner, Consultingwerk Ltd. 27.12.2013
   Enable tracing of Query Info on client */
{Consultingwerk/get-service.i Consultingwerk.OERA.IDataSourceQueryInfoProvider
                              "NEW Consultingwerk.OERA.DataSourceQueryInfoProvider()"} .

oForm = NEW Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityDesignerForm () .

WAIT-FOR System.Windows.Forms.Application:RUN (oForm) .

CATCH e AS Progress.Lang.Error :
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (e) .
END CATCH.

FINALLY:
    DELETE OBJECT Consultingwerk.Framework.FrameworkSettings:WaitStateManager .

    QUIT .
END FINALLY.
