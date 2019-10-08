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
    File        : NotifyIconWaitStateManager
    Purpose     : IWaitStateManager implementation using a NotifyIcon
                  component
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jun 13 15:49:44 CEST 2009
    Notes       : This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.* FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Drawing.*           FROM ASSEMBLY .
USING System.Windows.Forms.*     FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Framework.NotifyIconWaitStateManager
    INHERITS WaitStateManager:

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE PRIVATE VARIABLE oNotifyIcon          AS NotifyIcon  NO-UNDO .
    DEFINE PRIVATE VARIABLE oContextMenu         AS ContextMenu NO-UNDO .
    DEFINE PRIVATE VARIABLE oIdleIcon            AS Icon        NO-UNDO .
    DEFINE PRIVATE VARIABLE oBusyIcon            AS Icon        NO-UNDO .

    DEFINE PRIVATE VARIABLE oProcessInfoMenuItem AS MenuItem    NO-UNDO .
    DEFINE PRIVATE VARIABLE oSessionInfoMenuItem AS MenuItem    NO-UNDO .
    DEFINE PRIVATE VARIABLE oQuitMenuItem        AS MenuItem    NO-UNDO .
    DEFINE PRIVATE VARIABLE oDebugMenuItem       AS MenuItem    NO-UNDO .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotifyIconWaitStateManager class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotifyIconWaitStateManager ():
        THIS-OBJECT ("Consultingwerk/Framework/NotifyIcons/oe.ico":U,
                     "Consultingwerk/Framework/NotifyIcons/oedb.ico":U) .
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NotifyIconWaitStateManager class
        Notes:
        @param pcIdleIconFileName The file name of the idle icon
        @param pcBusyIconFileName The file name of the busy icon
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NotifyIconWaitStateManager (pcIdleIconFileName AS CHARACTER,
                                                   pcBusyIconFileName AS CHARACTER):
        SUPER ().

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        oNotifyIcon = NEW System.Windows.Forms.NotifyIcon () .
        oNotifyIcon:DoubleClick:Subscribe (NotifyIconDoubleClickHandler) .
        oNotifyIcon:MouseMove:Subscribe (NotifyIconMouseMoveHandler) .

        IF Consultingwerk.Framework.FrameworkSettings:DebugMode THEN DO:
            oContextMenu = NEW ContextMenu () .
            oProcessInfoMenuItem = oContextMenu:MenuItems:Add("Display &Process Information"{&TRAN}) .
            oSessionInfoMenuItem = oContextMenu:MenuItems:Add("Display &Session Information"{&TRAN}) .
            oDebugMenuItem = oContextMenu:MenuItems:Add("&Debug"{&TRAN}) .
            oQuitMenuItem = oContextMenu:MenuItems:Add("&Quit"{&TRAN}) .

            oNotifyIcon:ContextMenu = oContextMenu .

            {Consultingwerk/foreach.i MenuItem oItem in oContextMenu:MenuItems}
                oItem:Click:Subscribe (ContextMenuItemClickHandler) .
            END.
        END.

        FILE-INFORMATION:FILE-NAME = pcIdleIconFileName .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
           oIdleIcon = NEW Icon (FILE-INFORMATION:FULL-PATHNAME) .
        ELSE
           UNDO, THROW NEW AppError (SUBSTITUTE("Unable to start NotifyIconWaitStateManager. &1 cannot be found."{&TRAN}, pcIdleIconFileName), 0) .

        FILE-INFORMATION:FILE-NAME = pcBusyIconFileName .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U THEN
           oBusyIcon = NEW Icon (FILE-INFORMATION:FULL-PATHNAME) .
        ELSE
           UNDO, THROW NEW AppError (SUBSTITUTE("Unable to start NotifyIconWaitStateManager. &1 cannot be found."{&TRAN}, pcBusyIconFileName), 0) .

        oNotifyIcon:VISIBLE = TRUE.

        THIS-OBJECT:ClearWaitState ("":U) .
&ENDIF
    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Clear wait-state indication for a specific message
        Notes:   This is not supported by this class.
                 ClearWaitState (pcMessage) will cal ClearWaitState ()
        @param pcMessage The message to clear the wait state for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID ClearWaitState (pcMessage AS CHARACTER).

         SUPER:ClearWaitState (pcMessage) .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
         oNotifyIcon:Icon = oIdleIcon .
         oNotifyIcon:Text = "":U .
&ENDIF
    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Locates the current form
        Notes:   Invoked from the context menu of the notify icon
        @return The current System.Windows.Forms.Form
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED System.Windows.Forms.Form FindCurrentForm ():

        IF VALID-OBJECT (FrameworkSettings:MdiContainer) THEN
            RETURN FrameworkSettings:MdiContainer .
        ELSE
            IF TYPE-OF (SESSION:FIRST-FORM, Progress.Windows.Form) THEN
                RETURN CAST(SESSION:FIRST-FORM, Progress.Windows.Form) .

    END METHOD.
&ENDIF

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the notify icons context menu items
        Notes:
        @param sender The reference to the sending object
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ContextMenuItemClickHandler (sender AS System.Object,
                                                       e AS System.EventArgs):

        IF sender = oProcessInfoMenuItem THEN
            Consultingwerk.Windows.DebugHelper:DisplayProcessInformation () .
        IF sender = oSessionInfoMenuItem THEN
            Consultingwerk.Windows.Session.SessionInfoForm:ShowSessionInformation () .
        IF sender = oQuitMenuItem THEN
            Consultingwerk.Windows.DebugHelper:KillCurrentProcess (TRUE) .
        IF sender = oDebugMenuItem THEN
            Consultingwerk.Windows.DebugHelper:StartDebugger () .

    END METHOD.
&ENDIF

&IF DEFINED (DotNetAccessible) NE 0 &THEN
      /*------------------------------------------------------------------------------
        Purpose: Event handler for the DoubleClick event of the notify icon
        Notes:   Activates the current main or primary Form
        @param sender The reference to the sending object
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID NotifyIconDoubleClickHandler (sender AS System.Object,
                                                        e AS System.EventArgs):

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        oForm = FindCurrentForm () .

        IF VALID-OBJECT (oForm) THEN DO:
            IF Progress.Util.EnumHelper:AreEqual (oForm:WindowState, FormWindowState:Minimized) THEN
                oForm:WindowState = FormWindowState:Normal .

            oForm:Activate () .
        END.

    END METHOD.
&ENDIF

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseMove event of the notify icon
        Notes:   Sets the text for the notify icon, whne no message is currently shown
        @param sender The reference to the sending object
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID NotifyIconMouseMoveHandler (sender AS System.Object,
                                                      e AS System.EventArgs):

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        IF oNotifyIcon:Text <> "":U THEN
            RETURN .

        oForm = THIS-OBJECT:FindCurrentForm () .

        IF VALID-OBJECT(oForm) THEN
            oNotifyIcon:Text = SUBSTRING (oForm:Text, 1, 63, "CHARACTER":U) .

    END METHOD.
&ENDIF

      /*------------------------------------------------------------------------------
        Purpose: Indicate a wait-state with a message to the user.  This wait-state
                 will be bouund to a specific caller
        Notes:   The pcMessage is generally a message code, which will be interpreted
                 by classes implementing the IWaitStateManager interface
                 This is not supported by this class.
                 SetWaitState (pcMessage) will call SetWaitState ()
        @param pcMessage The message related to the wait state
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID SetWaitState (pcMessage AS CHARACTER):

        SUPER:SetWaitState (pcMessage) .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF pcMessage = "RetrieveData":U THEN
            oNotifyIcon:Text = "Retrieving Data"{&TRAN} .
        ELSE
            oNotifyIcon:Text = pcMessage .

        oNotifyIcon:Icon = oBusyIcon .
&ENDIF

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the NotifyIconWaitStateManager class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC NotifyIconWaitStateManager ():

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF VALID-OBJECT (oNotifyIcon) THEN DO:
            oNotifyIcon:VISIBLE = FALSE .
            oNotifyIcon:Dispose () .

            DELETE OBJECT oNotifyIcon NO-ERROR .
        END.

        IF VALID-OBJECT (oIdleIcon) THEN DO:
            oIdleIcon:Dispose () .

            DELETE OBJECT oIdleIcon NO-ERROR .
        END.

        IF VALID-OBJECT (oBusyIcon) THEN DO:
            oBusyIcon:Dispose () .

            DELETE OBJECT oBusyIcon NO-ERROR .
        END.
&ENDIF
    END DESTRUCTOR.

END CLASS.
