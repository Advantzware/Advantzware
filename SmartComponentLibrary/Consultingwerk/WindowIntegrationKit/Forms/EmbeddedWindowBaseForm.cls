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
    File        : EmbeddedWindowBaseForm
    Purpose     : BaseForm for Embedded Windows Forms
    Syntax      :
    Description : This class implements the majority of properties
                  enforced by the IEmbeddedWindowForm Interface
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 18 23:26:29 CET 2009
    Notes       : This class is meant to be ABSTRACT - but the .NET
                  Visual Designer does not support to design classes with
                  abstract base classes

                  This class file can also be used as an include file. It
                  is a common requirement to workaround the non availbility
                  of multiple-inheritance. In this case, specify the
                  &AlterantiveBaseClass
                  &AlternativePackageName
                  &AlternativeClassName
                  as include file parameters. Note that when specifying one,
                  all need the others to be specified as well
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm                  FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.*    FROM PROPATH .
USING Infragistics.Win.Misc.*                        FROM ASSEMBLY .
USING Infragistics.Win.UltraWinToolbars.*            FROM ASSEMBLY .
USING Progress.Lang.*                                FROM PROPATH .
USING Progress.Windows.*                             FROM ASSEMBLY .
USING System.Windows.Forms.*                         FROM ASSEMBLY .

{Consultingwerk/products.i}

/* Mike Fechner, Consultingwerk Ltd. 30.06.2014
   Ability to override the Alternative ClassName/PackageName etc.
   This is useful to create custom base classes for EmbeddedWindowForms that
   should not be influenced by customizations of the EmbeddedWindowBaseForm,
   e.g. the ProcedureEditorBaseForm */
&IF "{&OverrideAlternativeClassName}":U NE "":U &THEN
&SCOPED-DEFINE AlternativeClassName {&OverrideAlternativeClassName}
&SCOPED-DEFINE AlternativePackageName {&OverrideAlternativePackageName}
&SCOPED-DEFINE AlternativeBaseClass {&OverrideAlternativeBaseClass}
&IF DEFINED (CustomEmbeddedWindowBaseFormImplements) &THEN
&UNDEFINE CustomEmbeddedWindowBaseFormImplements
&ENDIF
&ENDIF


&IF "{&CustomEmbeddedWindowBaseFormUsingInclude}":U NE "":U &THEN
{{&CustomEmbeddedWindowBaseFormUsingInclude}}
&ENDIF

CLASS
&IF DEFINED (AlternativeClassName) EQ 0 &THEN
      Consultingwerk.WindowIntegrationKit.Forms.EmbeddedWindowBaseForm
      INHERITS BaseForm
&ELSE
      {&AlternativePackageName}.{&AlternativeClassName}
      INHERITS
      {&AlternativeBaseClass}
&ENDIF
&IF "{&CustomEmbeddedWindowBaseFormImplements}":U NE "":U &THEN
    IMPLEMENTS {&CustomEmbeddedWindowBaseFormImplements}
&ENDIF
    :

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the embedded window widget (ABL window)
        Notes:   Read-Only, set by EmbedWindow method
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EmbeddedWindow AS WIDGET-HANDLE NO-UNDO
    GET .
    PROTECTED SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a comma-delimited list of all FRAME handles in a Window
        Notes:   This may be useful when FRAME's are realized (and made visible) after
                 the Window is shown. In this case it may be difficult to query the
                 FRAME handles from the Window widget-tree
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FrameHandles AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Flags if the IEmbeddedWindowForm has already been finalized
        Notes:   Prevents duplicate execution of embedfinalize.i
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Finalized AS LOGICAL NO-UNDO INIT FALSE
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls if the Embedded window should receive Focus, when the Form
                 is Activated or not
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FocusEmbeddedWindowOnActivated AS LOGICAL INITIAL TRUE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls if the Form should become an MdiChild of not.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MakeMdiChild AS LOGICAL NO-UNDO INIT ?
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the MENU-BAR of the embedded window
        Notes:   This property is required, as the embedding seems to remove the
                 reference to the menu-bar, but the menu-bar is still available
                 (the legacy ABL code).
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MENU-BAR AS WIDGET-HANDLE NO-UNDO
    GET .
    SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if whole MENU-BAR of the window should be rendered as
                 a toolbar instance
        Notes:   This is an optional property .
                 The initial value is FALSE .
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuBarAsToolbar AS LOGICAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if MENU-ITEM Type Menu-Items of the Main-Menubar
                 (Windows MENU-BAR) should be rendered as a separate toolbar instance
        Notes:   This is an optional property .
                 The initial value is FALSE .
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuItemsAsToolbar AS LOGICAL NO-UNDO
    GET .
    SET .

    /*------------------------------------------------------------------------------
        Purpose: Allows the definition of a minimum height in Chars for the embedded
                 ABL window
        Notes:   This property is not required by the standard WinKit functionality
                 and is reserved for customization
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MinHeightChars AS DECIMAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Allows the definition of a minimum width in Chars for the embedded
                 ABL window
        Notes:   This property is not required by the standard WinKit functionality
                 and is reserved for customization
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MinWidthChars AS DECIMAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Keeps the original window title (as entered in the AppBuilder)
        Notes:   This is useful if this string should be used as the standard caption
                 of Ribbon Tabs or similar and during the initalization runtime
                 specific attributes (like the client or user) are added to the window title.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OriginalWindowTitle AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: A Dictionary of Overlay Controls
        Notes:   OverlayControls is not a Dictionary with the STRING of the associated
                 widgets handle as the key and the reference to the .NET Control as
                 the value
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OverlayControls AS "System.Collections.Generic.Dictionary<System.String,System.Windows.Forms.Control>":U NO-UNDO
    GET:
        IF NOT VALID-OBJECT (THIS-OBJECT:OverlayControls) THEN
            THIS-OBJECT:OverlayControls = NEW "System.Collections.Generic.Dictionary<System.String,System.Windows.Forms.Control>":U () .

        RETURN THIS-OBJECT:OverlayControls .
    END.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the Widget that had Focus when this Form was
                 deactivated
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PreviousFocus AS HANDLE NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: A reference to the procedure that's owning this
                 EmbeddedWindowBaseForm instance
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ProcedureHandle AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls if the embedded window Form should be shown as a Dialog
        Notes:   Typically supresses the call of the Show() method in the embedfinalize.i
                 and the call to the Dispose() method in closewindow.i
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ShowAsDialog AS LOGICAL NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the UltraToolbarsManager instance contained
                 in the Embedded Window Form
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ToolbarsManager AS UltraToolbarsManager NO-UNDO
    GET .
    PROTECTED SET .

    /*------------------------------------------------------------------------------
        Purpose: A collection of the Progress.Windows.WindowsContainer controls used
                 by this Form
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY WindowContainers AS "System.Collections.Generic.List<WindowContainer>":U NO-UNDO
    GET.
    PROTECTED SET.

/* Mike Fechner, Consultingwerk Ltd. 12.06.2012
   Ability to customize Consultingwerk.Forms.BaseForm */
&IF "{&CustomEmbeddedWindowBaseFormIncludeFile}":U NE "":U &THEN
{{&CustomEmbeddedWindowBaseFormIncludeFile}}
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Default constructor
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC &IF DEFINED (AlternativeClassName) EQ 0 &THEN
                            EmbeddedWindowBaseForm
                       &ELSE
                            {&AlternativeClassName}
                       &ENDIF ():
        SUPER ().

        THIS-OBJECT:WindowContainers = NEW "System.Collections.Generic.List<WindowContainer>":U () .

        THIS-OBJECT:MakeMdiChild = WinKitForms:DefaultMakeMdiChild .

/* Mike Fechner, Consultingwerk Ltd. 12.06.2012
   Ability to customize Consultingwerk.Forms.BaseForm */
&IF DEFINED (OverrideEmbeddedWindowBaseFormConstructorInclude) EQ 0 AND
    "{&CustomEmbeddedWindowBaseFormConstructorInclude}":U NE "":U &THEN
{{&CustomEmbeddedWindowBaseFormConstructorInclude}}
&ENDIF

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Requests closing of the embedded window
        Notes:   Returns TRUE when window closed, otherwise FALSE
        @param plOk Logical value indicating if the windows were closed
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CloseEmbeddedWindows (OUTPUT plOk AS LOGICAL):

        DEFINE VARIABLE hWindow AS HANDLE NO-UNDO.

        ASSIGN hWindow = THIS-OBJECT:EmbeddedWindow .

        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO.

        APPLY "window-close":U TO
            hWindow .

        IF NOT VALID-HANDLE (hWindow) THEN
            plOk = TRUE .
        ELSE
            plOk = FALSE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Toggles Sensitivity of OverlayControls based on the sensitive status
                 of the corresponding Widget/Frame
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableDisableOverlayControls ():

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .

        {Consultingwerk/foreach.i "'System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.Control>'" oPair in THIS-OBJECT:OverlayControls}

            ASSIGN oControl = oPair:Value .

            IF TYPE-OF (oControl, IOverlayControl) THEN DO:
                ASSIGN hWidget = CAST (oControl, IOverlayControl):WidgetHandle .

                IF VALID-HANDLE (hWidget) THEN

                    oControl:Enabled = hWidget:SENSITIVE .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns an indeterminate array of the HANDLEs of the embedded Windows
        Notes:   Returns a size 1 array with the EmbeddedWindow HANDLE
        @return The array with the window HANDLE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE EXTENT GetEmbeddedWindows ():

        DEFINE VARIABLE hWindows AS HANDLE EXTENT 1 NO-UNDO .

        ASSIGN hWindows[1] = THIS-OBJECT:EmbeddedWindow .

        RETURN hWindows .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Activated event
        Notes:   Optionally puts focus into the EmbeddedWindow
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnActivated (e AS System.EventArgs):

        DEFINE VARIABLE hWidget    AS HANDLE          NO-UNDO.
        DEFINE VARIABLE oContainer AS WindowContainer NO-UNDO .
        DEFINE VARIABLE oControl   AS Control         NO-UNDO .

        SUPER:OnActivated (INPUT e).

        IF THIS-OBJECT:FocusEmbeddedWindowOnActivated AND THIS-OBJECT:WindowContainers:Count > 0 THEN DO:

            oControl = Consultingwerk.Util.FormHelper:FindActiveChildControl (THIS-OBJECT)  .

            IF VALID-HANDLE (THIS-OBJECT:PreviousFocus) THEN DO:

                oContainer = THIS-OBJECT:WindowContainerOfWidget (THIS-OBJECT:PreviousFocus) .

                IF VALID-OBJECT (oContainer) THEN DO ON ERROR UNDO, THROW:
                    THIS-OBJECT:ActiveControl = oContainer .

                    IF CAN-QUERY (THIS-OBJECT:PreviousFocus, "WINDOW":U) THEN DO:
                        hWidget = THIS-OBJECT:PreviousFocus:WINDOW .
                        APPLY "ENTRY":U TO hWidget .
                    END.

                    IF CAN-QUERY (THIS-OBJECT:PreviousFocus, "FRAME":U) THEN DO:
                        hWidget = THIS-OBJECT:PreviousFocus:FRAME .
                        APPLY "ENTRY":U TO hWidget .
                    END.

                    hWidget = THIS-OBJECT:PreviousFocus .

                    APPLY "entry":U TO hWidget .

                    @SuppressUnusedWarnings.
                    CATCH err AS Progress.Lang.Error :
                        /* ignore errors when oContainer is not visible or deactive */
                    END CATCH.
                END.
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Deactivate event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnDeactivate (e AS System.EventArgs):

        DEFINE VARIABLE hFocus AS HANDLE NO-UNDO.

        IF VALID-HANDLE (FOCUS) THEN DO:

            ASSIGN hFocus = FOCUS .

            /* Test for FILL-IN in updatable browser */
            IF NOT CAN-QUERY (hFocus, "WINDOW":U) THEN
                ASSIGN hFocus = hFocus:PARENT.

            {Consultingwerk/foreach.i WindowContainer oContainer in THIS-OBJECT:WindowContainers}

                IF oContainer:EmbeddedWindow = hFocus:WINDOW THEN DO:

                    THIS-OBJECT:PreviousFocus = hFocus .
                    LEAVE .
                END.
            END.
        END.

        SUPER:OnDeactivate(INPUT e).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosed event
        Notes:
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosed (e AS System.Windows.Forms.FormClosedEventArgs):

        SUPER:OnFormClosed(INPUT e).

        THIS-OBJECT:WindowContainers:Clear() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Brings Controls/Widget's in sync for handling .NET Events
        Notes:   Used for instance with the ToolClick event and updatable browsers
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID UpdateControlsForEventHandler ():

        DEFINE VARIABLE oActiveControl AS Control NO-UNDO .

        ASSIGN oActiveControl = THIS-OBJECT:ActiveControl .

        /* Mike Fechner, Consultingwerk Ltd. 31.10.2012
           When the Focus is on the RenderedBrowseControl, make sure to
           propagate  */
        IF TYPE-OF (oActiveControl, RenderedBrowseControl) THEN DO:
            CAST (oActiveControl, RenderedBrowseControl):SynchronizeCurrentColumn () .
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Toggles Visibily of OverlayControls based on the view/hide status
                 of the corresponding Widget/Frame
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ViewHideOverlayControls ():

        DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

        DEFINE VARIABLE oControl AS System.Windows.Forms.Control NO-UNDO .

        {Consultingwerk/foreach.i "'System.Collections.Generic.KeyValuePair<System.String,System.Windows.Forms.Control>'" oPair in THIS-OBJECT:OverlayControls}

            ASSIGN oControl = oPair:Value .

            IF TYPE-OF (oControl, IOverlayControl) THEN DO:
                ASSIGN hWidget = CAST (oControl, IOverlayControl):WidgetHandle .

                IF VALID-HANDLE (hWidget) AND VALID-HANDLE (hWidget:FRAME) THEN DO:

                    IF hWidget:VISIBLE AND NOT hWidget:HIDDEN AND
                       hWidget:FRAME:VISIBLE AND NOT hWidget:FRAME:HIDDEN THEN

                        oControl:Visible = TRUE .
                    ELSE
                        oControl:Visible = FALSE .
                END.
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the WindowContainer that contains the widget
        Notes:
        @param phWidget The handle of the WINDOW widget to locate
        @return The reference to the WindowContainer that contains the WINDOW
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Windows.WindowContainer WindowContainerOfWidget (phWidget AS HANDLE):

        /* Probably FILL-IN For updatable browse column */
        IF NOT CAN-QUERY (phWidget, "WINDOW":U) THEN
            ASSIGN phWidget = phWidget:PARENT .

        {Consultingwerk/foreach.i WindowContainer oContainer in THIS-OBJECT:WindowContainers}
            IF oContainer:EmbeddedWindow = phWidget:WINDOW THEN
                RETURN oContainer .
        END .

        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the EmbeddedWindowBaseForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC &IF DEFINED (AlternativeClassName) EQ 0 &THEN
                            EmbeddedWindowBaseForm
                       &ELSE
                            {&AlternativeClassName}
                       &ENDIF ():

        /* Mike Fechner, Consultingwerk Ltd. 14.03.2016
           WinKit-16: Allow to add NO-ERROR to EmbeddedWindowBaseForm Destructor statement
           Customers can add the NO-ERROR here by injecting an include file to this class
           using the GLOBAL-DEFINE CustomEmbeddedWindowBaseFormUsingInclude or
           CustomEmbeddedWindowBaseFormConstructorInclude in products.i
           Within that referenced include file, add
           &GLOBAL-DEFINE OverlayControlsClearNoError NO-ERROR */
        THIS-OBJECT:OverlayControls:Clear () {&OverlayControlsClearNoError}.

/* Mike Fechner, Consultingwerk Ltd. 12.06.2012
   Ability to customize Consultingwerk.Forms.BaseForm */
&IF "{&CustomEmbeddedWindowBaseFormDestructorInclude}":U NE "":U &THEN
{{&CustomEmbeddedWindowBaseFormDestructorInclude}}
&ENDIF

    END DESTRUCTOR.

END CLASS.
