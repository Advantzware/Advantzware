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
    File        : SmartLookupDialogForm
    Purpose     : Foundation for GUI for .NET Forms that can be used
                  as a ISmartBusinessEntityLookupDialog Form
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 09 15:53:09 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*             FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.Base.SmartLookupDialogForm
    INHERITS SmartWindowForm
    IMPLEMENTS ISmartBusinessEntityLookupDialog:

    /*------------------------------------------------------------------------------
        Purpose: Event raised when the lookups SmartBusinessEntityAdapter is initialized
        Notes:
        @param sender The reference to the object that raised the event
        @param e The AfterInitializeBusinessEntityAdapterEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT AfterInitializeBusinessEntityAdapter SIGNATURE VOID
                           (sender AS System.Object,
                            e AS AfterInitializeBusinessEntityAdapterEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Name of the Type of SmartBusinessEntityAdapter used to lookup
                 values from the backend.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AdapterType AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the AppServer Partition to be used
                 by this SmartDataAdapter instance
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the LookupBrowser instance
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowser AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Fields not to be shown in the SmartDataBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserExcludeFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Fields to be shown in the SmartDataBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Lookup Control that has invoked this SmartLookupDialogForm
                 instance
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupControl AS SmartBusinessEntityLookup NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Fields that allow to Filter on the Lookup Browser Dialog
        Notes:   Comma-Delimited, first entry is default
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogFilterFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Operators for the LookupDialogFilterFields
        Notes:   Comma-Delimited, first entry is default
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogFilterOperators AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: QuerySort used when opening the Lookup Browser Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogQuerySort AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: QueryString used when opening the Lookup Browser Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogQueryString AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Business Entity that returns the data
                 for the Lookup Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Entity Table that is retrieved from
                 the Lookup Dialog Business Entity
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Entity View Tables that is retrieved
                 from the Lookup Dialog Business Entity
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityView AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Field that is returned by the SmartLookup
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyField AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the default caption for the Cancel Button in the Ribbon
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LookupRibbonButtonCancel AS CHARACTER NO-UNDO INIT "Cancel"{&TRAN}
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the default caption for the Cancel Button in the Ribbon
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LookupRibbonButtonSelect AS CHARACTER NO-UNDO INIT "Select"{&TRAN}
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the default caption for the Lookup Ribbon Group
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY LookupRibbonGroupCaption AS CHARACTER NO-UNDO INIT "Selection"{&TRAN}
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets a reference to the SmartBusinessEntityAdapter instance
                 used by the SmartBusinessEntityAdapter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupSmartBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLookupDialogForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLookupDialogForm ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartLookupDialogForm class
        Notes:
        @param poLookupControl The reference to the SmartBusinessEntityLookup that launched this SmartLookupDialogForm instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartLookupDialogForm (poLookupControl AS SmartBusinessEntityLookup):
        SUPER ().

        THIS-OBJECT:LookupControl = poLookupControl .

        DEFINE VARIABLE oToolbar AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates the cancel button
        Notes:
        @param poRibbonGroup The RibbonGroup to add the button to
        @return The ButtonTool that has been created
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ButtonTool CreateCancelButton (poRibbonGroup AS RibbonGroup):

        DEFINE VARIABLE oButtonTool  AS ButtonTool           NO-UNDO .

        oButtonTool = NEW ButtonTool ("LookupCancelRecord":U) .
        oButtonTool:SharedProps:Caption = SmartLookupDialogForm:LookupRibbonButtonCancel .

        poRibbonGroup:Tools:ToolbarsManager:Tools:Add (oButtonTool) .

        poRibbonGroup:Tools:AddTool (oButtonTool:Key):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

        RETURN oButtonTool .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates the select button
        Notes:
        @param poRibbonGroup The RibbonGroup to add the button to
        @return The ButtonTool that has been created
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ButtonTool CreateSelectButton (poRibbonGroup AS RibbonGroup):

        DEFINE VARIABLE oButtonTool  AS ButtonTool           NO-UNDO .

        oButtonTool = NEW ButtonTool ("LookupSelectRecord":U) .
        oButtonTool:SharedProps:Caption = SmartLookupDialogForm:LookupRibbonButtonSelect .

        poRibbonGroup:Tools:ToolbarsManager:Tools:Add (oButtonTool) .

        poRibbonGroup:Tools:AddTool (oButtonTool:Key):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

        RETURN oButtonTool .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a Field Value from the Business Entity Adapter
        Notes:
        @param pcFieldName The name of the Field to return
        @return The value of the field
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetLookupFieldValue (pcFieldName AS CHARACTER):

        DEFINE VARIABLE oAdapter AS SmartBusinessEntityAdapter NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:PrimaryDataTarget) AND
          TYPE-OF (THIS-OBJECT:PrimaryDataTarget, SmartBusinessEntityAdapter) THEN DO:

          oAdapter = CAST (THIS-OBJECT:PrimaryDataTarget, SmartBusinessEntityAdapter) .

          RETURN oAdapter:GetFieldValues (pcFieldName) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the DataAdapter used by the SmartBusinessEntityLookupDialog
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeDataAdapter ():

        /* noop */

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the AfterInitializeBusinessEntityAdapter event
        Notes:
        @param e The AfterInitializeBusinessEntityAdapterEventArgs instance with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnAfterInitializeBusinessEntityAdapter (e AS AfterInitializeBusinessEntityAdapterEventArgs):

        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "AfterInitializeBusinessEntityAdapter":U) .

        THIS-OBJECT:AfterInitializeBusinessEntityAdapter:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):

        DEFINE VARIABLE oToolbar     AS UltraToolbarsManager NO-UNDO .
        DEFINE VARIABLE oRibbonTab   AS RibbonTab            NO-UNDO .
        DEFINE VARIABLE oRibbonGroup AS RibbonGroup          NO-UNDO .
        DEFINE VARIABLE oButtonTool  AS ButtonTool           NO-UNDO .

        SUPER:OnLoad (e) .

        IF NOT VALID-OBJECT (THIS-OBJECT:LookupControl) THEN
            RETURN .

        oToolbar = UltraToolbarsManager:FromForm (THIS-OBJECT) .

        IF VALID-OBJECT (oToolbar) AND oToolbar:Ribbon:Visible THEN DO:

            oRibbonTab = oToolbar:Ribbon:Tabs [0] .

            oRibbonGroup = NEW RibbonGroup ("LookupSelectRibbonGroup":U, SmartLookupDialogForm:LookupRibbonGroupCaption) .
            oRibbonTab:Groups:Insert (0, oRibbonGroup) .

            oButtonTool = THIS-OBJECT:CreateSelectButton (oRibbonGroup) .
            oToolbar:Tools:Add (oButtonTool) .

            oButtonTool = THIS-OBJECT:CreateSelectButton (oRibbonGroup) .
            oButtonTool = THIS-OBJECT:CreateCancelButton (oRibbonGroup) .

            oToolbar:ToolClick:Subscribe (ToolClickHandler) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the Title of the Lookup Dialog
        Notes:
        @param pcText The Text to be used as the Forms Title
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetText (pcText AS CHARACTER):

        THIS-OBJECT:Text = pcText .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager
                 instance contained in this Form
        Notes:
        @param sender The Object that has raised this event
        @param e The ToolEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ToolClickHandler (sender AS System.Object,
                                          e AS ToolEventArgs):

        CASE e:Tool:Key:
            WHEN "LookupSelectRecord":U THEN DO:
                THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK .
                THIS-OBJECT:Close () .
            END.
            WHEN "LookupCancelRecord":U THEN DO:
                THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Cancel .
                THIS-OBJECT:Close () .
            END.

        END CASE .

    END METHOD.

END CLASS.