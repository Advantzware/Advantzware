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
    File        : UltraBusinessEntityTesterForm
    Purpose     : User interface to test business entity functionality
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon Aug 03 10:55:03 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*                        FROM PROPATH .
USING Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.* FROM PROPATH .
USING Consultingwerk.Util.*                                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.ServiceManagerMonitoring.*     FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                             FROM ASSEMBLY .
USING System.Windows.Forms.*                                          FROM ASSEMBLY .
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.UltraBusinessEntityTesterForm
    INHERITS UltraBusinessEntityTesterFormDesigner
    IMPLEMENTS IOERABusinessEntityTesterForm :

    DEFINE PRIVATE VARIABLE cRegistryKey     AS CHARACTER       NO-UNDO.
    DEFINE PRIVATE VARIABLE oLoginController AS LoginController NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Business Entity used
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET:
        RETURN CAST(THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text .
    END GET .
    SET (arg AS CHARACTER):
        CAST(THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text = arg .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the text fo the StatusBar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StatusBarText AS CHARACTER NO-UNDO
    GET:
        RETURN oeraBusinessEntityTesterUserControl1:StatusBarText .
    END GET .
    SET (arg AS CHARACTER):
        ASSIGN oeraBusinessEntityTesterUserControl1:StatusBarText = arg.
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the visibility of the Status Bar
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StatusBarVisible AS LOGICAL NO-UNDO
    GET:
        RETURN oeraBusinessEntityTesterUserControl1:StatusBarVisible .
    END GET .
    SET (arg AS LOGICAL):
        ASSIGN oeraBusinessEntityTesterUserControl1:StatusBarVisible = arg.
    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraBusinessEntityTesterForm class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraBusinessEntityTesterForm ():
        SUPER ().

        &IF NOT PROVERSION BEGINS "10.":U &THEN
        ultraToolbarsManager1:Style = Infragistics.Win.UltraWinToolbars.ToolbarStyle:Office2010 .
        ultraToolbarsManager1:Ribbon:ApplicationMenuButtonImage = ? .
        &ENDIF

        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:OERABusinessEntityTesterForm = THIS-OBJECT.
        THIS-OBJECT:oeraBusinessEntityTesterUserControl1:ImageList = THIS-OBJECT:imageList1.
        THIS-OBJECT:ultraToolbarsManager1:SetContextMenuUltra(THIS-OBJECT:oeraBusinessEntityTesterUserControl1:TreeView, "TreeContextMenu":U).

        cRegistryKey = SUBSTITUTE ("&1~\&2":U, "Software~\Consultingwerk Ltd.~\Business Entity Designer":U, "UltraBusinessEntityTesterFormDesigner":U ).

        /* Disable GetData button */
        DisableTool ("GetSchema":U).
        DisableTool ("GetData":U).
        DisableTool ("Filter":U).

        LoadLRUEntitiesFromRegistry ().

        THIS-OBJECT:AllowDrop = TRUE .

        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe (ultraToolbarsManager_ToolClick).
        THIS-OBJECT:ultraToolbarsManager1:ToolValueChanged:Subscribe (ToolValueChangedHandler).

        /* Mike Fechner, Consultingwerk Ltd. 02.03.2015
           SCL-681 : Stop-After not supported on OE10.2B */
        IF PROVERSION BEGINS "10":U THEN DO:
            ultraToolbarsManager1:Tools["StopAfter":U]:SharedProps:Visible = FALSE .
            ultraToolbarsManager1:Tools["Stop-After:":U]:SharedProps:Visible = FALSE .
        END.

        ASSIGN oLoginController = NEW LoginController (ultraToolbarsManager1,
                                                       THIS-OBJECT) .

    END CONSTRUCTOR.

     /*------------------------------------------------------------------------------
        Purpose: Test if the currently entered text in the combobox is one of the
                 items of the comboboxlist. If not a new item is added at the top
                 of the list.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID AddEntityToComboList ():
        DEFINE VARIABLE oComboBoxTool AS ComboBoxTool NO-UNDO.

        ASSIGN oComboBoxTool = CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool).

        IF oComboBoxTool:Text > "":U THEN
            IF NOT oComboBoxTool:ValueList:FindStringExact (oComboBoxTool:Text) > -1 THEN
                oComboBoxTool:ValueList:ValueListItems:Insert (0, oComboBoxTool:ValueList:ValueListItems:Count + 1, oComboBoxTool:Text).
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Set the tools (ToolButton and MenuItem) disabled.
                 Enforced by the Interface IOERABusinessEntityTesterForm
        Notes:
        @param pcToolName The name of the tool to disable
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableTool (pcToolName AS CHARACTER):

        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists(pcToolName) THEN
            THIS-OBJECT:ultraToolbarsManager1:Tools[pcToolName]:SharedProps:Enabled = FALSE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Set the tools (ToolButton and MenuItem) enabled.
                 Enforced by the Interface IOERABusinessEntityTesterForm
        Notes:
        @param pcToolName The name of the tool to enable
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableTool (pcToolName AS CHARACTER):

        IF THIS-OBJECT:ultraToolbarsManager1:Tools:Exists(pcToolName) THEN
            THIS-OBJECT:ultraToolbarsManager1:Tools[pcToolName]:SharedProps:Enabled = TRUE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event
        Notes:   Save the LRU entities to the registry
        @param e The FormClosingEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnFormClosing (e AS FormClosingEventArgs):

        DEFINE VARIABLE cEntityList AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

        IF CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):ValueList:ValueListItems:Count > 0 THEN
            DO i = 0 TO CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):ValueList:ValueListItems:Count - 1:
                cEntityList = cEntityList + ",":U + CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):ValueList:ValueListItems:Item[i]:DisplayText.
        END.
        cEntityList = TRIM (cEntityList, ",":U).

        Consultingwerk.Framework.Registry:SetRegistryValue ("User":U,
                                                            cRegistryKey,
                                                            "LRUEntities":U,
                                                            cEntityList) .

        @SuppressUnusedWarnings.
        CATCH ple AS Progress.Lang.Error :
            /* empty catch blck */
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the DragEnter event of the Form
        Notes:
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnDragEnter(e AS DragEventArgs):

        DEFINE VARIABLE cFiles AS CHARACTER NO-UNDO EXTENT .

        IF DragAndDropHelper:IsFileDrop (e:Data) THEN DO:

            cFiles = DragAndDropHelper:GetDropFileNames (e:Data) .

            IF EXTENT (cFiles) = 1 AND cFiles [1] MATCHES "*.cls":U THEN
                e:Effect = DragDropEffects:Link .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the DragDrop event of the Form
        Notes:
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnDragDrop (e AS DragEventArgs):

        DEFINE VARIABLE cFiles AS CHARACTER           NO-UNDO EXTENT .
        DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO .

        IF DragAndDropHelper:IsFileDrop (e:Data) THEN DO:

            cFiles = DragAndDropHelper:GetDropFileNames (e:Data) .

            IF EXTENT (cFiles) = 1 AND cFiles [1] MATCHES "*.cls":U THEN DO:

                oClass = ClassHelper:FileNameToClass (cFiles[1]) .

                IF NOT VALID-OBJECT (oClass) THEN
                    RETURN .

                IF oClass:IsA ("Consultingwerk.OERA.IBusinessEntity":U) THEN DO:

                    CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text =
                        oClass:TypeName .

                    THIS-OBJECT:oeraBusinessEntityTesterUserControl1:FetchEntity (oClass:TypeName) .

                END.
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Load the LRU entities from the registry
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LoadLRUEntitiesFromRegistry ():

        DEFINE VARIABLE cEntityList AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

        cEntityList = Consultingwerk.Framework.Registry:GetRegistryValue ("User":U,
                                                                          cRegistryKey,
                                                                          "LRUEntities":U) NO-ERROR .


        IF NUM-ENTRIES (cEntityList, ",":U) > 0 THEN
            DO i = 1 TO NUM-ENTRIES (cEntityList, ",":U):
                CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):ValueList:ValueListItems:Add(i, ENTRY (i, cEntityList, ",":U)).
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Monitors the Busienss Services using the ServiceManagerMonitoringForm
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID MonitorBusinessServices():

        DEFINE VARIABLE oForm AS ServiceManagerMonitoringForm NO-UNDO .

        oForm = NEW ServiceManagerMonitoringForm() .

        WAIT-FOR oForm:ShowDialog () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (INPUT e).

        DO ON ERROR UNDO, THROW:

            IF THIS-OBJECT:BusinessEntityName > "":U THEN
                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:FetchEntity (THIS-OBJECT:BusinessEntityName).

            CATCH err AS Progress.Lang.Error:
                ErrorHelper:ShowErrorMessage (err) .
            END CATCH.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Selectes all Tables
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectAll ():

        oeraBusinessEntityTesterUserControl1:SelectAll () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deselects all Tables
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectNone ():

        oeraBusinessEntityTesterUserControl1:SelectNone () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolValueChanged event of the UltraToolbarsManager
                 instance
        Notes:   En- / Disable the FetchEntity ToolButton
        @param sender The reference to the object that raised the event
        @param e The ToolEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ToolValueChangedHandler (sender AS System.Object,
                                                e      AS Infragistics.Win.UltraWinToolbars.ToolEventArgs):

        CASE e:Tool:Key :
            WHEN "EntityName":U THEN DO:
                IF CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text > "":U THEN
                    EnableTool ("GetSchema":U).
                ELSE
                    DisableTool ("GetSchema":U).
            END.
        END CASE.

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple, "Error while changing the Tool state!") .
        END CATCH.
    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClickEventArgs event of the UltraToolbarsManager
                 instance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager_ToolClick (sender AS System.Object,
                                                        e      AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iValue AS INTEGER NO-UNDO.

        CASE e:Tool:Key :
            WHEN "GetSchema":U THEN DO:
                IF CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text = "":U THEN
                    RETURN.

                AddEntityToComboList ().

                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:FetchEntity (CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text).
            END.
            WHEN "GetData":U THEN DO:
                IF NOT CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text > "":U THEN
                    RETURN.

                ASSIGN cValue = CAST(ultraToolbarsManager1:Tools["BatchSize":U], Infragistics.Win.UltraWinToolbars.ComboBoxTool):Text
                       iValue = INTEGER (cValue) NO-ERROR .

                IF iValue > 0 THEN .
                ELSE
                   iValue = 0 .

                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:BatchSize = iValue .

                ASSIGN cValue = CAST(ultraToolbarsManager1:Tools["StopAfter":U], Infragistics.Win.UltraWinToolbars.ComboBoxTool):Text
                       iValue = INTEGER (cValue) NO-ERROR .

                IF iValue > 0 THEN .
                ELSE
                   iValue = 0 .

                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:StopAfter = iValue .

                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:CustomContext = CAST (ultraToolbarsManager1:Tools["tbCustomContext":U], TextBoxTool):Text .
                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:CustomParameterObject = tbCustomParameter:Text .

                THIS-OBJECT:oeraBusinessEntityTesterUserControl1:FetchDataFromBackend (CAST (THIS-OBJECT:ultraToolbarsManager1:Tools["EntityName":U], ComboBoxTool):Text).
            END.
            WHEN "Filter":U THEN THIS-OBJECT:oeraBusinessEntityTesterUserControl1:ShowFilterDialog ().
            WHEN "RemoveFilter":U THEN THIS-OBJECT:oeraBusinessEntityTesterUserControl1:RemoveFilter ().
            WHEN "StopEntities":U THEN oeraBusinessEntityTesterUserControl1:ShutDownBusinessEntities () .
            WHEN "Monitor Business Services":U THEN THIS-OBJECT:MonitorBusinessServices () .
            WHEN "SelectAll":U THEN SelectAll () .
            WHEN "SelectNone":U THEN SelectNone () .
            WHEN "Data Source Query Info":U THEN
                oeraBusinessEntityTesterUserControl1:ShowDataSourceQueryInfo () .
            WHEN "Exit":U THEN THIS-OBJECT:Close ().
        END CASE.

        CATCH ple AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (ple, SUBSTITUTE ("Error while performing the task &1!", QUOTER (StringHelper:WellFormFromCamelCase(e:Tool:Key)))) .
        END CATCH.
    END METHOD.

 END CLASS.
