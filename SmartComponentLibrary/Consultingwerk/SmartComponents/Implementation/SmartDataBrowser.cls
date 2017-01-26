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
    File        : SmartDataBrowser
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Feb 18 09:19:52 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                                    FROM PROPATH .
USING Consultingwerk.SmartComponents.*                              FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Base.SmartBrowserControl       FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartDatasetAdapter       FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                         FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*               FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*                   FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.ISmartViewerControl FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.Design.*            FROM ASSEMBLY .
USING Consultingwerk.Util.*                                         FROM PROPATH .
USING Consultingwerk.Windows.Util.*                                 FROM PROPATH .
USING Infragistics.Win.*                                            FROM ASSEMBLY .
USING Infragistics.Win.UltraWinGrid.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                               FROM ASSEMBLY .
USING System.ComponentModel.*                                       FROM ASSEMBLY .
USING System.ComponentModel.Design.*                                FROM ASSEMBLY .
USING Progress.Util.*                                               FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDataBrowser
    INHERITS SmartBrowserControl
    IMPLEMENTS ISmartDataTargetWithBindingSource,
               ISmartDataBrowser,
               ICustomTypeDescriptor, IHasDesignerVerbs:

    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE PRIVATE
    &IF PROVERSION BEGINS "10.2":U &THEN
    STATIC
    &ENDIF TEMP-TABLE ttDefaultActionSubscriber NO-UNDO
        FIELD RecordOwner             AS Progress.Lang.Object
        FIELD DefaultActionSubscriber AS Progress.Lang.Object
        INDEX RecordOwner RecordOwner DefaultActionSubscriber
        .

    DEFINE VARIABLE lSetSortProperties AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE lFirstTime         AS LOGICAL NO-UNDO INIT TRUE . /* first time load of stored design */

    DEFINE VARIABLE cConstructorsSettingsKey AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lScrolling            AS LOGICAL NO-UNDO INIT FALSE .

    DEFINE VARIABLE lInOnBeforeSortChange AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE lUpdateSortIndicator  AS LOGICAL NO-UNDO INIT FALSE .

    DEFINE VARIABLE lInited               AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE VARIABLE lFormClosedSubscribed AS LOGICAL NO-UNDO INIT FALSE .

    /* Mike Fechner, Consultingwerk Ltd. 22.04.2010
       Core implementation of ICustomTypeDescriptor */
    /* Mike Fechner, Consultingwerk Ltd. 20.06.2014
       SCL-310: The SmartDataBrowser does no longer implement the
                ICustomTypeDescriptor Interface as this seemed to
                be responsible for frequent freezing of the Visual
                Designer reported by a customer. */
    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}
    {Consultingwerk/SmartComponents/Base/CreateVerbs.i}

    /*------------------------------------------------------------------------------
        Purpose: Event that combines double click on a data row or the selection
                 of the Return or Enter key
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DefaultAction DELEGATE System.EventHandler .

    &IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Static event raised whenever a SmartDataBrowser is activated. This
                 event is useful to control a central UltraGridColumnChooser etc.
        Notes:   This event may be removed using the NoStaticsInHybrids setting
                 in Consultingwerk/products.i. This allows to avoid static members
                 in hybrid classes SmartDataBrowser and SmartBusinessEntityAdapter
                 to avoid issues with the combination of static and hybrids on 11.2
                 and 11.3, OE defect OE00240707 http://knowledgebase.progress.com/articles/Article/000042229?popup=true
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC EVENT SmartDataBrowserActivated DELEGATE System.EventHandler.
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Raised when the SmartFilterActiveChanged property is changed
        Notes:
        @param sender The object that raised the SmartFilterActiveChanged event
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT SmartFilterActiveChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                 e AS System.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Get the BindingSource of the current Class.
                 This Property is used to get a possibly existing DesignTime
                 BindingSource from the SmartDataBrowser Instance. A SmartDataAdapter
                 derived Class gets registered as the SmartDataSource for the
                 SmartDataBrowser and RegisterSmartDataTarget () is called to do the
                 handshake between those two Classes.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO
    GET:
        IF VALID-OBJECT(THIS-OBJECT:DataSource) AND
           TYPE-OF(THIS-OBJECT:DataSource, Progress.Data.BindingSource) THEN
                RETURN CAST(THIS-OBJECT:DataSource, Progress.Data.BindingSource).
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the
                 Visual Designer)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the DefaultAction (DoubleClickRow or Enter) in the
                 SmartDataBrowser starts EnabledFields (Update) in a Viewer linked to
                 the same SmartDataBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EditInViewerOnDefaultAction AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the SmartDataBrowser should invoke a "Fetch all"
                 from it's data source when grouping by
        Notes:   Ensures proper group by, as the UltraGrid requires all data for group
                 by
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FetchAllRecordsOnGroupBy AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 will be excluded from the .NET TypeDescriptor properties list
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY HiddenProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should not show up in the Visual Designers Property Grid
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY NonBrowsableProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the demanded Visible Row Index that the ScrollToCurrentRow
                 should set when scrollign to the current row
        Notes:   Zero based version of the ABL Browse widget SET-REPOSITIONED-ROW
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RepositionedRow AS INTEGER INITIAL 0 NO-UNDO
    GET.
    SET.

&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Controls if SmartDataBrowsers should store the column settings
                 when the containing Form is closed.
        Notes:   This static property is not present when the NoStaticsInHybrids
                 preprocessor switch is set in Consultingwerk/products.i - instead
                 the preprocessor
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY SaveColumnSettings AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Contains the key used to store settings of this Browser in the
                 Registry
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SettingsKey AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the SmartFilter functioanlity should be used with
                 a filter row of the UltraGrid
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartFilterActive AS LOGICAL NO-UNDO INIT FALSE
    GET.
    SET (arg AS LOGICAL):
        THIS-OBJECT:SmartFilterActive = arg .

        OnSmartFilterActiveChanged (System.EventArgs:Empty) .

        IF THIS-OBJECT:Initializing = FALSE AND THIS-OBJECT:SmartFilterActive THEN
            THIS-OBJECT:EnableSmartFilterUserInterface () .

    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the SmartFilter functionality uses MATCHES
        Notes:   MATCHES uses a flexible combination with BEGINS based on entered text
                 or BEGINS for character fields
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartFilterUseMatches AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the SmartFilter functionality supports multiple
                 operators
        Notes:   When set to TRUE the SmartFilter will support all operators that
                 are reasonable for the column data type
                 The use of the SmartFilterUseOperators feature overrides the
                 SmartFilterUseMatches setting as the user has full control over the
                 filter comparison used.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartFilterUseOperators AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Depriciated property to invoke StoreVisibleGridColumns from the
                 property grid.
        Notes:   Protected GET to remain compability with old InitializeComponents
                 code that assigns a value. There is no need to GET and SET this
                 property anymore
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY StoreVisibleGridColumns AS LOGICAL INITIAL TRUE NO-UNDO
    GET:
        RETURN TRUE.
    END.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Stores the Columns of the Grid that should be visible
        Notes:   All other columns will be hidden
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY VisibleGridColumns AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Instance of SmartDataBrowser and subscribes Handlers
                 to Events.
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataBrowser ():

        SUPER ().

        SetDesignerProperties () .

        THIS-OBJECT:InitializeLayout:Subscribe(InitializeLayoutHandler).

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           A key to uniquely idnetify this instance, typically overwritten by the
           containing Form's initializeComponents method.

           The variable cConstructorsSettingsKey is used to detect if this value
           has been changed. So that SettingsKey will get used only when it was
           persisted in the Forms intializeComponent method. This check is for
           backwards compability with SmartComponent Library screens that have not
           (yet) set a SettingsKey in the FormDesign. These Forms should not
           write to new GUIDs in the registry every time. */
        THIS-OBJECT:SettingsKey = GUID .
        ASSIGN cConstructorsSettingsKey = THIS-OBJECT:SettingsKey .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Activates the current grid row during Update/Delete/Copy in a Viewer
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateCurrentRow ():

        DEFINE VARIABLE oRow AS UltraGridRow NO-UNDO .

        IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) THEN
            RETURN .

        IF THIS-OBJECT:BindingSource:Position >= 0 THEN .
        ELSE
            RETURN .

        oRow = THIS-OBJECT:Rows:GetRowWithListIndex (THIS-OBJECT:BindingSource:Position) .

        IF VALID-OBJECT (oRow) AND VALID-OBJECT (THIS-OBJECT:ActiveRowScrollRegion) THEN DO:
            THIS-OBJECT:ActiveRowScrollRegion:ScrollRowIntoView (oRow) .
            oRow:Activate () .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Callback method for the AfterRetrieveData event
        Notes:   Executed by the SmartDataAdapter class with the AfterRetrieveData
                 event
        @param sender The reference to the ISmartDataSource that invoked the Callback method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE VOID AfterRetrieveDataCallback (sender AS Consultingwerk.SmartComponents.Interfaces.ISmartDataSource):

        DEFINE VARIABLE oAdapter AS SmartDatasetAdapter NO-UNDO .
        DEFINE VARIABLE cIndex   AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cSort    AS CHARACTER           NO-UNDO .

        THIS-OBJECT:FormatColumns () .

        SUPER:AfterRetrieveDataCallback (sender) .

        /* Mike Fechner, Consultingwerk Ltd. 05.10.2013
           When the Data Adapter does not have a QuerySort set, let's indicate
           the sort arrows on the grid columns based on the Queries first used
           index */
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter) THEN DO:
            oAdapter = CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter) .

            IF Consultingwerk.CharacterType:IsNullOrEmpty (oAdapter:QuerySort) THEN DO:
                IF oAdapter:QueryHandle:INDEX-INFORMATION (1) > "":U THEN DO:

                    IF ENTRY (1, oAdapter:QueryHandle:INDEX-INFORMATION (1)) = "WHOLE-INDEX":U THEN
                        cIndex = ENTRY (2, oAdapter:QueryHandle:INDEX-INFORMATION (1)) .
                    ELSE
                        ENTRY (1, oAdapter:QueryHandle:INDEX-INFORMATION (1)) .

                    ASSIGN cSort = BufferHelper:BufferIndexByClause (oAdapter:QueryHandle:GET-BUFFER-HANDLE (1),
                                                                     cIndex,
                                                                     TRUE) .
                    IF cSort > "":U THEN
                        THIS-OBJECT:UpdateSortIndicator (cSort) .
                END.
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Applies stored Grid Settings
        Notes:
        @param pcSettings The character value representing the grid settings
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyGridSettings (pcSettings AS CHARACTER):

        DEFINE VARIABLE oSettings AS UltraGridSettings NO-UNDO .

        ASSIGN oSettings = NEW UltraGridSettings (THIS-OBJECT) .

        oSettings:ApplyStoredSettings (pcSettings) .

        DELETE OBJECT oSettings .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Clears the Filter Row
        Notes:   Method overridden in the SmartUpdatableBrowse to allow to save pending
                 updates before clearing the filter
        @param poUIElement The UIElement that was clicked to distinguish between clear filter row and a single filter cell
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ClearFilterRow (poUIElement AS FilterClearButtonUIElement):

        DEFINE VARIABLE oColumn    AS UltraGridColumn            NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 13.07.2015
           SCL-916 - Ability to clear only a single column filter */
        oColumn = CAST (poUIElement, Infragistics.Win.UltraWinGrid.FilterClearButtonUIElement):Column .

        IF VALID-OBJECT (oColumn) THEN
            oColumn:Band:ColumnFilters[oColumn:Key]:ClearFilterConditions() .
        ELSE DO:
            {Consultingwerk/foreach.i UltraGridBand oBand in THIS-OBJECT:DisplayLayout:Bands}
                oBand:ColumnFilters:ClearAllFilters() .
            END.
        END.

        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           TYPE-OF (THIS-OBJECT:SmartDataSource, ISmartFilterTarget) THEN

            CAST (THIS-OBJECT:SmartDataSource, ISmartFilterTarget):RetrieveData () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disable the usage of the Del key for the entire Grid
        Notes:   Invoked from the method InitializeLayoutHandler
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableDelKeyUsage ():

        THIS-OBJECT:DisplayLayout:Override:AllowDelete = DefaultableBoolean:False.

        {Consultingwerk/foreach.i UltraGridBand oBand in THIS-OBJECT:DisplayLayout:Bands}

            oBand:Override:AllowDelete = DefaultableBoolean:False.

        END.

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Disable the Controls during an Update.
                This Method is called by the SmartDataAdapter by a call to
                BeginUpdateState () which is defined in the Interface ISmartDataSource.
       Notes:   Enforced by Interface ISmartDataTarget.
       @param poSmartDataTarget The ISmartDataTarget instance that is in update mode
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID DisableForUpdate (poSmartDataTarget AS ISmartDataTarget):

        DEFINE VARIABLE oUpdatingAdapter AS SmartBusinessEntityAdapter NO-UNDO .
        DEFINE VARIABLE oOwnAdapter      AS SmartBusinessEntityAdapter NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 13.02.2014
           An updatable browser that is linked to a SmartDatasetChildAdpater which is
           getting it's data from the same SmartBusinessEntityAdapter that the
           data target that is getting into update mode, we do not need to disable
           the browser, when the SmartBusinessEntityAdapter is not using AutoCommit */
        ASSIGN oUpdatingAdapter = SmartBusinessEntityAdapterSettings:FromSmartDataTarget (poSmartDataTarget)
               oOwnAdapter      = SmartBusinessEntityAdapterSettings:FromSmartDataTarget (THIS-OBJECT) .

        IF VALID-OBJECT (oUpdatingAdapter) AND VALID-OBJECT (oOwnAdapter) AND
           oUpdatingAdapter = oOwnAdapter  AND oUpdatingAdapter:AutoCommit = FALSE THEN

            RETURN .

        SUPER:DisableForUpdate (poSmartDataTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disables selection of multiple rows in the SmartDataBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableMultiSelection ():


        THIS-OBJECT:SetMultiSelection (FALSE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Enabels selection of multiple rows in the SmartDataBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableMultiSelection ():

        THIS-OBJECT:SetMultiSelection (TRUE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Enables te SmartFilter UI
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EnableSmartFilterUserInterface ():

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        THIS-OBJECT:DisplayLayout:Override:AllowRowFiltering = DefaultableBoolean:True .
        THIS-OBJECT:DisplayLayout:Override:FilterUIType = FilterUIType:FilterRow .
        THIS-OBJECT:DisplayLayout:Override:FilterEvaluationTrigger = FilterEvaluationTrigger:OnEnterKey .
        THIS-OBJECT:DisplayLayout:Override:FilterOperandStyle = FilterOperandStyle:UseColumnEditor.

        IF THIS-OBJECT:SmartFilterUseOperators = FALSE THEN
            THIS-OBJECT:DisplayLayout:Override:FilterOperatorLocation = FilterOperatorLocation:Hidden .
        ELSE DO:
            THIS-OBJECT:DisplayLayout:Override:FilterOperatorLocation = FilterOperatorLocation:WithOperand .

            THIS-OBJECT:InitializeColumnFilterOperatorItems () .
        END.

        THIS-OBJECT:DisplayLayout:Override:FilterClearButtonLocation = FilterClearButtonLocation:Row .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Exports the browsers contents to an Excel file using the UltraGrid Excel Exported
        Notes:
        @return The filename of the exported document
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ExportToExcel ():

        DEFINE VARIABLE oExportController AS IUltraGridExcelExportController NO-UNDO.
        DEFINE VARIABLE cFileName         AS CHARACTER                       NO-UNDO.

        oExportController = {Consultingwerk/get-service.i Consultingwerk.Windows.Util.IUltraGridExcelExportController "NEW Consultingwerk.Windows.Util.UltraGridExcelExportController ()"} .

        cFileName = oExportController:Export (THIS-OBJECT) .

        IF Consultingwerk.CharacterType:IsNotNullOrEmpty (cFileName) THEN
            WindowsExplorerHelper:ShowFileInExplorer (cFileName).

        RETURN cFileName.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Exports the browsers contents to an Excel file using the UltraGrid Excel Exported
        Notes:
        @param poOptions The IUltraGridExcelExportParameter for this export
        @return The filename of the exported document
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER ExportToExcel (poOptions AS IUltraGridExcelExportParameter):

        DEFINE VARIABLE oExportController AS IUltraGridExcelExportController NO-UNDO.

        oExportController = {Consultingwerk/get-service.i Consultingwerk.Windows.Util.IUltraGridExcelExportController "NEW Consultingwerk.Windows.Util.UltraGridExcelExportController ()"} .

        RETURN oExportController:Export (THIS-OBJECT, poOptions).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the default column filter type for numeric and date columns
                 to GreaterThanOrEqualTo
        Notes:   Keeps the default visualization in sync with the comparison types
                 used by UltraGridFilterRowHelper class
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormatColumnFilters ():

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinGrid.UltraGridColumn oColumn in THIS-OBJECT:DisplayLayout:Bands[0]:Columns}

            IF oColumn:DataType:Equals (Progress.Util.TypeHelper:GetType ("System.Int32":U))   OR
               oColumn:DataType:Equals (Progress.Util.TypeHelper:GetType ("System.Int64":U))   OR
               oColumn:DataType:Equals (Progress.Util.TypeHelper:GetType ("System.Decimal":U)) OR
               oColumn:DataType:Equals (Progress.Util.TypeHelper:GetType ("System.DateTime":U)) THEN

                oColumn:FilterOperatorDefaultValue = FilterOperatorDefaultValue:GreaterThanOrEqualTo .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Formats numeric columns (Integer, Decimal).
                 Invoked from the method InitializeLayoutHandler.

                 All Bands and Columns are processed and if the DisplayStyle has not
                 been changed in the Visual Designer, Integer and Decimal fields get
                 formatted according to their datatype.
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormatColumns ():

        DEFINE VARIABLE oBindingSource             AS Progress.Data.BindingSource NO-UNDO .
        DEFINE VARIABLE iBand                      AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE iColumn                    AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE iBuffer                    AS INTEGER                     NO-UNDO.
        DEFINE VARIABLE oBand                      AS UltraGridBand               NO-UNDO.
        DEFINE VARIABLE oColumn                    AS UltraGridColumn             NO-UNDO.
        DEFINE VARIABLE hDataHandle                AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hBuffer                    AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE hColumn                    AS HANDLE                      NO-UNDO.
        DEFINE VARIABLE oInt32Type                 AS System.Type                 NO-UNDO.
        DEFINE VARIABLE oDecimalType               AS System.Type                 NO-UNDO.
        DEFINE VARIABLE lTempSetSortProperties     AS LOGICAL                     NO-UNDO INIT FALSE .
        DEFINE VARIABLE cLargeQueryColumns         AS CHARACTER                   NO-UNDO.
        DEFINE VARIABLE cLargeBindingSourceColumns AS CHARACTER                   NO-UNDO.
        DEFINE VARIABLE cFieldName                 AS CHARACTER                   NO-UNDO.

        /* Format numeric columns */
        oInt32Type = Progress.Util.TypeHelper:GetType("System.Int32":U) .
        oDecimalType = Progress.Util.TypeHelper:GetType("System.Decimal":U) .

        IF VALID-OBJECT(THIS-OBJECT:DataSource) AND TYPE-OF (THIS-OBJECT:DataSource, Progress.Data.BindingSource) THEN DO:
            oBindingSource = CAST (THIS-OBJECT:DataSource, Progress.Data.BindingSource) .

            IF VALID-OBJECT (oBindingSource:TableSchema) THEN
                ASSIGN cLargeBindingSourceColumns = Consultingwerk.Util.BindingSourceHelper:BindingSourceLargeColumns
                                                            (oBindingSource:TableSchema) .

            /* Mike Fechner, Consultingwerk Ltd. 15.09.2010
               Disable sorting in first band no regardless if there's a query or not
               because this may be critical to the initial sort */
            IF THIS-OBJECT:DisplayLayout:Bands:Count > 0 AND lUpdateSortIndicator = FALSE THEN DO:
                ASSIGN oBand = THIS-OBJECT:DisplayLayout:Bands [0] .

                DO iColumn = 0 TO oBand:Columns:Count - 1:
                    ASSIGN oColumn = oBand:Columns [iColumn] .

                   IF ListHelper:EntryIsInList (oColumn:Key, cLargeBindingSourceColumns) THEN
                   DO ON ERROR UNDO, THROW:
                       oColumn:SortIndicator = SortIndicator:Disabled .

                       CATCH ex AS System.ArgumentException:
                             /* Ignore potential System.ArgumentException:
                                Can't modify the sorted columns collection while
                                in the BeforeSortChange event */
                       END CATCH.
                   END. /* DO ON ERROR UNDO, THROW */
                END.
            END.

            /* Mike Fechner, Consultingwerk Ltd. 26.05.2009
               BindingSource needs valid handle to continue */
            ASSIGN hDataHandle = oBindingSource:HANDLE.

            IF NOT VALID-HANDLE(hDataHandle) THEN
                RETURN .

            IF hDataHandle:TYPE = "QUERY":U THEN
                ASSIGN cLargeQueryColumns = Consultingwerk.Util.QueryHelper:QueryLargeColumns (hDataHandle) .

            bandLoop: DO iBand = 0 TO THIS-OBJECT:DisplayLayout:Bands:Count - 1:

                ASSIGN oBand = THIS-OBJECT:DisplayLayout:Bands[iBand] .

                CASE hDataHandle:TYPE:
                    WHEN "DATASET":U THEN
                        ASSIGN hBuffer = hDataHandle:GET-BUFFER-HANDLE(oBand:Key) .
                    WHEN "BUFFER":U THEN
                        ASSIGN hBuffer = hDataHandle .
                    WHEN "QUERY":U THEN
                        ASSIGN hBuffer = ? .
                    OTHERWISE
                        NEXT bandLoop .
                END CASE.

                columnLoop: DO iColumn = 0 TO oBand:Columns:Count - 1:

                    ASSIGN oColumn = oBand:COLUMNS[iColumn] .

                    /* Mike Fechner, Consultingwerk Ltd. 24.07.2009
                       Disable sorting of non-primary query buffer */
                    IF NOT lSetSortProperties AND lUpdateSortIndicator = FALSE AND hDataHandle:TYPE = "QUERY":U THEN
                    DO ON ERROR UNDO, THROW:
                       IF INDEX (oColumn:Key, "[":U) > 0 THEN
                           ASSIGN cFieldName = BufferHelper:ParseFieldName(oColumn:Key):FieldName .
                       ELSE
                           ASSIGN cFieldName = oColumn:Key .

                       IF Consultingwerk.Util.QueryHelper:ColumnsBufferIndex (cFieldName, hDataHandle) > 1 THEN
                           oColumn:SortIndicator = SortIndicator:Disabled .
                       ELSE IF ListHelper:EntryIsInList (oColumn:Key, cLargeQueryColumns) THEN
                           oColumn:SortIndicator = SortIndicator:Disabled .

                       lTempSetSortProperties = TRUE .

                       CATCH ex AS System.ArgumentException:
                          /* Ignore potential System.ArgumentException:
                             Can't modify the sorted columns collection while
                             in the BeforeSortChange event */
                       END CATCH.
                    END. /* DO ON ERROR UNDO, THROW */

                    /* Do not format column, if format has been set (in Visual Designer) */
                    IF Progress.Util.EnumHelper:AreNotEqual(oColumn:CellDisplayStyle,
                                                            CellDisplayStyle:Default) THEN
                        NEXT columnLoop .

                    /* Find handle to buffer field, to allow translation of
                       FORMAT etc. */
                    IF VALID-HANDLE(hBuffer) THEN
                         ASSIGN hColumn = hBuffer:BUFFER-FIELD(oColumn:Key) NO-ERROR .
                    ELSE DO:
                        DO iBuffer = 1 TO hDataHandle:NUM-BUFFERS :
                            hBuffer = hDataHandle:GET-BUFFER-HANDLE(iBuffer) .

                            ASSIGN hColumn = hBuffer:BUFFER-FIELD(oColumn:Key) NO-ERROR .

                            IF VALID-HANDLE(hColumn) THEN
                                LEAVE .
                            ELSE
                                ASSIGN hBuffer = ? NO-ERROR . /* also reset possible error state */
                        END.

                        ASSIGN hBuffer = ? .
                    END.

                    /* Formatting of Integer Colums */
                    IF oColumn:DataType:Equals(oInt32Type) THEN DO:
                        IF NOT VALID-OBJECT(oColumn:CellAppearance) THEN
                            oColumn:CellAppearance = NEW Infragistics.Win.Appearance() .

                        oColumn:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right .
                        oColumn:CellDisplayStyle = CellDisplayStyle:FormattedText.
                        oColumn:Style = ColumnStyle:Integer.
                    END.

                    /* Formatting of Decimal Columns */
                    ELSE IF oColumn:DataType:Equals(oDecimalType) THEN DO:
                        IF NOT VALID-OBJECT(oColumn:CellAppearance) THEN
                            oColumn:CellAppearance = NEW Infragistics.Win.Appearance() .

                        oColumn:CellAppearance:TextHAlign = Infragistics.Win.HAlign:Right .
                        oColumn:CellDisplayStyle = CellDisplayStyle:FormattedText.
                        oColumn:Style = ColumnStyle:Double.
                    END.
                END.
            END.
        END.

        FINALLY:
            IF lTempSetSortProperties THEN
                ASSIGN lSetSortProperties = lTempSetSortProperties .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Formats the Grid Appearance
        Notes:   Sets the Color of the ActiveRow Appearance if not yet set.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormatGrid ():

        /* Mike Fechner, Consultingwerk Ltd. 24.07.2009
           Sorting should be done by the DataAdapter, not the Grid */
        THIS-OBJECT:DisplayLayout:Override:HeaderClickAction = HeaderClickAction:ExternalSortMulti .

        IF NOT VALID-OBJECT(THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance) THEN
            THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance = NEW Infragistics.Win.Appearance () .

        IF NOT VALID-OBJECT(THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance:BackColor) OR
            THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance:BackColor:IsEmpty THEN
            THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance:BackColor = System.Drawing.SystemColors:Highlight.

        IF NOT VALID-OBJECT(THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance:ForeColor) OR
            THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance:ForeColor:IsEmpty THEN
            THIS-OBJECT:DisplayLayout:Override:ActiveRowAppearance:ForeColor = System.Drawing.SystemColors:HighlightText.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Formats special columns supported by the SmartFramework functionality
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FormatSmartColumns ():

    &IF DEFINED (SmartFramework) NE 0 &THEN

        DEFINE VARIABLE oImageProvider AS Consultingwerk.Windows.Framework.IImageProvider NO-UNDO .

        ASSIGN oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .

        IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists ("SmartAttachments":U) THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartAttachments":U]:Hidden = FALSE .
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartAttachments":U]:Width = 20 .

            THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartAttachments":U]:ValueList = Consultingwerk.Windows.Framework.ValueLists:GetAttachmentValueList() .

            IF VALID-OBJECT (oImageProvider) THEN
                THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartAttachments":U]:Header:Appearance:Image = oImageProvider:ImageForKey
                                                            ("Consultingwerk/Windows/Framework/Images/SmartFramework/paperclip_16.png":U) .
        END.

        IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists ("SmartComments":U) THEN DO:
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartComments":U]:Hidden = FALSE .
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartComments":U]:Width = 20 .
            THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartComments":U]:ValueList = Consultingwerk.Windows.Framework.ValueLists:GetCommentsValueList() .

            IF VALID-OBJECT (oImageProvider) THEN
                THIS-OBJECT:DisplayLayout:Bands[0]:Columns["SmartComments":U]:Header:Appearance:Image = oImageProvider:ImageForKey
                                                            ("Consultingwerk/Windows/Framework/Images/SmartFramework/message_16.png":U) .
        END.

    &ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a string with the current Grid settings
        Notes:
        @return The string representing the current grid settings
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetCurrentSettings ():

        DEFINE VARIABLE oSettings AS UltraGridSettings NO-UNDO .

        ASSIGN oSettings = NEW UltraGridSettings (THIS-OBJECT) .

        RETURN oSettings:StoreColumnSettings() .

        FINALLY:
            DELETE OBJECT oSettings .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a ProDataset with a copy of the selected rows
        Notes:   Useful for routines that need to process the selected rows
                 Only supported when the SmartDataSource is a SmartDatasetAdapter
                 extended type, e.g. SmartBusinessEntityAdapter, SmartDatasetChildAdapter
        @param phDataset OUTPUT DATASET-HANDLE The Dataset containing the duplicates of the rows
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetSelectedRowsDataset (OUTPUT DATASET-HANDLE phDataset):

        DEFINE VARIABLE hSourceDataset AS HANDLE    NO-UNDO .
        DEFINE VARIABLE cTables        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE oDataSource    AS SmartDatasetAdapter NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:TypeOf (THIS-OBJECT:SmartDataSource,
                                                      "Consultingwerk.SmartComponents.Base.SmartDatasetAdapter":U,
                                                      "SmartDataSource":U) .

        ASSIGN oDataSource    = CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Base.SmartDatasetAdapter)
               hSourceDataset = oDataSource:Dataset
               cTables        = QueryHelper:QueryBufferNames (oDataSource:QueryHandle) .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i hSourceDataset Consultingwerk.WidgetTypeEnum:Dataset} .

        CREATE DATASET phDataset .
        phDataset:CREATE-LIKE (hSourceDataset) .

        UltraGridSelectedRowsHelper:CopySelectedRowsToProDataset (THIS-OBJECT,
                                                                  cTables,
                                                                  hSourceDataset,
                                                                  phDataset) .

        FINALLY:
            IF VALID-HANDLE (phDataset) THEN
                DELETE OBJECT phDataset .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Hides the non visible Columns from the Grid
        Notes:   Following the VisibleGridColumns Property
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID HideNonVisibleColumns ():

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        IF THIS-OBJECT:VisibleGridColumns > "":U THEN
            .
        ELSE
            RETURN .

        {Consultingwerk/foreach.i UltraGridColumn oColumn in THIS-OBJECT:DisplayLayout:Bands[0]:Columns}
            oColumn:Hidden = NOT ListHelper:EntryIsInList (oColumn:Key, THIS-OBJECT:VisibleGridColumns) .

            IF NOT ListHelper:EntryIsInList (oColumn:Key, THIS-OBJECT:VisibleGridColumns) THEN
                oColumn:ExcludeFromColumnChooser = ExcludeFromColumnChooser:True .
        END.

        {Consultingwerk/foreach.i UltraGridBand oBand in THIS-OBJECT:DisplayLayout:Bands '' bandLoop}

            IF oBand:Index = 0 THEN
                NEXT bandLoop .

            {Consultingwerk/foreach.i UltraGridColumn oColumn in oBand:Columns nodefine}

                oColumn:Hidden = NOT ListHelper:EntryIsInList (oBand:Key + ".":U + oColumn:Key, THIS-OBJECT:VisibleGridColumns) .

                IF NOT ListHelper:EntryIsInList (oBand:Key + ".":U + oColumn:Key, THIS-OBJECT:VisibleGridColumns) THEN
                    oColumn:ExcludeFromColumnChooser = ExcludeFromColumnChooser:True .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Column Filer Operator DropDown Items
        Notes:   Invoked from the EnableSmartFilterUserInterface method (InitializaLayoutHandler)
                 when the SmartFilterUseOperators property is TRUE
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeColumnFilterOperatorItems():

        UltraGridFilterRowHelper:InitializeSupportedFilterOperators (THIS-OBJECT) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handle default behaviour of the Grid and apply a default layout.
                 This Method is called if the InitializeLayout Event of the UltraGrid
                 is fired.
        Notes:
        @param sender Indicates which Object has raised the Event
        @param e The InitializeLayoutEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeLayoutHandler (sender AS System.Object,
                                                   e AS InitializeLayoutEventArgs):

        DEFINE VARIABLE oCustomizer AS ISmartDataBrowserColumnCustomizer NO-UNDO .

        THIS-OBJECT:DisplayLayout:Override:CellClickAction = CellClickAction:RowSelect.

        THIS-OBJECT:HideNonVisibleColumns () .

        THIS-OBJECT:SetGridReadOnly ().

        THIS-OBJECT:DisableDelKeyUsage ().

        THIS-OBJECT:FormatColumns () .

        THIS-OBJECT:FormatGrid () .

        THIS-OBJECT:FormatColumnFilters () .

        &IF DEFINED (SmartFramework) NE 0 &THEN
        THIS-OBJECT:FormatSmartColumns () .
        &ENDIF

        IF lFirstTime THEN DO:
           IF NOT THIS-OBJECT:DesignTime THEN
               THIS-OBJECT:RestoreSettings () .

            /* Place this after the ASSIGN THIS-OBJECT:Initializing = FALSE in SUPER */
            IF THIS-OBJECT:SmartFilterActive THEN
                THIS-OBJECT:EnableSmartFilterUserInterface () .

            IF NOT THIS-OBJECT:DesignTime THEN DO:
                oCustomizer = {Consultingwerk/get-service.i Consultingwerk.SmartComponents.Implementation.ISmartDataBrowserColumnCustomizer} .

                IF VALID-OBJECT (oCustomizer) THEN DO:
                    {Consultingwerk/foreach.i Infragistics.Win.UltraWinGrid.UltraGridBand oBand in THIS-OBJECT:DisplayLayout:Bands}
                        {Consultingwerk/foreach.i Infragistics.Win.UltraWinGrid.UltraGridColumn oColumn in oBand:Columns}

                            oCustomizer:AssignColumnProperties (THIS-OBJECT, oColumn) .
                        END.
                    END.
                END.
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err).
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads a browsers column settings from the Registry
        Notes:
        @return The settings for this Grid instance
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER LoadSettings ():

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        RETURN oSettingsService:GetSetting (THIS-OBJECT:SettingsKey,
                                            "GridSettings":U) .

        @SuppressUnusedWarnings.
        CATCH err AS System.Exception:
            /* ignore */
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the OnAfterRowRegionScroll event
        Notes:   Occurs after a row scrolling region is scrolled.
        @param e Infragistics.Win.UltraWinGrid.RowScrollRegionEventArgs
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnAfterRowRegionScroll (e AS RowScrollRegionEventArgs):

        SUPER:OnAfterRowRegionScroll (e) .

        IF NOT lScrolling THEN DO ON ERROR UNDO, THROW:
            ASSIGN lScrolling = TRUE .

            THIS-OBJECT:ScrollOffHome () .

            FINALLY:
                ASSIGN lScrolling = FALSE .
            END FINALLY.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the BeforeSortChange event
        Notes:
        @param e The BeforeSortChangeEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnBeforeSortChange (e AS BeforeSortChangeEventArgs):

        DEFINE VARIABLE cQuerySort       AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER   NO-UNDO .
        DEFINE VARIABLE lGroupBy         AS LOGICAL   NO-UNDO INIT FALSE .

        ASSIGN lInOnBeforeSortChange = TRUE .

        SUPER:OnBeforeSortChange(e).

        /* Mike Fechner, Consultingwerk Ltd. 01.10.2013
           Bug 2836: Setting sort indicator from SmartDataAdapter:SetQuerySort */
        IF lUpdateSortIndicator = TRUE THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 29.01.2010
           Don't try to sort DataAdapter's Query in VisualDesigner */
        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        IF e:Cancel = TRUE THEN
            RETURN .

        IF e:SortedColumns:Count = 0 THEN
            RETURN .

        IF VALID-OBJECT (THIS-OBJECT:BindingSource) AND VALID-HANDLE (THIS-OBJECT:BindingSource:Handle) AND
           VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND TYPE-OF (THIS-OBJECT:SmartDataSource, ISortableDataSource) THEN .
        ELSE RETURN.

        DO i = 1 TO e:SortedColumns:Count:

            IF e:SortedColumns[i - 1]:Band:Index > 0 THEN
                NEXT .

            IF Progress.Util.EnumHelper:AreEqual(e:SortedColumns[i - 1]:SortIndicator,
                                                 SortIndicator:Ascending) OR
               Progress.Util.EnumHelper:AreEqual(e:SortedColumns[i - 1]:SortIndicator,
                                                 SortIndicator:Descending) THEN

            /* Mike Fechner, Consultingwerk Ltd. 29.04.2012
               Will work with and without UseFullFieldNames (10.2B05+) */
            cQuerySort = cQuerySort + "BY ":U +
                         BindingSourceHelper:BindingBufferFieldName (THIS-OBJECT:BindingSource,
                                                                     e:SortedColumns[i - 1]:Key) +
                         " ":U .

            IF Progress.Util.EnumHelper:AreEqual(e:SortedColumns[i - 1]:SortIndicator,
                                                 SortIndicator:Descending) THEN
                cQuerySort = cQuerySort + "DESCENDING ":U .
        END.

        CAST (THIS-OBJECT:SmartDataSource, ISortableDataSource):QuerySort = TRIM(cQuerySort) .

        /* Mike Fechner, Consultingwerk Ltd. 15.10.2015
           Optionally ensure, that the SmartBusinessEntityAdapter contains
           all rows */
        IF THIS-OBJECT:FetchAllRecordsOnGroupBy THEN DO:
            {Consultingwerk/foreach.i UltraGridColumn oColumn in e:SortedColumns}
                IF oColumn:IsGroupByColumn THEN DO:
                    ASSIGN lGroupBy = TRUE .
                    LEAVE .
                END.
            END.

            IF lGroupBy AND TYPE-OF (THIS-OBJECT:SmartDataSource,
                                     ISupportsFetchAll) THEN DO:
                IF CAST (THIS-OBJECT:SmartDataSource,
                         ISupportsFetchAll):FetchAll () THEN

                    THIS-OBJECT:DisplayLayout:Bands[0]:SortedColumns:RefreshSort (TRUE) .
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessageBox (err) .

            e:Cancel = TRUE.
        END CATCH.

        FINALLY:
            ASSIGN lInOnBeforeSortChange = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the DefaultAction event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnDefaultAction (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            ASSIGN e = System.EventArgs:Empty .

        THIS-OBJECT:DefaultAction:Publish (THIS-OBJECT, e) .

        /* IDefaultActionSubscriber call-back, depreciated since 10.2B and the native event
           support */
        FOR EACH ttDefaultActionSubscriber WHERE ttDefaultActionSubscriber.RecordOwner = THIS-OBJECT NO-LOCK ON ERROR UNDO, THROW:
            CAST(ttDefaultActionSubscriber.DefaultActionSubscriber, IDefaultActionSubscriber):SmartDataBrowserDefaultActionHandler(THIS-OBJECT, e).
        END.

        /* Mike Fechner, Consultingwerk Ltd. 25.02.2014
           Support to start update in a linked SmartViewerControl */
        IF THIS-OBJECT:EditInViewerOnDefaultAction THEN
            THIS-OBJECT:StartEditModeInViewer () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the DoubleClickRow event
        Notes:
        @param e The DoubleClickRowEventArgs with the Data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnDoubleClickRow (e AS DoubleClickRowEventArgs):

        IF NOT e:Row:IsFilterRow THEN
            OnDefaultAction (System.EventArgs:Empty) .

        SUPER:OnDoubleClickRow (e) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked at the end of a Containers InitializeComponents method
        Notes:   Enforced by System.ComponentModel.ISupportInitialize
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.endinit(VS.80).aspx
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnEndInit ():

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        SUPER:OnEndInit().

        /* Prevent multiple execuitions on OnEndInit when Visual Validation is used */
        IF NOT lInited THEN
            ASSIGN lInited = TRUE .
        ELSE
            RETURN .

        IF NOT THIS-OBJECT:DesignTime AND lFormClosedSubscribed = FALSE THEN DO:
            oForm = THIS-OBJECT:FindForm() .

            IF VALID-OBJECT (oForm) THEN DO:
                oForm:FormClosed:Subscribe (ParentFormClosedHandler) .

                ASSIGN lFormClosedSubscribed = TRUE .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the GotFocus event.
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnGotFocus (e AS System.EventArgs):

        SUPER:OnGotFocus (e) .

        IF NOT THIS-OBJECT:ParentFormDestroyed THEN DO:

            &IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
            SmartDataBrowser:OnSmartDataBrowserActivated (THIS-OBJECT, e) .
            &ENDIF
            SmartDataBrowserSettings:OnSmartDataBrowserActivated (THIS-OBJECT, e) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the KeyDown Event
        Notes:   Used to trap Cursor-Up and Page-Up to implement backwards-batching
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnKeyDown (e AS System.Windows.Forms.KeyEventArgs):

        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Return) OR
           Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Enter) THEN DO:

           IF THIS-OBJECT:SmartFilterActive AND
              VALID-OBJECT (THIS-OBJECT:ActiveCell) AND
              THIS-OBJECT:ActiveCell:IsFilterRowCell THEN DO:

                IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
                   TYPE-OF (THIS-OBJECT:SmartDataSource, ISmartFilterTarget) THEN

                    THIS-OBJECT:PerformFilterRow () .

                e:Handled = TRUE .

                RETURN .
            END.
            ELSE
                OnDefaultAction (System.EventArgs:Empty) .
        END.

        SUPER:OnKeyDown (e) .

        /* Mike Fechner, Consultingwerk Ltd. 22.03.2014
           Bug 2885/SCL-212 Check validity of BindingSource */
        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           TYPE-OF (THIS-OBJECT:SmartDataSource, ISupportsScrollup) AND
           VALID-OBJECT (THIS-OBJECT:BindingSource) THEN

            IF THIS-OBJECT:BindingSource:Position = 0 AND
               (Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:Up) OR
                Progress.Util.EnumHelper:AreEqual (e:KeyCode, System.Windows.Forms.Keys:PageUp)) THEN

                THIS-OBJECT:ScrollOffHome () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the CreateControl event
        Notes:   The OnCreateControl method is called when the control is first
                 created
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnCreateControl ():

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        SUPER:OnCreateControl () .

        IF THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:CreateVerbs() .

        ELSE IF NOT lFormClosedSubscribed THEN DO:
            oForm = THIS-OBJECT:FindForm () .

            IF VALID-OBJECT (oForm) THEN DO:
                oForm:FormClosed:Subscribe (ParentFormClosedHandler) .

                ASSIGN lFormClosedSubscribed = TRUE .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseUp event
        Notes:   Handles the pressing of the Grid's FilterClearButtonUIElement to handle
                 the clearing of the SmartFilter Row
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseUp (e AS System.Windows.Forms.MouseEventArgs):

        DEFINE VARIABLE oUIElement AS Infragistics.Win.UIElement NO-UNDO .

        IF THIS-OBJECT:SmartFilterActive = TRUE THEN DO:
            oUIElement = THIS-OBJECT:DisplayLayout:UIElement:ElementFromPoint (e:Location) .

            DO WHILE VALID-OBJECT (oUIElement):

                IF TYPE-OF (oUIElement, FilterClearButtonUIElement) THEN DO:

                    THIS-OBJECT:ClearFilterRow (CAST (oUIElement, FilterClearButtonUIElement)) .

                    RETURN .
                END.

                oUIElement = oUIElement:Parent .
            END.
        END.

        SUPER:OnMouseUp (e) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MouseWheel Event
        Notes:   Used implement backwards-batching
        @param e System.Windows.Forms.MouseEventArgs
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnMouseWheel (e AS System.Windows.Forms.MouseEventArgs):

        SUPER:OnMouseWheel(e) .

        THIS-OBJECT:ScrollOffHome () .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    &IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Raises the SmartDataBrowserActivated event
        Notes:   Static event raised whenever a SmartDataBrowser is activated. This
                 event is useful to control a central UltraGridColumnChooser etc.
                 This method may be removed using the NoStaticsInHybrids setting
                 in Consultingwerk/products.i. This allows to avoid static members
                 in hybrid classes SmartDataBrowser and SmartBusinessEntityAdapter
                 to avoid issues with the combination of static and hybrids on 11.2
                 and 11.3, OE defect OE00240707 http://knowledgebase.progress.com/articles/Article/000042229?popup=true
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID OnSmartDataBrowserActivated (sender AS System.Object,
                                                              e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        SmartDataBrowser:SmartDataBrowserActivated:Publish (sender, e) .

    END METHOD.
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Raises the SmartFilterActiveChanged
        Notes:
        @param e The System.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnSmartFilterActiveChanged (e AS System.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = System.EventArgs:Empty .

        THIS-OBJECT:SmartFilterActiveChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The name (Label) of the Designer Verb that was invoked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE .

        CASE pcVerbText:
            WHEN "Store Visible Grid Columns":U THEN
                StoreVisibleGridColumns() .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormClosed event of the Parent Form
        Notes:
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParentFormClosedHandler (sender AS System.Object,
                                                   e AS System.Windows.Forms.FormClosedEventArgs):

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Save Grid Column settings, when enabled for the session and the SettingsKey is set
           and different from the GUID created in the constrcutor. */
        IF NOT THIS-OBJECT:DesignTime
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
        AND SmartDataBrowser:SaveColumnSettings
&ELSE
        AND SmartDataBrowserSettings:SaveColumnSettings
&ENDIF
           AND THIS-OBJECT:SettingsKey > "":U AND
           cConstructorsSettingsKey <> THIS-OBJECT:SettingsKey THEN
            SaveSettings () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the Filter Row
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PerformFilterRow ():

        CAST (THIS-OBJECT:SmartDataSource, ISmartFilterTarget):RetrieveData () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the actual browser column settings
        Notes:   SUBSTITUTE ("&1,&2,&3,&4":U,
                             THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Key,
                             THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Width,
                             THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Header:VisiblePosition,
                             THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Hidden) .
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RestoreSettings ():

        DEFINE VARIABLE cSettings AS CHARACTER         NO-UNDO .

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Save Grid Column settings, when enabled for the session and the SettingsKey is set
           and different from the GUID created in the constrcutor. */
        IF
&IF DEFINED (NoStaticsInHybrids) EQ 0 &THEN
        SmartDataBrowser:SaveColumnSettings
&ELSE
        SmartDataBrowserSettings:SaveColumnSettings
&ENDIF
          AND
           THIS-OBJECT:SettingsKey > "":U AND
           THIS-OBJECT:SettingsKey <> cConstructorsSettingsKey THEN .
        ELSE RETURN .

        IF NOT VALID-OBJECT (THIS-OBJECT:DataSource) THEN
            RETURN .

        IF NOT VALID-OBJECT (CAST (THIS-OBJECT:DataSource, Progress.Data.BindingSource):TableSchema) THEN
          RETURN .

        ASSIGN cSettings = LoadSettings () .

        THIS-OBJECT:ApplyGridSettings (cSettings) .

        lFirstTime = FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Resume row syncronization
        Notes:   SuspendRowSynchronization and ResumeRowSynchronization methods can
                 be used to temporarily suspend UltraGrid from responding to data
                 source change notifications. When row syncrhonization is suspended,
                 the UltraGrid will still mark the rows dirty so it will re-create
                 the rows next time it gets painted.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ResumeRowSynchronizationAfterReposition ():

        THIS-OBJECT:ResumeRowSynchronization() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores a browsers column settings to the Registry
        Notes:   This methods builds the settings string of the Grid that will
                 actually be stored using the StoreSettings method (so this
                 can be overloaded independently).
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SaveSettings ():

        DEFINE VARIABLE cSettings AS CHARACTER NO-UNDO.

        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        ASSIGN cSettings = THIS-OBJECT:GetCurrentSettings() .

        THIS-OBJECT:StoreSettings (cSettings) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Fetches the previous batch from the DataSource
        Notes:   Uses the OffHome method of the Adapter. The Adapter needs to implement
                 ISupportsScrollup
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ScrollOffHome ():

        DEFINE VARIABLE oAdapter      AS ISupportsScrollup NO-UNDO .
        DEFINE VARIABLE iRecordsAdded AS INTEGER           NO-UNDO .
        DEFINE VARIABLE iPrevious     AS INTEGER           NO-UNDO .

        DEFINE VARIABLE iOffset       AS INTEGER           NO-UNDO INIT 0 .

        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN
            RETURN .

        IF NOT TYPE-OF (THIS-OBJECT:SmartDataSource,
                        ISupportsScrollup) THEN RETURN .

        ASSIGN oAdapter = CAST (THIS-OBJECT:SmartDataSource,
                                ISupportsScrollup) .

        IF THIS-OBJECT:SmartFilterActive THEN iOffset = 1 .

        IF THIS-OBJECT:ActiveRowScrollRegion:ScrollPosition = iOffset AND
            oAdapter:RecordPosition <> RecordPositionEnum:FirstRecord AND
            oAdapter:PreviousBatchAvailable ()

            THEN DO:
                /* Mike Fechner, Consultingwerk Ltd. 23.12.2011
                   Lock the redrawing of the UltraGrid while performing the batching */
                Consultingwerk.Windows.API.Win32:LockWindowUpdate (THIS-OBJECT:Handle:ToInt32()) .

                ASSIGN iPrevious = THIS-OBJECT:BindingSource:Count .

                oAdapter:OffHome() .

                ASSIGN iRecordsAdded = THIS-OBJECT:BindingSource:Count - iPrevious .

                PROCESS EVENTS .

                THIS-OBJECT:ScrollToCurrentRow (TRUE) .

                IF iRecordsAdded > 0 THEN
                    THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:ScrollPosition = iRecordsAdded - 1 .

                /* Mike Fechner, Consultingwerk Ltd. 23.12.2011
                   Release the Lock of the redrawing of the UltraGrid after performing the batching */
                Consultingwerk.Windows.API.Win32:LockWindowUpdate (0) .
            END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Scrolls to the BindingSource's position
        Notes:   Enforces reposition/scolling also to rows already visible (previous default)
        @param plAllRegions Logical value indicating if all RowScrollRegions will be positioned to the current row of just the first one.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToCurrentRow (plAllRegions AS LOGICAL):

        THIS-OBJECT:ScrollToCurrentRow (plAllRegions, TRUE) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Scrolls to the BindingSource's position
        Notes:
        @param plAllRegions Logical value indicating if all RowScrollRegions will be positioned to the current row of just the first one.
        @param plEnforeScroll Logical set to false to indicate that we should only scroll when the row would otherwise not be visible, FALSE is the ABL Browse widget's SET-REPOSITIONED-ROW CONDITIONAL behaviour
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToCurrentRow (plAllRegions AS LOGICAL,
                                           plEnforeScroll AS LOGICAL):

        DEFINE VARIABLE oBindingSource AS Progress.Data.BindingSource NO-UNDO .
        DEFINE VARIABLE iPosition      AS INTEGER                     NO-UNDO .
        DEFINE VARIABLE i              AS INTEGER                     NO-UNDO .

        THIS-OBJECT:BeginUpdate () .

        ASSIGN lScrolling = TRUE .

        oBindingSource = THIS-OBJECT:BindingSource .

        IF NOT VALID-OBJECT (oBindingSource) THEN
            UNDO, THROW NEW Progress.Lang.AppError ("Unable to ScrollToCurrentRow without BindingSource") .

        THIS-OBJECT:Selected:Rows:Clear() .

        IF THIS-OBJECT:Rows:Count = 0 THEN
            RETURN .

        /* Mike Fechner, Consultingwerk Ltd. 11.05.2015
           SCL-806: When group-by is used, the Rows[Position] may fail, because it
           would refer to the summary rows only */
        THIS-OBJECT:Rows:GetRowWithListIndex (oBindingSource:Position, TRUE):Activate() .

        /* Visible Index = -1 when a row is not visible */
        IF NOT plEnforeScroll AND THIS-OBJECT:Rows:GetRowWithListIndex (oBindingSource:Position, TRUE):VisibleIndex >= THIS-OBJECT:RepositionedRow THEN
            RETURN .

        ASSIGN iPosition = THIS-OBJECT:BindingSource:Position .

        IF Progress.Util.EnumHelper:AreEqual (THIS-OBJECT:DisplayLayout:Override:AllowRowFiltering,
                                              DefaultableBoolean:True) AND
           Progress.Util.EnumHelper:AreEqual (THIS-OBJECT:DisplayLayout:Override:FilterUIType,
                                              FilterUIType:FilterRow) THEN
            ASSIGN iPosition = iPosition + 1.

        IF THIS-OBJECT:RepositionedRow > 0 THEN
            ASSIGN iPosition = MAX (0, iPosition - THIS-OBJECT:RepositionedRow) .

        IF iPosition >= 0 THEN DO:
            IF plAllRegions = FALSE THEN
                THIS-OBJECT:DisplayLayout:RowScrollRegions[0]:ScrollPosition = iPosition .
            ELSE DO:
                DO i = 0 TO THIS-OBJECT:DisplayLayout:RowScrollRegions:Count - 1:
                    THIS-OBJECT:DisplayLayout:RowScrollRegions[i]:ScrollPosition = iPosition .
                END.
            END.
        END.

        FINALLY:
            ASSIGN lScrolling = FALSE .

            THIS-OBJECT:EndUpdate () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Scrolls the Grid to the First Row
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToFirstRow ():

        THIS-OBJECT:BeginUpdate () .

        THIS-OBJECT:PerformAction (UltraGridAction:FirstRowInGrid) .

        IF EnumHelper:AreEqual (THIS-OBJECT:DisplayLayout:Override:AllowRowFiltering, DefaultableBoolean:True) AND
           EnumHelper:AreEqual (THIS-OBJECT:DisplayLayout:Override:FilterUIType, FilterUIType:FilterRow) THEN

            THIS-OBJECT:PerformAction (UltraGridAction:NextRow) .

        THIS-OBJECT:EndUpdate () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Scrolls the Grid to the Last Row
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ScrollToLastRow ():

        THIS-OBJECT:BeginUpdate () .

        THIS-OBJECT:PerformAction (UltraGridAction:LastRowInGrid) .

        THIS-OBJECT:EndUpdate () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set properties for ICustomTypeDescriptor
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        THIS-OBJECT:DesignerVerbs  = "Store Visible Grid Columns":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Set the entire Grid to Read-Only
        Notes:   Invoked from the method InitializeLayoutHandler
                 All Bands and Columns are processed and set into read only mode
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetGridReadOnly ():

        DEFINE VARIABLE j     AS INTEGER         NO-UNDO.

        THIS-OBJECT:DisplayLayout:Override:AllowAddNew = AllowAddNew:No.
        THIS-OBJECT:DisplayLayout:Override:AllowDelete = DefaultableBoolean:False.
        THIS-OBJECT:DisplayLayout:Override:AllowUpdate = DefaultableBoolean:False.

        THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = SelectType:SingleAutoDrag .

        {Consultingwerk/foreach.i UltraGridBand oBand in THIS-OBJECT:DisplayLayout:Bands}

            IF Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeCell, SelectType:Default) THEN
                oBand:Override:SelectTypeCell = SelectType:None.
            IF Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeCol, SelectType:Default) THEN
                oBand:Override:SelectTypeCol  = SelectType:None.
            IF Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:Default) THEN
                oBand:Override:SelectTypeRow  = SelectType:SingleAutoDrag.
            IF Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:Single) THEN
                oBand:Override:SelectTypeRow  = SelectType:SingleAutoDrag.

            DO j = 1 TO oBand:Columns:Count:
                /* Mike Fechner, Consultingwerk Ltd. 01.06.2009
                   Using full name of class activation, to avoid issues with table
                   name PM_ActivationDetails at Radius */
                oBand:Columns[j - 1]:CellActivation = Activation:NoEdit .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Register a new DefaultActionSubscriber (Class that implements
                 IDefaultActionSubscriber).
        Notes:
        @param poDefaultActionSubscriber The IDefaultActionSubscriber instance to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterDefaultActionSubscriber (poDefaultActionSubscriber AS IDefaultActionSubscriber):
        CREATE ttDefaultActionSubscriber.

        ASSIGN
            ttDefaultActionSubscriber.RecordOwner             = THIS-OBJECT
            ttDefaultActionSubscriber.DefaultActionSubscriber = poDefaultActionSubscriber
            .

        RELEASE ttDefaultActionSubscriber.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CollectFilterValues event of the ISmartFilterTarget
        Notes:   Attaches the Grids filter values to the Query Expression when the Grid's
                 SmartFilterActive attribute is set to TRUE
        @param sender The object that raised the CollectFilterValues event
        @param e The CollectFilterValuesEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID CollectFilterValuesHandler (sender AS Progress.Lang.Object,
                                                               e AS CollectFilterValuesEventArgs):

        IF VALID-OBJECT (THIS-OBJECT:SmartDataSource) AND
           TYPE-OF (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter) AND
            (NOT VALID-OBJECT (THIS-OBJECT:DataSource) OR NOT VALID-HANDLE (CAST (THIS-OBJECT:DataSource, Progress.Data.BindingSource):Handle)) THEN

            CAST (THIS-OBJECT:SmartDataSource, SmartDatasetAdapter):InitializeAdapter () .

        IF THIS-OBJECT:SmartFilterActive THEN
            UltraGridFilterRowHelper:BuildQueryExpression (THIS-OBJECT,
                                                           e:QueryExpressions,
                                                           THIS-OBJECT:SmartFilterUseMatches,
                                                           THIS-OBJECT:SmartFilterUseOperators) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the SmartDataBrowser class
        Notes:   Deletes the associated ttDefaultActionSubscriber records
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDataBrowser ():

        FOR EACH ttDefaultActionSubscriber WHERE ttDefaultActionSubscriber.RecordOwner = THIS-OBJECT ON ERROR UNDO, THROW:
            DELETE ttDefaultActionSubscriber.
        END.

    END DESTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Enables or Disables multi-selection in the Grid
        Notes:
        @param plEnable Logical value indicating if multi-selection should be possible
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SetMultiSelection (plEnable AS LOGICAL):

        IF plEnable THEN
            THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = SelectType:Extended .
        ELSE
            THIS-OBJECT:DisplayLayout:Override:SelectTypeRow = SelectType:SingleAutoDrag .

        {Consultingwerk/foreach.i UltraGridBand oBand in THIS-OBJECT:DisplayLayout:Bands}

            IF plEnable THEN DO:
                IF Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:Single) OR
                   Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:SingleAutoDrag) OR
                   Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:Default) OR
                   Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:None)
                   THEN
                    oBand:Override:SelectTypeRow  = SelectType:Extended.
            END.
            ELSE
                IF Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:Extended) OR
                   Progress.Util.EnumHelper:AreEqual(oBand:Override:SelectTypeRow, SelectType:ExtendedAutoDrag) THEN
                    oBand:Override:SelectTypeRow  = SelectType:SingleAutoDrag .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes EnableFields (Update) in a Viewer linked to the same
                 SmartDataAdapter
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StartEditModeInViewer ():

        DEFINE VARIABLE oAdapter AS Consultingwerk.SmartComponents.Base.SmartDataAdapter          NO-UNDO .
        DEFINE VARIABLE oViewer  AS Consultingwerk.SmartComponents.Interfaces.ISmartViewerControl NO-UNDO .

        IF NOT TYPE-OF (THIS-OBJECT:SmartDataSource,
                        Consultingwerk.SmartComponents.Base.SmartDataAdapter) THEN RETURN .

        oAdapter = CAST (THIS-OBJECT:SmartDataSource,
                         Consultingwerk.SmartComponents.Base.SmartDataAdapter) .

        {Consultingwerk/foreachABL.i Consultingwerk.SmartComponents.Interfaces.ISmartDataTarget oTarget in oAdapter:SmartDataTargets}

            IF TYPE-OF (oTarget, Consultingwerk.SmartComponents.Interfaces.ISmartViewerControl) THEN DO:
                oViewer = CAST (oTarget, Consultingwerk.SmartComponents.Interfaces.ISmartViewerControl) .

                IF VALID-OBJECT (oViewer:SmartTableIOSource) THEN DO:
                    IF oViewer:SmartTableIOState = "FieldsDisabled":U THEN
                        oViewer:EnableFields ("Update":U) .

                    LEAVE .
                END.
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores a browsers column settings to the Registry
        Notes:   This methods stores the actual settings string of the Grid that have
                 been built by SaveSettings method (so this
                 can be overloaded independently).
        @param pcSettings The settings string to store
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreSettings (pcSettings AS CHARACTER):

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        oSettingsService:StoreSetting (THIS-OBJECT:SettingsKey,
                                       "GridSettings":U,
                                       pcSettings) .

        @SuppressUnusedWarnings.
        CATCH err AS System.Exception:
            /* ignore */
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the currently visible columns (of band 0) in the
                 VisibleGridColumns property
        Notes:   Only supported during DesignTime
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID StoreVisibleGridColumns ():

        DEFINE VARIABLE i               AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j               AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cVisibleColumns AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lOk             AS LOGICAL   NO-UNDO INIT TRUE .
        DEFINE VARIABLE cOldValue       AS CHARACTER NO-UNDO.

        IF NOT THIS-OBJECT:DesignTime THEN
            RETURN .

        DO i = 0 TO THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count - 1:
            IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Hidden = FALSE THEN
                ASSIGN cVisibleColumns = cVisibleColumns + THIS-OBJECT:DisplayLayout:Bands[0]:Columns[i]:Key + ",":U.
        END.

        DO j = 1 TO THIS-OBJECT:DisplayLayout:Bands:Count - 1:
            DO i = 0 TO THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Count - 1:
                IF THIS-OBJECT:DisplayLayout:Bands[j]:Columns[i]:Hidden = FALSE THEN
                    ASSIGN cVisibleColumns = cVisibleColumns + THIS-OBJECT:DisplayLayout:Bands[j]:Key + ".":U +
                                                               THIS-OBJECT:DisplayLayout:Bands[j]:Columns[i]:Key + ",":U.
            END.
        END.

        ASSIGN cVisibleColumns = TRIM (cVisibleColumns, ",":U).

        IF cVisibleColumns = THIS-OBJECT:VisibleGridColumns THEN DO:
            MESSAGE "VisibleGridColumnns property is up to date! Nothing will be changed." SKIP (1)
                    cVisibleColumns
                VIEW-AS ALERT-BOX WARNING .

            RETURN .
        END.

        MESSAGE "Modify VisibleGridColumnns property? This property will have effect only at runtime." SKIP (1)
                cVisibleColumns
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "SmartDataBrowser" UPDATE lOk .

        IF lOk THEN DO:
            ASSIGN cOldValue = THIS-OBJECT:VisibleGridColumns .

            Consultingwerk.Util.DesignerHelper:RaiseComponentChanged (THIS-OBJECT,
                                                                      "VisibleGridColumns":U,
                                                                      BOX (cOldValue),
                                                                      BOX (cVisibleColumns)) .

            THIS-OBJECT:VisibleGridColumns = cVisibleColumns .
        END .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Suspends row syncronization
        Notes:   SuspendRowSynchronization and ResumeRowSynchronization methods can
                 be used to temporarily suspend UltraGrid from responding to data
                 source change notifications. When row syncrhonization is suspended,
                 the UltraGrid will still mark the rows dirty so it will re-create
                 the rows next time it gets painted.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SuspendRowSynchronizationForReposition ():

        THIS-OBJECT:SuspendRowSynchronization() .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the Sort Indicators of the SmartDataBrowser columns
        Notes:   Typically invoked from a SmartDataAdapter after setting the QuerySort
                 property. Does not call back into QuerySort
        @param pcQuerySort The QuerySort to visualize
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSortIndicator (pcQuerySort AS CHARACTER):

        DEFINE VARIABLE cColumn      AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cWord        AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE cNext        AS CHARACTER                      NO-UNDO .
        DEFINE VARIABLE i            AS INTEGER                        NO-UNDO INIT 1 .
        DEFINE VARIABLE oBufferField AS Consultingwerk.BufferFieldName NO-UNDO .

        /* Mike Fechner, Consultingwerk Ltd. 01.10.2013
           Avoid infinitive loop */
        IF lInOnBeforeSortChange THEN
            RETURN .

        ASSIGN lUpdateSortIndicator = TRUE .


        THIS-OBJECT:DisplayLayout:Bands[0]:SortedColumns:Clear () .

        DO WHILE i <= NUM-ENTRIES (pcQuerySort, " ":U) ON ERROR UNDO, THROW:
            ASSIGN cWord = ENTRY (i, pcQuerySort, " ":U) .

            IF cWord = "":U THEN
                NEXT .

            IF cWord = "BY":U THEN
                NEXT .

            IF cWord = "DESCENDING":U THEN
                NEXT .

            IF i < NUM-ENTRIES (pcQuerySort, " ":U) THEN
                cNext = ENTRY (i + 1, pcQuerySort, " ":U) .
            ELSE
                cNext = "":U .

            /* Mike Fechner, Consultingwerk Ltd. 01.12.2013
               Before attaching the BindingSource to a Query we can
               only "guess" the binding source column for the sort field */
            IF NOT VALID-OBJECT (THIS-OBJECT:BindingSource) OR
               NOT VALID-OBJECT (THIS-OBJECT:BindingSource:TableSchema) OR
               NOT VALID-OBJECT (THIS-OBJECT:BindingSource:TableSchema:Columns) OR
               NOT VALID-HANDLE (THIS-OBJECT:BindingSource:Handle) THEN DO:

                ASSIGN oBufferField = BufferHelper:ParseFieldName (cWord) .

                IF oBufferField:TableName > "":U THEN DO:
                    IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (SUBSTITUTE ("&1:&2":U,
                                                                          oBufferField:TableName,
                                                                          oBufferField:FieldName)) THEN
                        ASSIGN cColumn = SUBSTITUTE ("&1:&2":U,
                                                     oBufferField:TableName,
                                                     oBufferField:FieldName) .
                    ELSE
                        ASSIGN cColumn = oBufferField:FieldName .
                END.
                ELSE
                    ASSIGN cColumn = cWord .

            END .
            ELSE
                ASSIGN cColumn = BindingSourceHelper:BindingColumnName (CAST (THIS-OBJECT:DataSource,
                                                                              Progress.Data.BindingSource),
                                                                        cWord) .

            IF THIS-OBJECT:DisplayLayout:Bands[0]:Columns:Exists (cColumn) THEN DO:
                IF cNext = "DESCENDING":U THEN DO:
                    THIS-OBJECT:DisplayLayout:Bands[0]:Columns[cColumn]:SortIndicator = SortIndicator:Descending .
                    ASSIGN i = i + 1 .
                END.
                ELSE
                    THIS-OBJECT:DisplayLayout:Bands[0]:Columns[cColumn]:SortIndicator = SortIndicator:Ascending .
            END .


            FINALLY:
                i = i + 1 .
            END FINALLY.
        END.

        FINALLY:
            ASSIGN lUpdateSortIndicator = FALSE .
        END FINALLY.

    END METHOD .

END CLASS.
