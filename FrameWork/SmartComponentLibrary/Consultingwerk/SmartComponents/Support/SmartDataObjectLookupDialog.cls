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
    File        : SmartBusinessEntityLookupDialog
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 24 23:01:03 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .
USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartDataObjectLookupDialog
    INHERITS SmartDataObjectLookupDialogDesigner
    IMPLEMENTS IDefaultActionSubscriber :

    DEFINE PROTECTED VARIABLE oAdapter AS SmartDataObjectAdapter NO-UNDO.

    DEFINE PRIVATE VARIABLE cLookupKeyFieldValue AS CHARACTER NO-UNDO.

    DEFINE PRIVATE VARIABLE cLookupColumnWidth   AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE cLookupRegistryKey   AS CHARACTER NO-UNDO.

    DEFINE PRIVATE VARIABLE cPreviousColumn      AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE cPreviousOperator    AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Name of the Type of SmartDataObjectAdapter used to lookup
                 values from the backend.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AdapterType AS CHARACTER NO-UNDO
    INIT "Consultingwerk.SmartComponents.Implementation.SmartDataObjectAdapter":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Lookup Browser
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowser AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO
    GET:
        RETURN smartDataBrowser1 .
    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Fields to be shown in the SmartDataBrowser
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO
    GET.
    SET.

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
        Purpose: Field that is returned by the SmartLookup
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyField AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the file name of the SmartDataObject used by this
                 Lookup Dialog
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupSmartDataObjectName AS CHARACTER NO-UNDO
    GET:
        RETURN oAdapter:SmartDataObjectName.
    END GET.
    SET (arg AS CHARACTER):
        oAdapter:SmartDataObjectName = arg.
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor with event subscription
        Notes:
        @param poSmartLookup The reference to the SmartLookup instance for this LookupDialog
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartDataObjectLookupDialog (poSmartLookup AS SmartLookup):
        SUPER ().

        cLookupRegistryKey = poSmartLookup:GetLookupRegistryKey ().
        RestoreLookupSettings ().

        THIS-OBJECT:smartDataBrowser1:RegisterDefaultActionSubscriber (THIS-OBJECT).
        THIS-OBJECT:tbSearchField:EditorButtonClick:Subscribe (tbSearchField_EditorButtonClick).
        THIS-OBJECT:tbSearchField:KeyDown:Subscribe (tbSearchField_KeyDown) .

        THIS-OBJECT:Shown:Subscribe (InitializeDatabinding).

        THIS-OBJECT:Closed:Subscribe (StoreLookupSettings).

        /* Mike Fechner, Consultingwerk Ltd. 12.10.2010
           Looks have their own way of storing settings */
        THIS-OBJECT:smartDataBrowser1:SettingsKey = "":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Applys the filter to the query
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ApplyFilter ():

        DEFINE VARIABLE cFilter   AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cOperator AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iField    AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cWhereAnd AS CHARACTER NO-UNDO .

        iField = LOOKUP (UNBOX(cbFilterField:Value),
                         THIS-OBJECT:LookupDialogFilterFields) .

        IF cPreviousColumn > "":U THEN
            oAdapter:RemoveQuerySelection (cPreviousColumn, cPreviousOperator) .

        IF NUM-ENTRIES (THIS-OBJECT:LookupDialogFilterOperators) >= iField THEN
            ASSIGN cOperator = ENTRY (iField, THIS-OBJECT:LookupDialogFilterOperators) .
        ELSE
            ASSIGN cOperator = ">=" .

        oAdapter:AssignQuerySelection (UNBOX(cbFilterField:Value), tbSearchField:Text, cOperator) .

        ASSIGN cPreviousColumn   = UNBOX(cbFilterField:Value)
               cPreviousOperator = cOperator .

        oAdapter:RetrieveData () .

        THIS-OBJECT:button2:Enabled = oAdapter:RowObject:AVAILABLE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a Field Value from the SmartDataObject Adapter
        Notes:
        @param pcFieldName The name of the Field to return
        @return The value of the field
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetLookupFieldValue (pcFieldName AS CHARACTER):

        DEFINE VARIABLE hDataHandle AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hField      AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i           AS INTEGER NO-UNDO.

        ASSIGN hDataHandle = oAdapter:BindingSource:Handle .

        IF VALID-HANDLE(hDataHandle) THEN
            DO i = 1 TO hDataHandle:NUM-BUFFERS:
                ASSIGN hField = hDataHandle:GET-BUFFER-HANDLE(i):BUFFER-FIELD(pcFieldName)
                    NO-ERROR .

                IF VALID-HANDLE(hField) THEN
                    RETURN hField:BUFFER-VALUE .
            END.

        RETURN "".

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the DataAdapter used by the SmartDataObjectLookupDialog
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID InitializeDataAdapter ():

        IF THIS-OBJECT:AdapterType > "":U THEN
            oAdapter = DYNAMIC-NEW (THIS-OBJECT:AdapterType) () .
        ELSE
            oAdapter = NEW SmartDataObjectAdapter () .

        oAdapter:BatchSize = 50 .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Event handler for the Shown event of this dialog form
        Notes:   Initializes the Data Binding
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeDatabinding (sender AS System.Object,
                                               e      AS System.EventArgs):

        DEFINE VARIABLE i       AS INTEGER         NO-UNDO.
        DEFINE VARIABLE oColumn AS UltraGridColumn NO-UNDO.

        IF NOT VALID-OBJECT (oAdapter) THEN
            UNDO, THROW NEW AppError ("No DataAdapter has been initialized so far.", 0) .

        smartDataBrowser1:SmartDataSource = oAdapter.

        oAdapter:SmartNavigationSource = THIS-OBJECT:smartNavigationPanel1 .

        oAdapter:RetrieveData ().

        THIS-OBJECT:button2:Enabled = oAdapter:RowObject:AVAILABLE .

        BrowserInitializeLayout(?, ?).

        IF THIS-OBJECT:LookupDialogFilterFields > "":U THEN DO:
            DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:LookupDialogFilterFields):
                oColumn = smartDataBrowser1:DisplayLayout:Bands[0]:Columns[ENTRY(i, THIS-OBJECT:LookupDialogFilterFields)]
                    NO-ERROR .

                IF NOT VALID-OBJECT (oColumn) THEN
                    UNDO, THROW NEW AppError (SUBSTITUTE("Column &1 not valid in LookupDialogFilterFields.",
                                                         ENTRY(i, THIS-OBJECT:LookupDialogFilterFields)), 0) .

                cbFilterField:ValueList:ValueListItems:Add (ENTRY(i, THIS-OBJECT:LookupDialogFilterFields),
                                                            oColumn:Header:Caption) .

                IF i = 1 THEN
                    cbFilterField:Value = ENTRY(i, THIS-OBJECT:LookupDialogFilterFields) .
            END.
        END.
        ELSE DO:
            THIS-OBJECT:cbFilterField:Visible = FALSE .
            THIS-OBJECT:tbSearchField:Visible = FALSE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the InitializeLayout event of the Lookup Grid Control
        Notes:   Make sure that only the given list of LookupBrowserFields are visible.
                 If LookupBrowserFields is an empty string all columns will be shown
        @param sender The reference to the object that raised the event
        @param e The InitializeLayoutEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
     METHOD PRIVATE VOID BrowserInitializeLayout (sender AS System.Object,
                                                  e      AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs):

        DEFINE VARIABLE oBand AS UltraGridBand NO-UNDO.
        DEFINE VARIABLE oGrid AS UltraGrid     NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER       NO-UNDO.
        DEFINE VARIABLE j     AS INTEGER       NO-UNDO.

        DEFINE VARIABLE iColumnWidthIndex AS INTEGER NO-UNDO.

        oGrid = THIS-OBJECT:smartDataBrowser1.

        IF LookupBrowserFields > "":U THEN
            DO i = 1 TO oGrid:DisplayLayout:Bands:Count:
                oBand = oGrid:DisplayLayout:Bands[i - 1] .

                DO j = 1 TO oBand:Columns:Count:
                    iColumnWidthIndex = LOOKUP(oBand:COLUMNS[j - 1]:Key, cLookupColumnWidth).
                    IF iColumnWidthIndex > 0 THEN
                        oBand:COLUMNS[j - 1]:Width = INTEGER(ENTRY(iColumnWidthIndex + 1, cLookupColumnWidth)).

                    IF NOT ListHelper:EntryIsInList (oBand:COLUMNS[j - 1]:Key, LookupBrowserFields) THEN
                        oBand:Columns[j - 1]:Hidden = TRUE.
                END.

                DO j = 1 TO NUM-ENTRIES (LookupBrowserFields):
                    oBand:Columns [ENTRY (j, LookupBrowserFields)]:Header:VisiblePosition = j .
                END.
        END.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the stored settings for this lookup
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID RestoreLookupSettings ():

        DEFINE VARIABLE cPosition   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSize       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cColumnSize AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .

        IF cLookupRegistryKey > "":U THEN .
        ELSE RETURN .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        ASSIGN
            cPosition   = oSettingsService:GetSetting (cLookupRegistryKey,
                                                      "Position":U)
            cSize       = oSettingsService:GetSetting (cLookupRegistryKey,
                                                       "Size":U)
            cColumnSize = oSettingsService:GetSetting (cLookupRegistryKey,
                                                       "ColumnSize":U) .

        IF NUM-ENTRIES(cPosition) = 2 THEN
            ASSIGN THIS-OBJECT:Top  = INTEGER(ENTRY(1, cPosition))
                   THIS-OBJECT:Left = INTEGER(ENTRY(2, cPosition)) NO-ERROR .

        IF NUM-ENTRIES(cSize) = 2 THEN
            ASSIGN THIS-OBJECT:Width  = INTEGER(ENTRY(1, cSize))
                   THIS-OBJECT:Height = INTEGER(ENTRY(2, cSize)) NO-ERROR .

        cLookupColumnwidth = cColumnSize.

        CATCH ple AS Progress.Lang.Error :
            /* Ignore Errors
               the registry key will not exist on first run of a dialog */
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Closed event of the Lookup Dialog
        Notes:   Stores the Lookup Settings in the Settings Service
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID StoreLookupSettings (sender AS System.Object,
                                             e AS System.EventArgs):

        DEFINE VARIABLE cPosition   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cSize       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cColumnSize AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oGrid AS UltraGrid     NO-UNDO.
        DEFINE VARIABLE oBand AS UltraGridBand NO-UNDO.
        DEFINE VARIABLE i     AS INTEGER       NO-UNDO.
        DEFINE VARIABLE j     AS INTEGER       NO-UNDO.

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .

        IF cLookupRegistryKey > "":U THEN .
        ELSE RETURN .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        ASSIGN
            cPosition = STRING(THIS-OBJECT:Top) + "," + STRING(THIS-OBJECT:Left)
            cSize     = STRING(THIS-OBJECT:Width) + "," + STRING(THIS-OBJECT:Height)
            .

        oGrid = THIS-OBJECT:smartDataBrowser1.

        IF LookupBrowserFields > "":U THEN
            DO i = 1 TO oGrid:DisplayLayout:Bands:Count:
                oBand = oGrid:DisplayLayout:Bands[i - 1] .

                DO j = 1 TO oBand:Columns:Count:
                    IF ListHelper:EntryIsInList (oBand:COLUMNS[j - 1]:Key, LookupBrowserFields) THEN DO:
                        cColumnSize = TRIM(SUBSTITUTE("&1,&2,&3", cColumnSize, oBand:COLUMNS[j - 1]:Key, oBand:COLUMNS[j - 1]:Width), ",").
                    END.
                END.
        END.

        oSettingsService:StoreSetting (cLookupRegistryKey,
                                       "Position":U,
                                       cPosition) .
        oSettingsService:StoreSetting (cLookupRegistryKey,
                                       "Size":U,
                                       cSize) .
        oSettingsService:StoreSetting (cLookupRegistryKey,
                                       "ColumnSize":U,
                                       cColumnSize) .

        CATCH ple AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (ple) .
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Method to be executed inside the class which implements the Interface
                 IDefaultActionSubscriber.
        Notes:   Invoked by the SmartDataBrowser Instance if the DefaultActionHandler
                 Method is executed.
        @param sender The reference to the Instance of the SmartDataBrowser which has invoked the Method
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SmartDataBrowserDefaultActionHandler (sender AS SmartDataBrowser,
                                                             e AS System.EventArgs):

        THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:OK.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbSearchField
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID tbSearchField_EditorButtonClick (sender AS System.Object,
                                                           e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        ApplyFilter() .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the tbSearchField
        Notes:
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID tbSearchField_KeyDown (sender AS System.Object,
                                                 e AS System.Windows.Forms.KeyEventArgs):

        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode,
                                              System.Windows.Forms.Keys:Enter) OR
           Progress.Util.EnumHelper:AreEqual (e:KeyCode,
                                              System.Windows.Forms.Keys:Return) THEN

            ApplyFilter () .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:  Destructor of the SmartDataObjectLookupDialog
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartDataObjectLookupDialog ():

        IF VALID-OBJECT (oAdapter) THEN
            DELETE OBJECT oAdapter .

    END DESTRUCTOR.

END CLASS.
