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
    File        : DynamicBrowserRenderer
    Purpose     : Rendering engine for SmartDynamicBrowser and
                  SmartDynamicUpdatableBrowser controls
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 15 16:04:15 CEST 2013
    Notes       : Based on Browser definition contained in
                  Consultingwerk/Framework/RepositoryServices/dsDynamicBrowser.i
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.*                FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.*              FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                 FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

{ Consultingwerk/products.i }

CLASS Consultingwerk.SmartComponents.Implementation.DynamicBrowserRenderer
    IMPLEMENTS IDynamicBrowserRenderer:

    {Consultingwerk/Framework/RepositoryServices/dsDynamicBrowser.i &REFERENCE-ONLY=REFERENCE-ONLY}

    /*------------------------------------------------------------------------------
        Purpose: Renders a Dynamic Browser based on the repository definition
        Notes:
        @param pcBrowserKey The key of the dynamic browser to render
        @param poBrowser The reference to the SmartDataBrowser to apply the settings to
        @param dsDynamicBrowser DATASET The dataset with the browser layout information
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RenderDynamicBrowser (pcBrowserKey AS CHARACTER,
                                             poBrowser AS SmartDataBrowser,
                                             DATASET dsDynamicBrowser):

        DEFINE VARIABLE oUpdatableGrid     AS SmartUpdatableBrowser            NO-UNDO .
        DEFINE VARIABLE oColumn            AS UltraGridColumn                  NO-UNDO .
        DEFINE VARIABLE oValueListProvider AS IValueListProvider               NO-UNDO .
        DEFINE VARIABLE oCustomizer        AS IDynamicBrowseRendererCustomizer NO-UNDO .


        ASSIGN oValueListProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IValueListProvider}
               oCustomizer        = {Consultingwerk/get-service.i Consultingwerk.SmartComponents.Implementation.IDynamicBrowseRendererCustomizer}.


        FIND ttBrowserProperties WHERE ttBrowserProperties.BrowserKey = pcBrowserKey NO-ERROR.

        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i "BUFFER ttBrowserProperties:HANDLE"} .

        IF TYPE-OF (poBrowser, SmartUpdatableBrowser) THEN
            oUpdatableGrid = CAST (poBrowser, SmartUpdatableBrowser) .

        /* Hide columns not part of the layout */
        {Consultingwerk/foreach.i UltraGridColumn oHideColumn in poBrowser:DisplayLayout:Bands[0]:Columns}

            FIND FIRST ttColumnProperties OF ttBrowserProperties
                 WHERE ttColumnProperties.ColumnKey = oHideColumn:Key NO-ERROR .

            IF NOT AVAILABLE ttColumnProperties THEN DO:
                oHideColumn:Hidden = TRUE .
                oHideColumn:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True .
            END .
        END.

        /* AllowGroupBy */
        IF ttBrowserProperties.AllowGroupBy THEN
            ASSIGN poBrowser:DisplayLayout:Override:AllowGroupBy = Infragistics.Win.DefaultableBoolean:True
                   poBrowser:DisplayLayout:ViewStyleBand = ViewStyleBand:OutlookGroupBy .
        ELSE
            ASSIGN poBrowser:DisplayLayout:Override:AllowGroupBy = Infragistics.Win.DefaultableBoolean:False
                   poBrowser:DisplayLayout:ViewStyleBand = ViewStyleBand:Horizontal .

        /* MaxRowScrollRegions */
        IF ttBrowserProperties.MaxRowScrollRegions > 0 THEN
           poBrowser:DisplayLayout:MaxRowScrollRegions = ttBrowserProperties.MaxRowScrollRegions .

        /* MaxColScrollRegions */
        IF ttBrowserProperties.MaxColScrollRegions > 0 THEN
           poBrowser:DisplayLayout:MaxColScrollRegions = ttBrowserProperties.MaxColScrollRegions .

        /* RowSelectors */
        IF ttBrowserProperties.RowSelectors = TRUE THEN
            poBrowser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:True .
        ELSE
            poBrowser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False .

        /* SelectTypeRow */
        IF ttBrowserProperties.SelectTypeRow > "":U THEN
            poBrowser:DisplayLayout:Override:SelectTypeRow = {Consultingwerk/parse-enum-ignorecase.i Infragistics.Win.UltraWinGrid.SelectType ttBrowserProperties.SelectTypeRow} .

        /* SettingsKey */
        IF ttBrowserProperties.SettingsKey > "":U THEN
            poBrowser:SettingsKey = ttBrowserProperties.SettingsKey .

        ASSIGN poBrowser:SmartFilterActive             = ttBrowserProperties.SmartFilterActive
               poBrowser:DisplayLayout:UseFixedHeaders = ttBrowserProperties.UseFixedHeaders.

        /* Mike Fechner, Consultingwerk Ltd. 26.03.2014
           Customizer */
        IF VALID-OBJECT (oCustomizer) THEN
            oCustomizer:AssignBrowserProperties (poBrowser,
                                                 BUFFER ttBrowserProperties:HANDLE) .

        columnLoop:
        FOR EACH ttColumnProperties OF ttBrowserProperties:

            /* Get the column */
            IF ttColumnProperties.CalculatedColumn THEN DO:
                oColumn = poBrowser:DisplayLayout:Bands[0]:Columns:Add (ttColumnProperties.ColumnKey) .

                IF ttColumnProperties.ColumnDataType > "":U THEN DO:
                    IF NUM-ENTRIES (ttColumnProperties.ColumnDataType, ".":U) > 1 THEN
                        oColumn:DataType = Progress.Util.TypeHelper:GetType (ttColumnProperties.ColumnDataType) .
                    ELSE
                        oColumn:DataType = Consultingwerk.Util.DataTypeHelper:ToSystemType (ttColumnProperties.ColumnDataType) .
                END.
            END.
            ELSE DO:
                IF NOT poBrowser:DisplayLayout:Bands[0]:Columns:Exists (ttColumnProperties.ColumnKey) THEN DO:
                    Consultingwerk.Util.MessageFormHelper:ShowMessage (SUBSTITUTE ("Invalid Column Key: &1 in &2."{&TRAN},
                                                                                   ttColumnProperties.ColumnKey, pcBrowserKey),
                                                                       "Dynamic Browser"{&TRAN},
                                                                       Consultingwerk.Windows.Util.Forms.MessageFormImages:ImageError) .
                    NEXT columnLoop .
                 END.

                oColumn = poBrowser:DisplayLayout:Bands[0]:Columns [ttColumnProperties.ColumnKey] .
            END.

            /* Order */
            IF ttColumnProperties.FieldOrder >= 0 THEN
                oColumn:Header:VisiblePosition = ttColumnProperties.FieldOrder .

            /* Hidden */
            IF ttColumnProperties.Hidden THEN
                oColumn:Hidden = TRUE .
            ELSE
                oColumn:Hidden = FALSE .

            /* ExcludeFromColumnChooser */
            IF ttColumnProperties.ExcludeFromColumnChooser THEN
                oColumn:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:True .
            ELSE
                oColumn:ExcludeFromColumnChooser = Infragistics.Win.UltraWinGrid.ExcludeFromColumnChooser:False .

            /* Width */
            IF ttColumnProperties.Width > 0 THEN
                oColumn:Width = ttColumnProperties.Width .

            /* Caption */
            oColumn:Header:Caption = ttColumnProperties.Caption .

            /*  ToolTipText */
            oColumn:Header:ToolTipText = ttColumnProperties.ToolTipText .

            /* AllowSorting */
            IF ttColumnProperties.AllowSorting = FALSE THEN
                oColumn:SortIndicator = SortIndicator:Disabled .
            ELSE
                oColumn:SortIndicator = SortIndicator:None .

            /* AllowRowFiltering */
            IF ttColumnProperties.AllowRowFiltering = TRUE THEN
                oColumn:AllowRowFiltering = Infragistics.Win.DefaultableBoolean:True .
            ELSE
                oColumn:AllowRowFiltering = Infragistics.Win.DefaultableBoolean:False .

            /* AllowRowSummaries */
            IF ttColumnProperties.AllowRowSummaries > "":U THEN
                oColumn:AllowRowSummaries = {Consultingwerk/parse-enum-ignorecase.i Infragistics.Win.UltraWinGrid.AllowRowSummaries ttColumnProperties.AllowRowSummaries} .

            /* AllowGroupBy */
            IF ttColumnProperties.AllowGroupBy = TRUE THEN
                oColumn:AllowGroupBy = Infragistics.Win.DefaultableBoolean:True .
            ELSE
                oColumn:AllowGroupBy = Infragistics.Win.DefaultableBoolean:False .

            /* GroupByMode */
            IF ttColumnProperties.GroupByMode > "":U THEN
                oColumn:GroupByMode = {Consultingwerk/parse-enum-ignorecase.i Infragistics.Win.UltraWinGrid.GroupByMode ttColumnProperties.GroupByMode} .

            /* HiddenWhenGroupBy */
            IF ttColumnProperties.HiddenWhenGroupBy = TRUE THEN
                oColumn:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:True .
            ELSE
                oColumn:HiddenWhenGroupBy = Infragistics.Win.DefaultableBoolean:False .

            /* Style */
            IF ttColumnProperties.Style > "":U THEN
                oColumn:Style = {Consultingwerk/parse-enum-ignorecase.i Infragistics.Win.UltraWinGrid.ColumnStyle ttColumnProperties.Style} .

            /* MaskInput */
            IF ttColumnProperties.MaskInput > "":U THEN
                oColumn:MaskInput = ttColumnProperties.MaskInput .

           /* Nullable */
            IF ttColumnProperties.Nullable > "":U THEN
                oColumn:Nullable = {Consultingwerk/parse-enum-ignorecase.i Infragistics.Win.UltraWinGrid.Nullable ttColumnProperties.Nullable} .

            /* NullText */
            IF ttColumnProperties.NullText > "":U THEN
                oColumn:NullText = ttColumnProperties.NullText .

            /* HeaderFixed */
            oColumn:Header:Fixed = ttColumnProperties.HeaderFixed .

            /* Updatable Browser */
            IF VALID-OBJECT (oUpdatableGrid) THEN DO:

                IF ttColumnProperties.EnabledForAdd = FALSE THEN
                    oUpdatableGrid:DisabledColumnsForAdd = oUpdatableGrid:DisabledColumnsForAdd +
                                                           (IF oUpdatableGrid:DisabledColumnsForAdd > "":U THEN ",":U ELSE "":U) +
                                                           ttColumnProperties.ColumnKey .

                IF ttColumnProperties.EnabledForUpdate = FALSE THEN
                    oUpdatableGrid:DisabledColumnsForUpdate = oUpdatableGrid:DisabledColumnsForUpdate +
                                                              (IF oUpdatableGrid:DisabledColumnsForUpdate > "":U THEN ",":U ELSE "":U) +
                                                              ttColumnProperties.ColumnKey .
            END.

            /* ValueList */
            /* Mike Fechner, Consultingwerk Ltd. 26.03.2014
               Value lists with non character key fields require the FullEditorDisplay CellDisplayStyle
               to work properly */
            IF VALID-OBJECT (ttColumnProperties.ValueList) AND TYPE-OF (ttColumnProperties.ValueList, Infragistics.Win.ValueList) THEN DO:
                oColumn:ValueList        = CAST (ttColumnProperties.ValueList, Infragistics.Win.ValueList) .
                oColumn:CellDisplayStyle = Infragistics.Win.UltraWinGrid.CellDisplayStyle:FullEditorDisplay.
            END.
            ELSE IF ttColumnProperties.ValueListKey > "":U AND VALID-OBJECT (oValueListProvider) THEN DO:
                oColumn:ValueList        = oValueListProvider:ValueListFromKey (ttColumnProperties.ValueListKey) .
                oColumn:CellDisplayStyle = Infragistics.Win.UltraWinGrid.CellDisplayStyle:FullEditorDisplay.
            END.

            /* ValueListItems */
            ELSE IF ttColumnProperties.ValueListItems > "":U THEN
                oColumn:ValueList = ValueListHelper:FromList (ttColumnProperties.ValueListItems) .

            ELSE IF ttColumnProperties.ValueListItemPairs > "":U THEN
                oColumn:ValueList = ValueListHelper:FromListItemPairs (ttColumnProperties.ValueListItemPairs) .

            /* Mike Fechner, Consultingwerk Ltd. 26.03.2014
               Customizer */
            IF VALID-OBJECT (oCustomizer) THEN
                oCustomizer:AssignColumnProperties (poBrowser,
                                                    BUFFER ttBrowserProperties:HANDLE,
                                                    oColumn,
                                                    BUFFER ttColumnProperties:HANDLE) .

        END.

    END METHOD .

END CLASS.
