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
    File        : BusinessEntityDatasetController
    Purpose     : Dataset Controller for the Database Schema Dataset
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 13 20:03:46 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.UI.*      FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Infragistics.Win.UltraWinExplorerBar.*          FROM ASSEMBLY .
USING Infragistics.Win.UltraWinListView.*             FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.DatabaseSchemaDatasetController IMPLEMENTS IDatasetController:

    { Consultingwerk/BusinessEntityDesigner/Services/dsDatabaseSchema.i }

    DEFINE VARIABLE oTableAdapter AS SmartDataAdapter NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the DatabaseFieldsControl of the
                 Business Entity Designer
        Notes:   Required for the SelectedItemChanged event handler of the UltraListView
                 instances
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatabaseFieldsControl AS DatabaseFieldsControl NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Database Schema dataset
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the DatabaseSchemaDatasetController class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatabaseSchemaDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsDatabaseSchema:HANDLE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object detaching from the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (INPUT poConsumer AS Progress.Lang.Object):

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a handle of the database schema dataset containing the schema
                 of a the first database table with the given name
        Notes:
        @param pcTable The name of the database table to return
        @return The handle of the database schema dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetTableSchema (pcTable AS CHARACTER):

        FIND FIRST eTable WHERE eTable.TempTableName = pcTable NO-ERROR .

        IF NOT AVAILABLE eTable THEN
            FIND FIRST eTable WHERE eTable.TempTableName MATCHES "*~.":U + pcTable .

        RETURN GetTableSchema (eTable.BusinessEntityName,
                               pcTable) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a handle of the database schema dataset containing the schema
                 of a single database table
        Notes:
        @param pcDatabase The name of the database that contains the table to return
        @param pcTable The name of the database table to return
        @return The handle of the database schema dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC HANDLE GetTableSchema (pcDatabase AS CHARACTER,
                                         pcTable AS CHARACTER):

        DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.

        DEFINE VARIABLE cFullTableName AS CHARACTER NO-UNDO.

        ASSIGN cFullTableName = SUBSTITUTE ("&1.&2":U,
                                            pcDatabase,
                                            pcTable) .

        FIND eTable WHERE eTable.BusinessEntityName = pcDatabase
                      AND eTable.TempTableName      = cFullTableName .

        CREATE DATASET hDataset .
        hDataset:CREATE-LIKE (DATASET dsDatabaseSchema:HANDLE) .

        hDataset::eTable:BUFFER-CREATE () .
        hDataset::eTable:BUFFER-COPY (BUFFER eTable:HANDLE) .

        FOR EACH eField WHERE eField.BusinessEntityName = eTable.BusinessEntityName
                          AND eField.TempTableName      = eTable.TempTableName ON ERROR UNDO, THROW:

            hDataset::eField:BUFFER-CREATE () .
            hDataset::eField:BUFFER-COPY (BUFFER eField:HANDLE) .
        END.

        FOR EACH eIndex WHERE eIndex.BusinessEntityName = eTable.BusinessEntityName
                          AND eIndex.TempTableName      = eTable.TempTableName ON ERROR UNDO, THROW:

            hDataset::eIndex:BUFFER-CREATE () .
            hDataset::eIndex:BUFFER-COPY (BUFFER eIndex:HANDLE) .
        END.

        RETURN hDataset .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the item selection changed event of the UltraListView Controls
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ListViewItemSelectionChangedHandler (sender AS System.Object,
                                                               e AS ItemSelectionChangedEventArgs):

        DEFINE VARIABLE cDBName    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.

        IF e:SelectedItems:Count = 0 THEN
            RETURN .

        IF e:SelectedItems[0]:Key BEGINS "table.":U AND
           CAST (sender, System.Windows.Forms.Control):Name BEGINS "db.":U THEN DO:

            ASSIGN cDBName    = ENTRY (2, CAST (sender, System.Windows.Forms.Control):Name, ".":U)
                   cTableName = ENTRY (3, e:SelectedItems[0]:Key, ".":U) .

            THIS-OBJECT:DatabaseFieldsControl:SelectTable (cDBName, cTableName) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the mouse down event of the UltraListView Controls
        Notes:
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ListViewMouseDownHandler (sender AS System.Object,
                                                    e AS System.Windows.Forms.MouseEventArgs):

        DEFINE VARIABLE oListView     AS UltraListView     NO-UNDO .
        DEFINE VARIABLE oListViewItem AS UltraListViewItem NO-UNDO .

        oListView = CAST (sender, UltraListView) .

        oListViewItem = oListView:ItemFromPoint (e:Location) .

        IF VALID-OBJECT (oListViewItem) THEN
            oListView:DoDragDrop (oListViewItem,
                                  CAST (Progress.Util.EnumHelper:Or (System.Windows.Forms.DragDropEffects:Copy,
                                                                     System.Windows.Forms.DragDropEffects:Link),
                                        System.Windows.Forms.DragDropEffects)) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Toolbox of the Business Entity Designer
        Notes:
        @param poExplorerBar The reference to the UltraExplorerBar Control that contains the Toolbox
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PopulateToolbox (poExplorerBar AS UltraExplorerBar):

        DEFINE VARIABLE oGroup    AS UltraExplorerBarGroup NO-UNDO .
        DEFINE VARIABLE oItem     AS UltraListViewItem     NO-UNDO .
        DEFINE VARIABLE cFilename AS CHARACTER             NO-UNDO.
        DEFINE VARIABLE lFirst    AS LOGICAL               NO-UNDO INIT TRUE .
        DEFINE VARIABLE oListView AS UltraListView         NO-UNDO .


        FILE-INFORMATION:FILE-NAME = "Consultingwerk~\BusinessEntityDesigner~\UI~\Images~\data_table.png":U .
        ASSIGN cFileName = FILE-INFORMATION:FULL-PATHNAME .

        FOR EACH eDatabase ON ERROR UNDO, THROW:
            oGroup = NEW UltraExplorerBarGroup ("db.":U + eDatabase.DatabaseName) .
            oGroup:Text = eDatabase.DatabaseName .
            oGroup:Settings:Style = Infragistics.Win.UltraWinExplorerBar.GroupStyle:ControlContainer .
            poExplorerBar:Groups:Add (oGroup) .

            oListView = NEW UltraListView () .
            oListView:Name = "db.":U + eDatabase.DatabaseName .
            oListView:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List .
            oListView:BorderStyle = Infragistics.Win.UIElementBorderStyle:None .
            oListView:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
            oListView:ShowGroups = FALSE.
            oListView:ViewSettingsList:MultiColumn = FALSE.

            oListView:MouseDown:Subscribe (ListViewMouseDownHandler) .
            oListView:ItemSelectionChanged:Subscribe (ListViewItemSelectionChangedHandler) .

            IF cFileName > "":U THEN
                oListView:ItemSettings:DefaultImage = Consultingwerk.Util.ImageHelper:Load (cFileName) .

            oListView:Dock = System.Windows.Forms.DockStyle:Fill .
            oGroup:Container:Controls:Add (oListView) .

            FOR EACH eTable WHERE eTable.BusinessEntityName = eDatabase.DatabaseName:

                oItem = NEW UltraListViewItem (ENTRY (NUM-ENTRIES (eTable.TempTableName, ".":U), eTable.TempTableName, ".":U), ?, ?) .

                oItem:Key = SUBSTITUTE ("table.&1":U,
                                        eTable.TempTableName) .

                oListView:Items:Add (oItem) .
            END.

            IF lFirst THEN
              poExplorerBar:SelectedGroup = oGroup .

            lFirst = FALSE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object attaching to the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (INPUT poConsumer AS Progress.Lang.Object):

    END METHOD.

END CLASS.
