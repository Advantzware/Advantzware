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
    File        : DatabaseFieldsControl
    Purpose     : User Control that allows dragging individual database
                  fields to the Business Entity Design Canvas
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 12 16:28:04 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*       FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Infragistics.Win.UltraWinExplorerBar.*           FROM ASSEMBLY .
USING Infragistics.Win.UltraWinListView.*              FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .
USING Progress.Windows.UserControl                     FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.UI.DatabaseFieldsControl
    INHERITS UserControl:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraExplorerBar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraExplorerBarContainerControl1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarContainerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the DatabaseSchemaDatasetController
                 instance
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatabaseSchemaDatasetController AS DatabaseSchemaDatasetController NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatabaseFieldsControl class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatabaseFieldsControl ():


        SUPER().
        InitializeComponent().

        THIS-OBJECT:ultraExplorerBar1:Groups[0]:Visible = FALSE .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the fields of the given database tables
        Notes:
        @param pcDatabaseName The name of the database
        @param pcTableName The name of the database table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectTable (pcDatabaseName AS CHARACTER,
                                    pcTableName AS CHARACTER):

        DEFINE VARIABLE hDataset AS HANDLE               NO-UNDO.
        DEFINE VARIABLE hQuery   AS HANDLE               NO-UNDO.

        DEFINE VARIABLE oItem    AS UltraListViewItem    NO-UNDO .

        THIS-OBJECT:ultraExplorerBar1:Groups[0]:Visible = TRUE .
        THIS-OBJECT:ultraExplorerBar1:Groups[0]:Text = SUBSTITUTE ("&1.&2":U,
                                                                   pcDatabaseName,
                                                                   pcTableName) .

        THIS-OBJECT:ultraListView1:Items:Clear () .

        IF NOT VALID-OBJECT (THIS-OBJECT:DatabaseSchemaDatasetController) THEN
            LEAVE .

           hDataset = THIS-OBJECT:DatabaseSchemaDatasetController:GetTableSchema (pcDatabaseName,
                                                                                  pcTableName) .

        hQuery = QueryHelper:CreatePreparedQuery (hDataset::eField) .

        DO WHILE NOT hQuery:QUERY-OFF-END:
            oItem = ultraListView1:Items:Add
                      (SUBSTITUTE ("field.&1.&2.&3":U,
                                   pcDatabaseName,
                                   pcTableName,
                                   hDataset::eField::FieldName),
                       hDataset::eField::FieldName) .

            oItem:Tag = BOX (SUBSTITUTE ("&1&9&2&9&3&9&4&9&5":U,
                                         hDataset::eField::FieldDataType,
                                         hDataset::eField::FieldInitial,
                                         hDataset::eField::FieldLabel,
                                         hDataset::eField::FieldFormat,
                                         hDataset::eField::FieldExtent,
                                         "":U,
                                         "":U,
                                         "":U,
                                         CHR (1))) .

            hQuery:GET-NEXT () .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hDataset) .
            GarbageCollectorHelper:DeleteObject (hQuery) .
        END FINALLY.


    END METHOD .

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.DatabaseFieldsControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraExplorerBarGroup1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup NO-UNDO.
        ultraExplorerBarGroup1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup().
        THIS-OBJECT:ultraExplorerBarContainerControl1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarContainerControl().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:ultraExplorerBar1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar().
        THIS-OBJECT:ultraExplorerBarContainerControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraExplorerBar1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraExplorerBarContainerControl1 */
        /*  */
        THIS-OBJECT:ultraExplorerBarContainerControl1:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:ultraExplorerBarContainerControl1:Location = NEW System.Drawing.Point(3, 21).
        THIS-OBJECT:ultraExplorerBarContainerControl1:Name = "ultraExplorerBarContainerControl1":U.
        THIS-OBJECT:ultraExplorerBarContainerControl1:Size = NEW System.Drawing.Size(175, 369).
        THIS-OBJECT:ultraExplorerBarContainerControl1:TabIndex = 0.
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:BorderStyle = Infragistics.Win.UIElementBorderStyle:None.
        THIS-OBJECT:ultraListView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraListView1:ItemSettings:DefaultImage = CAST(resources:GetObject("ultraListView1.ItemSettings.DefaultImage":U), System.Drawing.Image).
        THIS-OBJECT:ultraListView1:ItemSettings:SelectionType = Infragistics.Win.UltraWinListView.SelectionType:Single.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:ShowGroups = FALSE.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(175, 369).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:ultraListView1:MouseDown:Subscribe(THIS-OBJECT:ultraListView1_MouseDown).
        /*  */
        /* ultraExplorerBar1 */
        /*  */
        THIS-OBJECT:ultraExplorerBar1:Controls:Add(THIS-OBJECT:ultraExplorerBarContainerControl1).
        THIS-OBJECT:ultraExplorerBar1:Dock = System.Windows.Forms.DockStyle:Fill.
        ultraExplorerBarGroup1:Container = THIS-OBJECT:ultraExplorerBarContainerControl1.
        ultraExplorerBarGroup1:Settings:Style = Infragistics.Win.UltraWinExplorerBar.GroupStyle:ControlContainer.
        ultraExplorerBarGroup1:Text = "New Group":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarGroup EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraExplorerBarGroup1.
        THIS-OBJECT:ultraExplorerBar1:Groups:AddRange(arrayvar0).
        THIS-OBJECT:ultraExplorerBar1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraExplorerBar1:Name = "ultraExplorerBar1":U.
        THIS-OBJECT:ultraExplorerBar1:Size = NEW System.Drawing.Size(181, 411).
        THIS-OBJECT:ultraExplorerBar1:Style = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarStyle:Toolbox.
        THIS-OBJECT:ultraExplorerBar1:TabIndex = 0.
        THIS-OBJECT:ultraExplorerBar1:ViewStyle = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarViewStyle:Office2007.
        /*  */
        /* DatabaseFieldsControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraExplorerBar1).
        THIS-OBJECT:Name = "DatabaseFieldsControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(181, 411).
        THIS-OBJECT:ultraExplorerBarContainerControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraExplorerBar1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the mouse down event of the UltraListView Control
        Notes:
        @param sender The reference to the object that raised the event
        @param e The MouseEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ultraListView1_MouseDown (sender AS System.Object,
                                                    e AS System.Windows.Forms.MouseEventArgs):

        DEFINE VARIABLE oListViewItem AS UltraListViewItem NO-UNDO .

        oListViewItem = ultraListView1:ItemFromPoint (e:Location) .

        IF VALID-OBJECT (oListViewItem) THEN
            ultraListView1:DoDragDrop (oListViewItem,
                                  CAST (Progress.Util.EnumHelper:Or (System.Windows.Forms.DragDropEffects:Copy,
                                                                     System.Windows.Forms.DragDropEffects:Link),
                                        System.Windows.Forms.DragDropEffects)) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DatabaseFieldsControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DatabaseFieldsControl ():

    END DESTRUCTOR .

END CLASS.
