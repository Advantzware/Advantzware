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
    File        : ObjectExplorerForm
    Purpose     : Allows the user to browse objects loaded in the current 
                  session
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 13 22:11:53 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .
USING Progress.Windows.Form FROM ASSEMBLY .
USING System.Windows.Forms.* FROM ASSEMBLY .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.ObjectExplorer.ObjectExplorerForm INHERITS Form: 
    
    DEFINE PRIVATE VARIABLE bindingSource1 AS System.Windows.Forms.BindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorSeparator2 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorSeparator1 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorSeparator AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorPositionItem AS System.Windows.Forms.ToolStripTextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorMovePreviousItem AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorMoveNextItem AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorMoveLastItem AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorMoveFirstItem AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigatorCountItem AS System.Windows.Forms.ToolStripLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingNavigator1 AS System.Windows.Forms.BindingNavigator NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE propertyGrid1 AS System.Windows.Forms.PropertyGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE dataGridView1 AS System.Windows.Forms.DataGridView NO-UNDO.

        
    DEFINE VARIABLE oTable AS System.Data.DataTable NO-UNDO .         
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ObjectExplorerForm class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ObjectExplorerForm ():
        
        SUPER().
        
        oTable = NEW System.Data.DataTable () .
        oTable:Columns:Add ("Type":U, Progress.Util.TypeHelper:GetType ("System.String":U)) .
        oTable:Columns:Add ("Object":U, Progress.Util.TypeHelper:GetType ("System.Object":U)) .
                
        GetObjects () . 

        InitializeComponent().
                    
        dataGridView1:Columns:Add ("Type":U, "Type":U) .
        dataGridView1:Columns["Type":U]:DataPropertyName = "Type":U .
        dataGridView1:Columns["Type":U]:AutoSizeMode = DataGridViewAutoSizeColumnMode:FILL.

        bindingSource1:DataSource = oTable . 
        propertyGrid1:SelectedObject = oTable:Rows[0]:Item ["Object":U] .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the PositionChanged event of the bindingSource1 instance
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID bindingSource1_PositionChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
        
        propertyGrid1:SelectedObject = CAST(bindingSource1:Current, System.Data.DataRowView):Item["Object":U] .
        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Gets the list of objects in the current session                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID GetObjects ():
        
        DEFINE VARIABLE newRow      AS "System.Object[]":U  NO-UNDO .
        DEFINE VARIABLE hObject     AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE cObjectType AS CHARACTER            NO-UNDO.
        
        hObject = SESSION:FIRST-OBJECT .
        
        DO WHILE VALID-OBJECT (hObject):
            IF TYPE-OF (hObject, System.Object) THEN DO:
        
                IF hObject = THIS-OBJECT THEN DO:
                  hObject = hObject:NEXT-SIBLING . 
                  NEXT .                       
                END.
        
                IF TYPE-OF (hObject, System.Data.DataRow) AND 
                   
                   System.Data.DataTable:ReferenceEquals(CAST (hObject, System.Data.DataRow):TABLE,
                                                               oTable) THEN DO:
                  
                  hObject = hObject:NEXT-SIBLING . 
                  NEXT .                        
                END . 

                IF TYPE-OF (hObject, System.Data.DataTable) AND 
                   
                   System.Data.DataTable:ReferenceEquals(CAST (hObject, System.Data.DataTable),
                                                               oTable) THEN DO:
                  
                  hObject = hObject:NEXT-SIBLING . 
                  NEXT .                       
                END . 

                IF TYPE-OF (hObject, System.Data.DataColumn) AND 
                   
                   System.Data.DataTable:ReferenceEquals(CAST (hObject, System.Data.DataColumn):TABLE,
                                                               oTable) THEN DO:
                  
                  hObject = hObject:NEXT-SIBLING . 
                  NEXT .                       
                END . 

        
                ASSIGN cObjectType = CAST (hObject, System.Object):GetType():FullName .                       

                newRow = {Consultingwerk/new-array.i System.Object 2} .
                newRow:SetValue (BOX(cObjectType), 0) . 
                newRow:SetValue (CAST(hObject, System.Object), 1) .
            
                oTable:Rows:Add (newRow) . 
                
                DELETE OBJECT newRow .
            
            END.
            
            hObject = hObject:NEXT-SIBLING . 
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.ObjectExplorer.ObjectExplorerForm":U).
        THIS-OBJECT:bindingSource1 = NEW System.Windows.Forms.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:bindingNavigator1 = NEW System.Windows.Forms.BindingNavigator(THIS-OBJECT:components).
        THIS-OBJECT:bindingNavigatorCountItem = NEW System.Windows.Forms.ToolStripLabel().
        THIS-OBJECT:bindingNavigatorMoveFirstItem = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:bindingNavigatorMovePreviousItem = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:bindingNavigatorSeparator = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:bindingNavigatorPositionItem = NEW System.Windows.Forms.ToolStripTextBox().
        THIS-OBJECT:bindingNavigatorSeparator1 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:bindingNavigatorMoveNextItem = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:bindingNavigatorMoveLastItem = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:bindingNavigatorSeparator2 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:dataGridView1 = NEW System.Windows.Forms.DataGridView().
        THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
        THIS-OBJECT:propertyGrid1 = NEW System.Windows.Forms.PropertyGrid().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingNavigator1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:bindingNavigator1:SuspendLayout().
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
        THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
        THIS-OBJECT:splitContainer1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:AllowNew = FALSE.
        THIS-OBJECT:bindingSource1:PositionChanged:Subscribe(THIS-OBJECT:bindingSource1_PositionChanged).
        /*  */
        /* bindingNavigator1 */
        /*  */
        THIS-OBJECT:bindingNavigator1:AddNewItem = ?.
        THIS-OBJECT:bindingNavigator1:CountItem = THIS-OBJECT:bindingNavigatorCountItem.
        THIS-OBJECT:bindingNavigator1:DeleteItem = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 9 NO-UNDO.
        arrayvar0[1] = THIS-OBJECT:bindingNavigatorMoveFirstItem.
        arrayvar0[2] = THIS-OBJECT:bindingNavigatorMovePreviousItem.
        arrayvar0[3] = THIS-OBJECT:bindingNavigatorSeparator.
        arrayvar0[4] = THIS-OBJECT:bindingNavigatorPositionItem.
        arrayvar0[5] = THIS-OBJECT:bindingNavigatorCountItem.
        arrayvar0[6] = THIS-OBJECT:bindingNavigatorSeparator1.
        arrayvar0[7] = THIS-OBJECT:bindingNavigatorMoveNextItem.
        arrayvar0[8] = THIS-OBJECT:bindingNavigatorMoveLastItem.
        arrayvar0[9] = THIS-OBJECT:bindingNavigatorSeparator2.
        THIS-OBJECT:bindingNavigator1:Items:AddRange(arrayvar0).
        THIS-OBJECT:bindingNavigator1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:bindingNavigator1:MoveFirstItem = THIS-OBJECT:bindingNavigatorMoveFirstItem.
        THIS-OBJECT:bindingNavigator1:MoveLastItem = THIS-OBJECT:bindingNavigatorMoveLastItem.
        THIS-OBJECT:bindingNavigator1:MoveNextItem = THIS-OBJECT:bindingNavigatorMoveNextItem.
        THIS-OBJECT:bindingNavigator1:MovePreviousItem = THIS-OBJECT:bindingNavigatorMovePreviousItem.
        THIS-OBJECT:bindingNavigator1:Name = "bindingNavigator1":U.
        THIS-OBJECT:bindingNavigator1:PositionItem = THIS-OBJECT:bindingNavigatorPositionItem.
        THIS-OBJECT:bindingNavigator1:Size = NEW System.Drawing.Size(626, 25).
        THIS-OBJECT:bindingNavigator1:TabIndex = 0.
        THIS-OBJECT:bindingNavigator1:Text = "bindingNavigator1":U.
        /*  */
        /* bindingNavigatorCountItem */
        /*  */
        THIS-OBJECT:bindingNavigatorCountItem:Name = "bindingNavigatorCountItem":U.
        THIS-OBJECT:bindingNavigatorCountItem:Size = NEW System.Drawing.Size(44, 22).
        THIS-OBJECT:bindingNavigatorCountItem:Text = "of ~{0~}":U.
        THIS-OBJECT:bindingNavigatorCountItem:ToolTipText = "The total number of elements.":U.
        /*  */
        /* bindingNavigatorMoveFirstItem */
        /*  */
        THIS-OBJECT:bindingNavigatorMoveFirstItem:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:bindingNavigatorMoveFirstItem:Image = CAST(resources:GetObject("bindingNavigatorMoveFirstItem.Image":U), System.Drawing.Image).
        THIS-OBJECT:bindingNavigatorMoveFirstItem:Name = "bindingNavigatorMoveFirstItem":U.
        THIS-OBJECT:bindingNavigatorMoveFirstItem:RightToLeftAutoMirrorImage = TRUE.
        THIS-OBJECT:bindingNavigatorMoveFirstItem:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:bindingNavigatorMoveFirstItem:Text = "Move first":U.
        /*  */
        /* bindingNavigatorMovePreviousItem */
        /*  */
        THIS-OBJECT:bindingNavigatorMovePreviousItem:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:bindingNavigatorMovePreviousItem:Image = CAST(resources:GetObject("bindingNavigatorMovePreviousItem.Image":U), System.Drawing.Image).
        THIS-OBJECT:bindingNavigatorMovePreviousItem:Name = "bindingNavigatorMovePreviousItem":U.
        THIS-OBJECT:bindingNavigatorMovePreviousItem:RightToLeftAutoMirrorImage = TRUE.
        THIS-OBJECT:bindingNavigatorMovePreviousItem:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:bindingNavigatorMovePreviousItem:Text = "Move previous":U.
        /*  */
        /* bindingNavigatorSeparator */
        /*  */
        THIS-OBJECT:bindingNavigatorSeparator:Name = "bindingNavigatorSeparator":U.
        THIS-OBJECT:bindingNavigatorSeparator:Size = NEW System.Drawing.Size(6, 25).
        /*  */
        /* bindingNavigatorPositionItem */
        /*  */
        THIS-OBJECT:bindingNavigatorPositionItem:AccessibleName = "Position":U.
        THIS-OBJECT:bindingNavigatorPositionItem:AutoSize = FALSE.
        THIS-OBJECT:bindingNavigatorPositionItem:Name = "bindingNavigatorPositionItem":U.
        THIS-OBJECT:bindingNavigatorPositionItem:Size = NEW System.Drawing.Size(50, 23).
        THIS-OBJECT:bindingNavigatorPositionItem:Text = "0":U.
        THIS-OBJECT:bindingNavigatorPositionItem:ToolTipText = "Current position":U.
        /*  */
        /* bindingNavigatorSeparator1 */
        /*  */
        THIS-OBJECT:bindingNavigatorSeparator1:Name = "bindingNavigatorSeparator1":U.
        THIS-OBJECT:bindingNavigatorSeparator1:Size = NEW System.Drawing.Size(6, 25).
        /*  */
        /* bindingNavigatorMoveNextItem */
        /*  */
        THIS-OBJECT:bindingNavigatorMoveNextItem:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:bindingNavigatorMoveNextItem:Image = CAST(resources:GetObject("bindingNavigatorMoveNextItem.Image":U), System.Drawing.Image).
        THIS-OBJECT:bindingNavigatorMoveNextItem:Name = "bindingNavigatorMoveNextItem":U.
        THIS-OBJECT:bindingNavigatorMoveNextItem:RightToLeftAutoMirrorImage = TRUE.
        THIS-OBJECT:bindingNavigatorMoveNextItem:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:bindingNavigatorMoveNextItem:Text = "Move next":U.
        /*  */
        /* bindingNavigatorMoveLastItem */
        /*  */
        THIS-OBJECT:bindingNavigatorMoveLastItem:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        THIS-OBJECT:bindingNavigatorMoveLastItem:Image = CAST(resources:GetObject("bindingNavigatorMoveLastItem.Image":U), System.Drawing.Image).
        THIS-OBJECT:bindingNavigatorMoveLastItem:Name = "bindingNavigatorMoveLastItem":U.
        THIS-OBJECT:bindingNavigatorMoveLastItem:RightToLeftAutoMirrorImage = TRUE.
        THIS-OBJECT:bindingNavigatorMoveLastItem:Size = NEW System.Drawing.Size(23, 22).
        THIS-OBJECT:bindingNavigatorMoveLastItem:Text = "Move last":U.
        /*  */
        /* bindingNavigatorSeparator2 */
        /*  */
        THIS-OBJECT:bindingNavigatorSeparator2:Name = "bindingNavigatorSeparator2":U.
        THIS-OBJECT:bindingNavigatorSeparator2:Size = NEW System.Drawing.Size(6, 25).
        /*  */
        /* dataGridView1 */
        /*  */
        THIS-OBJECT:dataGridView1:AutoGenerateColumns = FALSE.
        THIS-OBJECT:dataGridView1:ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode:AutoSize.
        THIS-OBJECT:dataGridView1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:dataGridView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:dataGridView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dataGridView1:Name = "dataGridView1":U.
        THIS-OBJECT:dataGridView1:Size = NEW System.Drawing.Size(251, 511).
        THIS-OBJECT:dataGridView1:TabIndex = 1.
        /*  */
        /* splitContainer1 */
        /*  */
        THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 25).
        THIS-OBJECT:splitContainer1:Name = "splitContainer1":U.
        /*  */
        /* splitContainer1.Panel1 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:dataGridView1).
        /*  */
        /* splitContainer1.Panel2 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:propertyGrid1).
        THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(626, 511).
        THIS-OBJECT:splitContainer1:SplitterDistance = 251.
        THIS-OBJECT:splitContainer1:TabIndex = 2.
        /*  */
        /* propertyGrid1 */
        /*  */
        THIS-OBJECT:propertyGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:propertyGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:propertyGrid1:Name = "propertyGrid1":U.
        THIS-OBJECT:propertyGrid1:Size = NEW System.Drawing.Size(371, 511).
        THIS-OBJECT:propertyGrid1:TabIndex = 0.
        THIS-OBJECT:propertyGrid1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* ObjectExplorerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(626, 536).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:bindingNavigator1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:SizableToolWindow.
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ObjectExplorerForm":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:Manual.
        THIS-OBJECT:Text = ".NET ObjectExplorer":U.
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingNavigator1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:bindingNavigator1:ResumeLayout(FALSE).
        THIS-OBJECT:bindingNavigator1:PerformLayout().
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
        CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ObjectExplorerForm class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ObjectExplorerForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.