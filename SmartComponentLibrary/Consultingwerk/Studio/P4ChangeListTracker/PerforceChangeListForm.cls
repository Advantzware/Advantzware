/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : PerforceChangeListForm
    Purpose     : Shows submitted Perforce Change lists and allows to copy
                  the description to the clipboard
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 13 12:21:52 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm.
USING Consultingwerk.Studio.P4ChangeListTracker.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.Studio.P4.* FROM PROPATH.

CLASS Consultingwerk.Studio.P4ChangeListTracker.PerforceChangeListForm INHERITS BaseForm: 

    { Consultingwerk/Studio/P4/ttChangeLists.i }

    DEFINE PRIVATE VARIABLE changeListDataGridViewTextBoxColumn AS System.Windows.Forms.DataGridViewTextBoxColumn NO-UNDO.
    DEFINE PRIVATE VARIABLE changeDescriptionDataGridViewTextBoxColumn AS System.Windows.Forms.DataGridViewTextBoxColumn NO-UNDO.
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE timeStringDataGridViewTextBoxColumn AS System.Windows.Forms.DataGridViewTextBoxColumn NO-UNDO.
    DEFINE PRIVATE VARIABLE dateStringDataGridViewTextBoxColumn AS System.Windows.Forms.DataGridViewTextBoxColumn NO-UNDO.
    DEFINE PRIVATE VARIABLE dataGridView1 AS System.Windows.Forms.DataGridView NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripButton2 AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripContainer1 AS System.Windows.Forms.ToolStripContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripButton1 AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStrip1 AS System.Windows.Forms.ToolStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE userNameDataGridViewTextBoxColumn AS System.Windows.Forms.DataGridViewTextBoxColumn NO-UNDO.

    DEFINE QUERY qQuery FOR ttChangeLists SCROLLING . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PerforceChangeListForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC PerforceChangeListForm ():
		
        SUPER().
        
        InitializeComponent().

        OPEN QUERY qQuery PRESELECT EACH ttChangeLists BY ttChangeLists.ChangeList DESCENDING .
        
        bindingSource1:Handle = QUERY qQuery:HANDLE .  
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

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
        DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc3 = NEW Progress.Data.TableDesc("ttChangeLists":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.P4ChangeListTracker.PerforceChangeListForm":U).
        THIS-OBJECT:toolStripContainer1 = NEW System.Windows.Forms.ToolStripContainer().
        THIS-OBJECT:dataGridView1 = NEW System.Windows.Forms.DataGridView().
        THIS-OBJECT:changeListDataGridViewTextBoxColumn = NEW System.Windows.Forms.DataGridViewTextBoxColumn().
        THIS-OBJECT:dateStringDataGridViewTextBoxColumn = NEW System.Windows.Forms.DataGridViewTextBoxColumn().
        THIS-OBJECT:timeStringDataGridViewTextBoxColumn = NEW System.Windows.Forms.DataGridViewTextBoxColumn().
        THIS-OBJECT:userNameDataGridViewTextBoxColumn = NEW System.Windows.Forms.DataGridViewTextBoxColumn().
        THIS-OBJECT:changeDescriptionDataGridViewTextBoxColumn = NEW System.Windows.Forms.DataGridViewTextBoxColumn().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:toolStrip1 = NEW System.Windows.Forms.ToolStrip().
        THIS-OBJECT:toolStripButton1 = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:toolStripButton2 = NEW System.Windows.Forms.ToolStripButton().
        THIS-OBJECT:toolStripContainer1:ContentPanel:SuspendLayout().
        THIS-OBJECT:toolStripContainer1:TopToolStripPanel:SuspendLayout().
        THIS-OBJECT:toolStripContainer1:SuspendLayout().
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:toolStrip1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* toolStripContainer1 */
        /*  */
        /*  */
        /* toolStripContainer1.ContentPanel */
        /*  */
        THIS-OBJECT:toolStripContainer1:ContentPanel:Controls:Add(THIS-OBJECT:dataGridView1).
        THIS-OBJECT:toolStripContainer1:ContentPanel:Size = NEW System.Drawing.Size(565, 241).
        THIS-OBJECT:toolStripContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:toolStripContainer1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:toolStripContainer1:Name = "toolStripContainer1":U.
        THIS-OBJECT:toolStripContainer1:Size = NEW System.Drawing.Size(565, 266).
        THIS-OBJECT:toolStripContainer1:TabIndex = 0.
        THIS-OBJECT:toolStripContainer1:Text = "toolStripContainer1":U.
        /*  */
        /* toolStripContainer1.TopToolStripPanel */
        /*  */
        THIS-OBJECT:toolStripContainer1:TopToolStripPanel:Controls:Add(THIS-OBJECT:toolStrip1).
        /*  */
        /* dataGridView1 */
        /*  */
        THIS-OBJECT:dataGridView1:AllowUserToAddRows = FALSE.
        THIS-OBJECT:dataGridView1:AllowUserToDeleteRows = FALSE.
        THIS-OBJECT:dataGridView1:AllowUserToOrderColumns = TRUE.
        THIS-OBJECT:dataGridView1:AutoGenerateColumns = FALSE.
        THIS-OBJECT:dataGridView1:ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode:AutoSize.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.DataGridViewColumn EXTENT 5 NO-UNDO.
        arrayvar0[1] = THIS-OBJECT:changeListDataGridViewTextBoxColumn.
        arrayvar0[2] = THIS-OBJECT:dateStringDataGridViewTextBoxColumn.
        arrayvar0[3] = THIS-OBJECT:timeStringDataGridViewTextBoxColumn.
        arrayvar0[4] = THIS-OBJECT:userNameDataGridViewTextBoxColumn.
        arrayvar0[5] = THIS-OBJECT:changeDescriptionDataGridViewTextBoxColumn.
        THIS-OBJECT:dataGridView1:Columns:AddRange(arrayvar0).
        THIS-OBJECT:dataGridView1:DataSource = THIS-OBJECT:bindingSource1.
        THIS-OBJECT:dataGridView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:dataGridView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:dataGridView1:Name = "dataGridView1":U.
        THIS-OBJECT:dataGridView1:ReadOnly = TRUE.
        THIS-OBJECT:dataGridView1:SelectionMode = System.Windows.Forms.DataGridViewSelectionMode:FullRowSelect.
        THIS-OBJECT:dataGridView1:Size = NEW System.Drawing.Size(565, 241).
        THIS-OBJECT:dataGridView1:TabIndex = 0.
        /*  */
        /* changeListDataGridViewTextBoxColumn */
        /*  */
        THIS-OBJECT:changeListDataGridViewTextBoxColumn:DataPropertyName = "ChangeList":U.
        THIS-OBJECT:changeListDataGridViewTextBoxColumn:HeaderText = "ChangeList":U.
        THIS-OBJECT:changeListDataGridViewTextBoxColumn:Name = "changeListDataGridViewTextBoxColumn":U.
        THIS-OBJECT:changeListDataGridViewTextBoxColumn:ReadOnly = TRUE.
        /*  */
        /* dateStringDataGridViewTextBoxColumn */
        /*  */
        THIS-OBJECT:dateStringDataGridViewTextBoxColumn:DataPropertyName = "DateString":U.
        THIS-OBJECT:dateStringDataGridViewTextBoxColumn:HeaderText = "DateString":U.
        THIS-OBJECT:dateStringDataGridViewTextBoxColumn:Name = "dateStringDataGridViewTextBoxColumn":U.
        THIS-OBJECT:dateStringDataGridViewTextBoxColumn:ReadOnly = TRUE.
        /*  */
        /* timeStringDataGridViewTextBoxColumn */
        /*  */
        THIS-OBJECT:timeStringDataGridViewTextBoxColumn:DataPropertyName = "TimeString":U.
        THIS-OBJECT:timeStringDataGridViewTextBoxColumn:HeaderText = "TimeString":U.
        THIS-OBJECT:timeStringDataGridViewTextBoxColumn:Name = "timeStringDataGridViewTextBoxColumn":U.
        THIS-OBJECT:timeStringDataGridViewTextBoxColumn:ReadOnly = TRUE.
        /*  */
        /* userNameDataGridViewTextBoxColumn */
        /*  */
        THIS-OBJECT:userNameDataGridViewTextBoxColumn:DataPropertyName = "UserName":U.
        THIS-OBJECT:userNameDataGridViewTextBoxColumn:HeaderText = "UserName":U.
        THIS-OBJECT:userNameDataGridViewTextBoxColumn:Name = "userNameDataGridViewTextBoxColumn":U.
        THIS-OBJECT:userNameDataGridViewTextBoxColumn:ReadOnly = TRUE.
        /*  */
        /* changeDescriptionDataGridViewTextBoxColumn */
        /*  */
        THIS-OBJECT:changeDescriptionDataGridViewTextBoxColumn:DataPropertyName = "ChangeDescription":U.
        THIS-OBJECT:changeDescriptionDataGridViewTextBoxColumn:HeaderText = "ChangeDescription":U.
        THIS-OBJECT:changeDescriptionDataGridViewTextBoxColumn:Name = "changeDescriptionDataGridViewTextBoxColumn":U.
        THIS-OBJECT:changeDescriptionDataGridViewTextBoxColumn:ReadOnly = TRUE.
        THIS-OBJECT:changeDescriptionDataGridViewTextBoxColumn:Width = 300.
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar1 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc3:ChildTables = arrayvar1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
        arrayvar2[1] = NEW Progress.Data.ColumnPropDesc("ChangeList":U, "":U, Progress.Data.DataType:INTEGER).
        arrayvar2[2] = NEW Progress.Data.ColumnPropDesc("DateString":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar2[3] = NEW Progress.Data.ColumnPropDesc("TimeString":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar2[4] = NEW Progress.Data.ColumnPropDesc("UserName":U, "":U, Progress.Data.DataType:CHARACTER).
        arrayvar2[5] = NEW Progress.Data.ColumnPropDesc("ChangeDescription":U, "":U, Progress.Data.DataType:CHARACTER).
        tableDesc3:Columns = arrayvar2.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc3.
        /*  */
        /* toolStrip1 */
        /*  */
        THIS-OBJECT:toolStrip1:Dock = System.Windows.Forms.DockStyle:None.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
        arrayvar3[1] = THIS-OBJECT:toolStripButton1.
        arrayvar3[2] = THIS-OBJECT:toolStripButton2.
        THIS-OBJECT:toolStrip1:Items:AddRange(arrayvar3).
        THIS-OBJECT:toolStrip1:Location = NEW System.Drawing.Point(3, 0).
        THIS-OBJECT:toolStrip1:Name = "toolStrip1":U.
        THIS-OBJECT:toolStrip1:Size = NEW System.Drawing.Size(233, 25).
        THIS-OBJECT:toolStrip1:TabIndex = 0.
        /*  */
        /* toolStripButton1 */
        /*  */
        THIS-OBJECT:toolStripButton1:Image = CAST(resources:GetObject("toolStripButton1.Image":U), System.Drawing.Image).
        THIS-OBJECT:toolStripButton1:ImageTransparentColor = System.Drawing.Color:Magenta.
        THIS-OBJECT:toolStripButton1:Name = "toolStripButton1":U.
        THIS-OBJECT:toolStripButton1:Size = NEW System.Drawing.Size(66, 22).
        THIS-OBJECT:toolStripButton1:Text = "Refresh":U.
        THIS-OBJECT:toolStripButton1:Click:Subscribe(THIS-OBJECT:toolStripButton1_Click).
        /*  */
        /* toolStripButton2 */
        /*  */
        THIS-OBJECT:toolStripButton2:Image = CAST(resources:GetObject("toolStripButton2.Image":U), System.Drawing.Image).
        THIS-OBJECT:toolStripButton2:ImageTransparentColor = System.Drawing.Color:Magenta.
        THIS-OBJECT:toolStripButton2:Name = "toolStripButton2":U.
        THIS-OBJECT:toolStripButton2:Size = NEW System.Drawing.Size(124, 22).
        THIS-OBJECT:toolStripButton2:Text = "Copy to Clipboard":U.
        THIS-OBJECT:toolStripButton2:Click:Subscribe(THIS-OBJECT:toolStripButton2_Click).
        /*  */
        /* PerforceChangeListForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(565, 266).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:toolStripContainer1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "PerforceChangeListForm":U.
        THIS-OBJECT:Text = "Submitted Perforce Change Lists":U.
        THIS-OBJECT:WindowPositionRegistryKey = "PerforceChangeListForm":U.
        THIS-OBJECT:toolStripContainer1:ContentPanel:ResumeLayout(FALSE).
        THIS-OBJECT:toolStripContainer1:TopToolStripPanel:ResumeLayout(FALSE).
        THIS-OBJECT:toolStripContainer1:TopToolStripPanel:PerformLayout().
        THIS-OBJECT:toolStripContainer1:ResumeLayout(FALSE).
        THIS-OBJECT:toolStripContainer1:PerformLayout().
        CAST(THIS-OBJECT:dataGridView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:toolStrip1:ResumeLayout(FALSE).
        THIS-OBJECT:toolStrip1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
		
		SUPER:OnShown (e) .
		
		PROCESS EVENTS . 
		
		THIS-OBJECT:ReloadChanges() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Reloads the Change lists from Perforce 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ReloadChanges ():
		
        DEFINE VARIABLE oChanges AS Changes NO-UNDO .  
        
        oChanges = NEW Changes() . 
        
        oChanges:GetChanges (ChangeListStatusEnum:Submitted, 20, OUTPUT TABLE ttChangeLists) .

        OPEN QUERY qQuery PRESELECT EACH ttChangeLists BY ttChangeLists.ChangeList DESCENDING .

        bindingSource1:RefreshAll() .        

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the toolStripButton
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID toolStripButton1_Click (sender AS System.Object, e AS System.EventArgs):
		
		THIS-OBJECT:ReloadChanges () . 
		
		CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the toolStripButton2
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID toolStripButton2_Click (sender AS System.Object, e AS System.EventArgs):
		
		DEFINE VARIABLE oDescribe AS Describe NO-UNDO . 
		DEFINE VARIABLE lcText    AS LONGCHAR NO-UNDO . 
		
		IF AVAILABLE (ttChangeLists) THEN DO:
		    
		    oDescribe = NEW Describe () . 
		    
		    lcText = oDescribe:ToLongchar (STRING (ttChangeLists.ChangeList)) .
		    
		    CLIPBOARD:VALUE = lcText . 
		    
        END.		    

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the PerforceChangeListForm class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC PerforceChangeListForm ():

    END DESTRUCTOR .

END CLASS.
