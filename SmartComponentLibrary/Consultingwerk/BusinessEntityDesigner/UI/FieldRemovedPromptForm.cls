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
    File        : FieldRemovedPromptForm
    Purpose     : Prompt for the action to be taken when fields haev been 
                  removed from the database
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 10 11:42:03 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.UI.*       FROM PROPATH .  
USING Consultingwerk.SmartComponents.Base.*            FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.FieldRemovedPromptForm INHERITS SmartWindowForm: 

    DEFINE PRIVATE VARIABLE btnOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE rsRemoveSourceColumn AS System.Windows.Forms.RadioButton NO-UNDO.
    DEFINE PRIVATE VARIABLE rsRemoveField AS System.Windows.Forms.RadioButton NO-UNDO.
    DEFINE PRIVATE VARIABLE rsDoNothing AS System.Windows.Forms.RadioButton NO-UNDO.
    DEFINE PRIVATE VARIABLE tbDatabaseFieldName AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Field Name to be shown in the dialog 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldName AS CHARACTER NO-UNDO 
    GET:
        RETURN tbDatabaseFieldName:Text .
    END GET .
    SET (arg AS CHARACTER):
        ASSIGN tbDatabaseFieldName:Text = arg .
    END SET . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the FieldRemovedHandlingEnum value active on the Dialog
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FieldRemovedHandling AS FieldRemovedHandlingEnum NO-UNDO 
    GET:
        IF rsRemoveSourceColumn:Checked THEN 
            RETURN FieldRemovedHandlingEnum:RemoveSourceColumn .
        IF rsRemoveField:Checked THEN 
            RETURN FieldRemovedHandlingEnum:RemoveField .
        ELSE
            RETURN FieldRemovedHandlingEnum:DoNothing .
    END GET.
    SET(INPUT arg AS FieldRemovedHandlingEnum):
    
        Consultingwerk.Assertion.ObjectAssert:IsValid (arg) .

        CASE arg:
            WHEN FieldRemovedHandlingEnum:RemoveSourceColumn THEN 
                ASSIGN rsRemoveSourceColumn:Checked = TRUE 
                       rsRemoveField:Checked        = FALSE
                       rsDoNothing:Checked          = FALSE . 
            WHEN FieldRemovedHandlingEnum:RemoveSourceColumn THEN 
                ASSIGN rsRemoveSourceColumn:Checked = FALSE 
                       rsRemoveField:Checked        = TRUE
                       rsDoNothing:Checked          = FALSE . 
            WHEN FieldRemovedHandlingEnum:DoNothing THEN 
                ASSIGN rsRemoveSourceColumn:Checked = FALSE 
                       rsRemoveField:Checked        = FALSE
                       rsDoNothing:Checked          = TRUE . 
        END CASE.
    END SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldRemovedPromptForm class
        Notes:   
    ------------------------------------------------------------------------------*/     
    CONSTRUCTOR PUBLIC FieldRemovedPromptForm (  ):
        
        
        SUPER().
        InitializeComponent().
        
        THIS-OBJECT:StoreFormPosition = FALSE . 
        
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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.FieldRemovedPromptForm":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:rsDoNothing = NEW System.Windows.Forms.RadioButton().
        THIS-OBJECT:rsRemoveField = NEW System.Windows.Forms.RadioButton().
        THIS-OBJECT:rsRemoveSourceColumn = NEW System.Windows.Forms.RadioButton().
        THIS-OBJECT:tbDatabaseFieldName = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:btnOk = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btnCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:rsDoNothing).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:rsRemoveField).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:rsRemoveSourceColumn).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbDatabaseFieldName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(496, 160).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* rsDoNothing */
        /*  */
        THIS-OBJECT:rsDoNothing:AutoSize = TRUE.
        THIS-OBJECT:rsDoNothing:Location = NEW System.Drawing.Point(48, 133).
        THIS-OBJECT:rsDoNothing:Name = "rsDoNothing":U.
        THIS-OBJECT:rsDoNothing:Size = NEW System.Drawing.Size(75, 18).
        THIS-OBJECT:rsDoNothing:TabIndex = 8.
        THIS-OBJECT:rsDoNothing:Text = "do nothing":U.
        THIS-OBJECT:rsDoNothing:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:rsDoNothing:UseVisualStyleBackColor = TRUE.
        /*  */
        /* rsRemoveField */
        /*  */
        THIS-OBJECT:rsRemoveField:AutoSize = TRUE.
        THIS-OBJECT:rsRemoveField:Location = NEW System.Drawing.Point(48, 106).
        THIS-OBJECT:rsRemoveField:Name = "rsRemoveField":U.
        THIS-OBJECT:rsRemoveField:Size = NEW System.Drawing.Size(266, 18).
        THIS-OBJECT:rsRemoveField:TabIndex = 7.
        THIS-OBJECT:rsRemoveField:Text = "remove the field from the Business Entity Design":U.
        THIS-OBJECT:rsRemoveField:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:rsRemoveField:UseVisualStyleBackColor = TRUE.
        /*  */
        /* rsRemoveSourceColumn */
        /*  */
        THIS-OBJECT:rsRemoveSourceColumn:Checked = TRUE.
        THIS-OBJECT:rsRemoveSourceColumn:Location = NEW System.Drawing.Point(48, 69).
        THIS-OBJECT:rsRemoveSourceColumn:Name = "rsRemoveSourceColumn":U.
        THIS-OBJECT:rsRemoveSourceColumn:Size = NEW System.Drawing.Size(445, 31).
        THIS-OBJECT:rsRemoveSourceColumn:TabIndex = 6.
        THIS-OBJECT:rsRemoveSourceColumn:TabStop = TRUE.
        THIS-OBJECT:rsRemoveSourceColumn:Text = "leave the field in the Business Entity Design, but remove the source column information":U.
        THIS-OBJECT:rsRemoveSourceColumn:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:rsRemoveSourceColumn:UseVisualStyleBackColor = TRUE.
        /*  */
        /* tbDatabaseFieldName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDatabaseFieldName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbDatabaseFieldName:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:tbDatabaseFieldName:BorderStyle = System.Windows.Forms.BorderStyle:None.
        THIS-OBJECT:tbDatabaseFieldName:Enabled = FALSE.
        THIS-OBJECT:tbDatabaseFieldName:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:tbDatabaseFieldName:Location = NEW System.Drawing.Point(50, 50).
        THIS-OBJECT:tbDatabaseFieldName:Name = "tbDatabaseFieldName":U.
        THIS-OBJECT:tbDatabaseFieldName:Size = NEW System.Drawing.Size(434, 13).
        THIS-OBJECT:tbDatabaseFieldName:TabIndex = 5.
        THIS-OBJECT:tbDatabaseFieldName:Text = "Customer.CustNum":U.
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 4.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* textBox1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:textBox1:BorderStyle = System.Windows.Forms.BorderStyle:None.
        THIS-OBJECT:textBox1:Enabled = FALSE.
        THIS-OBJECT:textBox1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, 8.25, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(50, 12).
        THIS-OBJECT:textBox1:Multiline = TRUE.
        THIS-OBJECT:textBox1:Name = "textBox1":U.
        THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(434, 32).
        THIS-OBJECT:textBox1:TabIndex = 3.
        THIS-OBJECT:textBox1:Text = "The following database column has been removed from the database, but is still referenced as a source column in the business entity design:":U.
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(328, 173).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 1.
        THIS-OBJECT:btnOk:Text = "&Ok":U.
        THIS-OBJECT:btnOk:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnOk:UseVisualStyleBackColor = TRUE.
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(409, 173).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 2.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        THIS-OBJECT:btnCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* FieldRemovedPromptForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(496, 208).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "FieldRemovedPromptForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Field removed from Database":U.
        THIS-OBJECT:WindowPositionRegistryKey = "FieldRemovedPromptForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the FieldRemovedPromptForm class
        Notes:   
    ------------------------------------------------------------------------------*/     
    DESTRUCTOR PUBLIC FieldRemovedPromptForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
