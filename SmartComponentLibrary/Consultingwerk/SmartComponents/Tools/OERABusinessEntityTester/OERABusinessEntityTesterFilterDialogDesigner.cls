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
    File        : OERABusinessEntityTesterFilterDialogDesigner
    Purpose     : Filter Dialog for the Business Entity Tester
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Apr 08 11:18:24 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.* FROM PROPATH .
USING Progress.Lang.*        FROM ASSEMBLY .
USING Progress.Windows.Form  FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterFilterDialogDesigner 
    INHERITS BaseForm: 
    
    DEFINE PROTECTED VARIABLE cbRecentItems AS System.Windows.Forms.ComboBox NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PROTECTED VARIABLE button2 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.

    DEFINE PROTECTED VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.
        
    CONSTRUCTOR PUBLIC OERABusinessEntityTesterFilterDialogDesigner ():
        SUPER().
        InitializeComponent().
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterFilterDialogDesigner":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:button2 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:cbRecentItems = NEW System.Windows.Forms.ComboBox().
        THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:label1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:cbRecentItems).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:button2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:panel1:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 172).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(603, 36).
        THIS-OBJECT:panel1:TabIndex = 1.
        /*  */
        /* button2 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:button2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button2:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button2:Location = NEW System.Drawing.Point(435, 6).
        THIS-OBJECT:button2:Name = "button2":U.
        THIS-OBJECT:button2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button2:TabIndex = 0.
        THIS-OBJECT:button2:Text = "&OK":U.
        THIS-OBJECT:button2:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button2:UseVisualStyleBackColor = TRUE.
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(516, 6).
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 1.
        THIS-OBJECT:button1:Text = "&Cancel":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* textBox1 */
        /*  */
        THIS-OBJECT:textBox1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:textBox1:Font = NEW System.Drawing.Font("Courier New":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:textBox1:Multiline = TRUE.
        THIS-OBJECT:textBox1:Name = "textBox1":U.
        THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(603, 172).
        THIS-OBJECT:textBox1:TabIndex = 0.
        /*  */
        /* cbRecentItems */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbRecentItems:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:cbRecentItems:DropDownStyle = System.Windows.Forms.ComboBoxStyle:DropDownList.
        THIS-OBJECT:cbRecentItems:Enabled = FALSE.
        THIS-OBJECT:cbRecentItems:FormattingEnabled = TRUE.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 1 NO-UNDO.
        arrayvar0[1] = "(select from list)":U.
        THIS-OBJECT:cbRecentItems:Items:AddRange(arrayvar0).
        THIS-OBJECT:cbRecentItems:Location = NEW System.Drawing.Point(61, 8).
        THIS-OBJECT:cbRecentItems:MaxDropDownItems = 10.
        THIS-OBJECT:cbRecentItems:Name = "cbRecentItems":U.
        THIS-OBJECT:cbRecentItems:Size = NEW System.Drawing.Size(247, 21).
        THIS-OBJECT:cbRecentItems:TabIndex = 2.
        /*  */
        /* label1 */
        /*  */
        THIS-OBJECT:label1:AutoSize = TRUE.
        THIS-OBJECT:label1:Location = NEW System.Drawing.Point(12, 11).
        THIS-OBJECT:label1:Name = "label1":U.
        THIS-OBJECT:label1:Size = NEW System.Drawing.Size(43, 17).
        THIS-OBJECT:label1:TabIndex = 3.
        THIS-OBJECT:label1:Text = "Recent:":U.
        THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* OERABusinessEntityTesterFilterDialogDesigner */
        /*  */
        THIS-OBJECT:CancelButton = THIS-OBJECT:button1.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(603, 208).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "OERABusinessEntityTesterFilterDialogDesigner":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Filter Dialog":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC OERABusinessEntityTesterFilterDialogDesigner ( ):
        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.
    END DESTRUCTOR.

END CLASS.
