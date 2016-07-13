/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ErrorSessionInfoForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 13 22:17:52 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*       FROM PROPATH.
USING Consultingwerk.Forms.* FROM PROPATH .
USING Progress.Lang.*        FROM PROPATH .
USING System.Windows.Forms.* FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Util.Forms.ErrorSessionInfoForm 
    INHERITS BaseForm: 
	
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE value AS System.Windows.Forms.ColumnHeader NO-UNDO.
    DEFINE PRIVATE VARIABLE property AS System.Windows.Forms.ColumnHeader NO-UNDO.
    DEFINE PRIVATE VARIABLE listView1 AS System.Windows.Forms.ListView NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ErrorSessionInfoForm class                                                                        
        Notes:                                          
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ErrorSessionInfoForm ():
		
        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.ErrorSessionInfoForm":U).
        THIS-OBJECT:listView1 = NEW System.Windows.Forms.ListView().
        THIS-OBJECT:property = NEW System.Windows.Forms.ColumnHeader().
        THIS-OBJECT:value = NEW System.Windows.Forms.ColumnHeader().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* listView1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:listView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ColumnHeader EXTENT 2 NO-UNDO.
        arrayvar0[1] = THIS-OBJECT:property.
        arrayvar0[2] = THIS-OBJECT:value.
        THIS-OBJECT:listView1:Columns:AddRange(arrayvar0).
        THIS-OBJECT:listView1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:listView1:Name = "listView1":U.
        THIS-OBJECT:listView1:Size = NEW System.Drawing.Size(611, 387).
        THIS-OBJECT:listView1:TabIndex = 0.
        THIS-OBJECT:listView1:UseCompatibleStateImageBehavior = FALSE.
        THIS-OBJECT:listView1:View = System.Windows.Forms.View:Details.
        /*  */
        /* property */
        /*  */
        THIS-OBJECT:property:Text = "Property":U.
        THIS-OBJECT:property:Width = 200.
        /*  */
        /* value */
        /*  */
        THIS-OBJECT:value:Text = "Value":U.
        THIS-OBJECT:value:Width = 400.
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:listView1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(635, 413).
        THIS-OBJECT:panel1:TabIndex = 1.
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(548, 419).
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 2.
        THIS-OBJECT:button1:Text = "&Close":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* ErrorSessionInfoForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:button1.
        THIS-OBJECT:CancelButton = THIS-OBJECT:button1.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(635, 454).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "ErrorSessionInfoForm":U.
        THIS-OBJECT:Text = "Error Session-Info":U.
        THIS-OBJECT:WindowPositionRegistryKey = "ErrorSessionInfoForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
  	    Purpose: Sets the Session Info
	    Notes:   This method will display the session info 
	    @param poSessionInfo The SessionInfo to display
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SetSessionInfo (poSessionInfo AS SessionInfo):
		
		DEFINE VARIABLE oItem AS ListViewItem NO-UNDO . 

		listView1:Items:Clear () . 
		
		{Consultingwerk/foreachABL.i NameValuePair oPair in poSessionInfo:Properties}

            oItem = NEW ListViewItem (oPair:Name) .
            oItem:SubItems:Add (oPair:Value) . 

            listView1:Items:Add (oItem) .
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ErrorSessionInfoForm class                                                                        
        Notes:                                          
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC ErrorSessionInfoForm ():

	END DESTRUCTOR.

END CLASS.
