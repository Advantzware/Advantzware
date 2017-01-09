/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : FileLookupControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 28 16:28:26 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Session.*    FROM PROPATH .  
USING Consultingwerk.Util.*               FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Windows.*                  FROM ASSEMBLY .
USING Infragistics.Win.*                  FROM ASSEMBLY.
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Session.FileLookupControl INHERITS UserControl: 

	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FileLookupControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC FileLookupControl ():
		
        SUPER().

        InitializeComponent().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        THIS-OBJECT:FillFileNameCombo () . 
        ultraListView1:Items:Clear () .

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Populates the FileName Combo box 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID FillFileNameCombo ():
		
		THIS-OBJECT:ultraComboEditor1:ValueList:ValueListItems:Clear () . 

        THIS-OBJECT:ultraComboEditor1:ValueList:ValueListItems:Add (".applicationsettings":U) .
        THIS-OBJECT:ultraComboEditor1:ValueList:ValueListItems:Add (".applicationsettings.xml":U) .
        THIS-OBJECT:ultraComboEditor1:ValueList:ValueListItems:Add (".BusinessEntityDesignerSettings.xml":U) .
        THIS-OBJECT:ultraComboEditor1:ValueList:ValueListItems:Add (".classpath":U) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinListView.UltraListViewSubItem EXTENT 1 NO-UNDO.
        arrayvar0[1] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem("abc":U, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewItem1 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem1 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("xyz":U, arrayvar0, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraListViewSubItemColumn1 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn1 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("SubItemColumn 0":U).
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(71, 3).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(167, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 0.
        THIS-OBJECT:ultraComboEditor1:KeyPress:Subscribe(THIS-OBJECT:ultraComboEditor1_KeyPress).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(54, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 1.
        THIS-OBJECT:ultraLabel1:Text = "Filename:":U.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(71, 30).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(167, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 2.
        THIS-OBJECT:ultraButton1:Text = "Search file":U.
        THIS-OBJECT:ultraButton1:Click:Subscribe(THIS-OBJECT:ultraButton1_Click).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(71, 59).
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:ReadOnly = TRUE.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(167, 21).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 3.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(3, 63).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(62, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 4.
        THIS-OBJECT:ultraLabel2:Text = "SEARCH():":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(3, 89).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(48, 14).
        THIS-OBJECT:ultraLabel3:TabIndex = 5.
        THIS-OBJECT:ultraLabel3:Text = "File-Info:":U.
        /*  */
        /* ultraListView1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraListView1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        ultraListViewItem1:Key = "lkjl":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinListView.UltraListViewItem EXTENT 1 NO-UNDO.
        arrayvar1[1] = ultraListViewItem1.
        THIS-OBJECT:ultraListView1:Items:AddRange(arrayvar1).
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(71, 86).
        THIS-OBJECT:ultraListView1:MainColumn:Tag = "Property":U.
        THIS-OBJECT:ultraListView1:MainColumn:Width = 140.
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(167, 56).
        ultraListViewSubItemColumn1:Key = "SubItemColumn 0":U.
        ultraListViewSubItemColumn1:Text = "Value":U.
        ultraListViewSubItemColumn1:Width = 400.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO.
        arrayvar2[1] = ultraListViewSubItemColumn1.
        THIS-OBJECT:ultraListView1:SubItemColumns:AddRange(arrayvar2).
        THIS-OBJECT:ultraListView1:TabIndex = 6.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
        /*  */
        /* FileLookupControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:Name = "FileLookupControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(241, 145).
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Searches for the file with the name entered in the ultraComboEditor1 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID SearchFile ():
		
		DEFINE VARIABLE cFileName   AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE hCall       AS HANDLE            NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER           NO-UNDO .
        DEFINE VARIABLE cProperties AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE cProperty   AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oItem       AS UltraListViewItem NO-UNDO .
        DEFINE VARIABLE cSkip       AS CHARACTER         NO-UNDO 
            INIT "TYPE,HANDLE,INSTANTIATING-PROCEDURE":U.
    		
		ASSIGN cFileName   = ultraComboEditor1:Text 
		       cProperties = LIST-QUERY-ATTRS (FILE-INFO:HANDLE).
		
		ultraTextEditor1:Text = SEARCH (cFileName) . 
		ultraListView1:Items:Clear () .
		
		FILE-INFO:FILE-NAME = cFileName . 
		
		CREATE CALL hCall .  
		hCall:CALL-TYPE = GET-ATTR-CALL-TYPE . 
		hCall:IN-HANDLE = FILE-INFO:HANDLE . 

        DO i = 1 TO NUM-ENTRIES (cProperties):
    		
    		ASSIGN cProperty = ENTRY (i, cProperties) . 
    		
    		IF LOOKUP (cProperty, cSkip) > 0 THEN  
    		    NEXT . 
    		
    		hCall:CALL-NAME = cProperty .
    		
    		hCall:INVOKE () NO-ERROR . 
    		
    		IF ERROR-STATUS:ERROR THEN 
    		    NEXT . 
    		
    		oItem = ultraListView1:Items:Add (ENTRY (i, cProperties)) .
    		
    		oItem:Value = ENTRY (i, cProperties) .
    		
    		IF cProperty MATCHES "*-TIME":U THEN 
    		    oItem:SubItems[0]:Value = STRING (INTEGER (hCall:RETURN-VALUE), "hh:mm:ss":U) .
		    ELSE 
    		    oItem:SubItems[0]:Value = hCall:RETURN-VALUE . 

        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hCall) .		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the ultraButton1 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraButton1_Click (sender AS System.Object, 
	                                        e AS System.EventArgs):
		
		THIS-OBJECT:SearchFile () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyPress event of the ultraComboEditor1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The KeyPressEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraComboEditor1_KeyPress (sender AS System.Object,   
	                                                e AS System.Windows.Forms.KeyPressEventArgs):
		
		IF e:KeyChar = CHR(13) THEN 
		    THIS-OBJECT:SearchFile () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the FileLookupControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC FileLookupControl ():

    END DESTRUCTOR .

END CLASS.
