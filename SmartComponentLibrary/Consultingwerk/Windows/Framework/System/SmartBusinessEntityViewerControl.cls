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
    File        : SmartBusinessEntityViewerControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : 17.10.2013 19:18:20
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.System.SmartBusinessEntityViewerControl 
    INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_TrackDeletions_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_TrackDeletions AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_BusinessEntityPackage_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_BusinessEntityPackage AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_BusinessEntityName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_BusinessEntityName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_BusinessEntityPurpose_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_BusinessEntityPurpose AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_DBRequired_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eSmartBusinessEntity_DBRequired AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBusinessEntityViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBusinessEntityViewerControl ():
                
        SUPER().
        InitializeComponent().
        
    END CONSTRUCTOR.
    
	/*------------------------------------------------------------------------------
	    Purpose: Open a file dialog to select a BusinessEntity
	    Notes:
	    @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID eSmartBusinessEntity_BusinessEntityName_EditorButtonClick (sender AS System.Object, 
	                                                                               e      AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs ):
		
		DEFINE VARIABLE oOpenFileDialog AS System.Windows.Forms.OpenFileDialog NO-UNDO .
        DEFINE VARIABLE oDialogResult   AS System.Windows.Forms.DialogResult   NO-UNDO . 
        
        oOpenFileDialog = NEW System.Windows.Forms.OpenFileDialog () .
        oOpenFileDialog:AddExtension = TRUE . 
        oOpenFileDialog:CheckFileExists = TRUE . 
        oOpenFileDialog:CheckPathExists = TRUE . 
        oOpenFileDialog:DefaultExt = "cls":U . 
        oOpenFileDialog:DereferenceLinks = TRUE . 
        oOpenFileDialog:Filter = "Business Entity source files (*.cls)|*.cls|All files (*.*)|*.*"{&TRAN} .
        oOpenFileDialog:Multiselect = FALSE . 
        oOpenFileDialog:RestoreDirectory = TRUE . 
        oOpenFileDialog:Title = "Select a Business Entity"{&TRAN} .         
        
        WAIT-FOR oOpenFileDialog:ShowDialog () SET oDialogResult .
         
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        THIS-OBJECT:SplitBusinessEntity (oOpenFileDialog:FILENAME).
		
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
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eSmartBusinessEntity":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton("btnSelectBusinessEntity":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntity_DBRequired = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_DBRequired, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_TrackDeletions, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityJoin = "YES":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.SmartFramework.System.SmartBusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eSmartBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 6 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityGuid":U, "BusinessEntity Guid":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("BusinessEntityPackage":U, "Package":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "Name":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("BusinessEntityPurpose":U, "Purpose":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("DBRequired":U, "DB Required":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("TrackDeletions":U, "Track Deletions":U, Progress.Data.DataType:LOGICAL).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eSmartBusinessEntity_BusinessEntityPackage_Label */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label:Appearance = appearance1.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label:Location = NEW System.Drawing.Point(20, 50).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label:Name = "eSmartBusinessEntity_BusinessEntityPackage_Label":U.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label:Size = NEW System.Drawing.Size(51, 14).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label:TabIndex = 1.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label:Text = "Package:":U.
        /*  */
        /* eSmartBusinessEntity_BusinessEntityPackage */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BusinessEntityPackage":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage:Location = NEW System.Drawing.Point(130, 46).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage:Name = "eSmartBusinessEntity_BusinessEntityPackage":U.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage:Size = NEW System.Drawing.Size(571, 21).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage:TabIndex = 1.
        /*  */
        /* eSmartBusinessEntity_BusinessEntityName_Label */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label:Appearance = appearance2.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label:Location = NEW System.Drawing.Point(20, 23).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label:Name = "eSmartBusinessEntity_BusinessEntityName_Label":U.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label:Size = NEW System.Drawing.Size(38, 14).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label:TabIndex = 3.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label:Text = "Name:":U.
        /*  */
        /* eSmartBusinessEntity_BusinessEntityName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Key = "btnSelectBusinessEntity":U.
        editorButton1:Text = "...":U.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BusinessEntityName":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:Location = NEW System.Drawing.Point(130, 19).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:Name = "eSmartBusinessEntity_BusinessEntityName":U.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:Size = NEW System.Drawing.Size(571, 21).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:TabIndex = 0.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:EditorButtonClick:Subscribe(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_EditorButtonClick).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* eSmartBusinessEntity_BusinessEntityPurpose_Label */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label:Appearance = appearance3.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label:Location = NEW System.Drawing.Point(20, 103).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label:Name = "eSmartBusinessEntity_BusinessEntityPurpose_Label":U.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label:Size = NEW System.Drawing.Size(50, 14).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label:TabIndex = 5.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label:Text = "Purpose:":U.
        /*  */
        /* eSmartBusinessEntity_BusinessEntityPurpose */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "BusinessEntityPurpose":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:Location = NEW System.Drawing.Point(130, 99).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:Multiline = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:Name = "eSmartBusinessEntity_BusinessEntityPurpose":U.
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:Size = NEW System.Drawing.Size(571, 198).
        THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose:TabIndex = 3.
        /*  */
        /* eSmartBusinessEntity_DBRequired_Label */
        /*  */
        appearance4:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label:Appearance = appearance4.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label:Location = NEW System.Drawing.Point(20, 77).
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label:Name = "eSmartBusinessEntity_DBRequired_Label":U.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label:Size = NEW System.Drawing.Size(72, 14).
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label:TabIndex = 7.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label:Text = "DB Required:":U.
        /*  */
        /* eSmartBusinessEntity_DBRequired */
        /*  */
        THIS-OBJECT:eSmartBusinessEntity_DBRequired:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "DBRequired":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntity_DBRequired:Location = NEW System.Drawing.Point(130, 78).
        THIS-OBJECT:eSmartBusinessEntity_DBRequired:Name = "eSmartBusinessEntity_DBRequired":U.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired:Size = NEW System.Drawing.Size(14, 13).
        THIS-OBJECT:eSmartBusinessEntity_DBRequired:TabIndex = 2.
        THIS-OBJECT:eSmartBusinessEntity_DBRequired:UseMnemonics = TRUE.
        /*  */
        /* eSmartBusinessEntity_TrackDeletions_Label */
        /*  */
        appearance5:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label:Appearance = appearance5.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label:Location = NEW System.Drawing.Point(188, 77).
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label:Name = "eSmartBusinessEntity_TrackDeletions_Label":U.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label:Size = NEW System.Drawing.Size(86, 14).
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label:TabIndex = 9.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label:Text = "&Track Deletions:":U.
        /*  */
        /* eSmartBusinessEntity_TrackDeletions */
        /*  */
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions:AutoSize = TRUE.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "TrackDeletions":U, TRUE)).
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions:Location = NEW System.Drawing.Point(280, 77).
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions:Name = "eSmartBusinessEntity_TrackDeletions":U.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions:Size = NEW System.Drawing.Size(14, 13).
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions:TabIndex = 10.
        THIS-OBJECT:eSmartBusinessEntity_TrackDeletions:ThreeState = TRUE.
        /*  */
        /* SmartBusinessEntityViewerControl */
        /*  */
        THIS-OBJECT:AllowDrop = TRUE.
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_DBRequired_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_DBRequired).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_TrackDeletions_Label).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:eSmartBusinessEntity_TrackDeletions).
        THIS-OBJECT:Name = "SmartBusinessEntityViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(722, 314).
        THIS-OBJECT:SmartTableIOState = "NoDataSource":U.
        THIS-OBJECT:DragDrop:Subscribe(THIS-OBJECT:SmartBusinessEntityViewerControl_DragDrop).
        THIS-OBJECT:DragEnter:Subscribe(THIS-OBJECT:SmartBusinessEntityViewerControl_DragEnter).
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPackage, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_BusinessEntityPurpose, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_DBRequired, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eSmartBusinessEntity_TrackDeletions, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an 
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID LookupKeyDownHandler (sender AS System.Object, 
                                              e AS System.Windows.Forms.KeyEventArgs):

        DEFINE VARIABLE cControlName AS CHARACTER NO-UNDO.
        
        IF e:Handled = FALSE AND Progress.Util.EnumHelper:AreEqual (e:KeyCode, SmartLookupSettings:ShortcutKey) 
            AND THIS-OBJECT:Enabled THEN DO:

            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT, 
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName), 
                            sender, 
                            NEW EditorButtonEventArgs (?, ?)) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

    END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Allow to drop a BusinessEntity on the viewer control
	    Notes:
	    @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID SmartBusinessEntityViewerControl_DragDrop (sender AS System.Object, 
	                                                               e      AS System.Windows.Forms.DragEventArgs ):
		
		DEFINE VARIABLE oArray        AS System.Array           NO-UNDO . 
        DEFINE VARIABLE cFileName     AS CHARACTER              NO-UNDO .
        		
		IF e:Data:GetDataPresent (System.Windows.Forms.DataFormats:FileDrop) THEN DO:
            oArray = CAST (e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),
                           System.Array) .
                           
            IF VALID-OBJECT (oArray) THEN DO:
                cFileName = oArray:GetValue (0):ToString () .
                 
                IF cFileName > "":U THEN DO:
                    THIS-OBJECT:SplitBusinessEntity (cFileName).                  
                END.          
                                             
            END .

        END.
		
	END METHOD.

	/*------------------------------------------------------------------------------
         Purpose: Set the drag & drop effect
         Notes:
         @param sender The reference to the object that raised the event
         @param e The DragEventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID SmartBusinessEntityViewerControl_DragEnter (sender AS System.Object, 
	                                                                e      AS System.Windows.Forms.DragEventArgs ):
		
		/* Sebastian Düngel, Consultingwerk Ltd. 31.10.2013
           The viewer Control is not disabled */
		IF THIS-OBJECT:eSmartBusinessEntity_BusinessEntityName:Enabled = FALSE THEN DO:
		    ASSIGN e:Effect = System.Windows.Forms.DragDropEffects:NONE.
		    RETURN.
		END. 
		
		IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN
            ASSIGN e:Effect = System.Windows.Forms.DragDropEffects:COPY.
        ELSE
            ASSIGN e:Effect = System.Windows.Forms.DragDropEffects:NONE.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Split the BusinessEntity name in Name and Package
	 Notes:
	 @param cFileName The BusinessEntityName
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SplitBusinessEntity (cFileName AS CHARACTER).
		
		DEFINE VARIABLE cBusinessEntity    AS CHARACTER NO-UNDO.
		DEFINE VARIABLE cPackageName       AS CHARACTER NO-UNDO.
		DEFINE VARIABLE i                  AS INTEGER   NO-UNDO.
		DEFINE VARIABLE lResult            AS LOGICAL   NO-UNDO.
		DEFINE VARIABLE oMessageProvider   AS Consultingwerk.SmartFramework.IMessageProvider NO-UNDO.
		DEFINE VARIABLE oMessage           AS Consultingwerk.SmartFramework.Message NO-UNDO.
		DEFINE VARIABLE oClass             AS Progress.Lang.Class NO-UNDO.
		
		IF ClassHelper:FileNameToClass (cFileName) = ? THEN
		    RETURN.
		
		ASSIGN
		    oClass          = ClassHelper:FileNameToClass (cFileName)
		    cBusinessEntity = oClass:TypeName
		    .
		
        DO i = 1 TO NUM-ENTRIES (cBusinessEntity, ".":U) - 1:
            ASSIGN cPackageName = SUBSTITUTE ("&1.&2":U, cPackageName, ENTRY (i, cBusinessEntity, ".":U)).     
        END. 
		
		ASSIGN 
		    cPackageName     = SUBSTRING (cPackageName, 2)
		    cBusinessEntity  = ENTRY (NUM-ENTRIES (cBusinessEntity, ".":U), cBusinessEntity, ".":U)
		    .
		
		ASSIGN lResult = Consultingwerk.Framework.FrameworkSettings:ServiceAdapter:ValidateEntityName ("":U, SUBSTITUTE ("&1.&2":U, cPackageName, cBusinessEntity)).
		
		IF NOT lResult THEN DO:
		   oMessageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider} .
		   
           IF VALID-OBJECT (oMessageProvider) THEN DO:
		      oMessage = oMessageProvider:GetMessage ("SFR":U, 1).
		      MessageFormHelper:ShowMessage (oMessage).
		   END. 

		END. 
		ELSE
		    ASSIGN eSmartBusinessEntity_BusinessEntityName:TEXT    = cBusinessEntity
                   eSmartBusinessEntity_BusinessEntityPackage:TEXT = cPackageName
                   .
		
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartBusinessEntityViewerControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartBusinessEntityViewerControl ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
