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
    File        : LaunchWebViewCallParameterControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Sat Oct 31 18:56:56 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                           FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.*       FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                FROM PROPATH .
USING Consultingwerk.Util.*                                FROM PROPATH .  
USING Consultingwerk.Web2.SmartFramework.Menu.*            FROM PROPATH .
USING Consultingwerk.Windows.Framework.*                   FROM PROPATH . 
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .  
USING Infragistics.Win.UltraWinEditors.*                   FROM ASSEMBLY .
USING Progress.Lang.*                                      FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Menu.LaunchWebViewCallParameterControl 
    INHERITS CallParameterBaseControl: 
	
    DEFINE PRIVATE VARIABLE coEntityView AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE coEntityTable AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE groupBox1 AS System.Windows.Forms.GroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE tbViewTitle AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbParameter AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbEntityName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE coViewUri AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    DEFINE PRIVATE VARIABLE oFormStartTypeValueList AS Infragistics.Win.ValueList NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LaunchWebViewCallParameterControl class                                                              
        Notes:      
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC LaunchWebViewCallParameterControl ():
        SUPER ().
        InitializeComponent ().
        
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
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbEntityName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:tbViewTitle = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:coEntityTable = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:coEntityView = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbParameter = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:coViewUri = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:groupBox1 = NEW System.Windows.Forms.GroupBox().
        CAST(THIS-OBJECT:tbEntityName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbViewTitle, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coEntityTable, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coEntityView, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbParameter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:coViewUri, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:groupBox1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel3 */
        /*  */
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(3, 33).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel3:TabIndex = 2.
        THIS-OBJECT:ultraLabel3:Text = "Parameter:":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 0.
        THIS-OBJECT:ultraLabel2:Text = "View URI:":U.
        /*  */
        /* ultraLabel4 */
        /*  */
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(6, 19).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 0.
        THIS-OBJECT:ultraLabel4:Text = "Business Entity Name:":U.
        /*  */
        /* tbEntityName */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbEntityName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbEntityName:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbEntityName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbEntityName:Location = NEW System.Drawing.Point(184, 15).
        THIS-OBJECT:tbEntityName:Name = "tbEntityName":U.
        THIS-OBJECT:tbEntityName:Size = NEW System.Drawing.Size(608, 21).
        THIS-OBJECT:tbEntityName:TabIndex = 1.
        THIS-OBJECT:tbEntityName:EditorButtonClick:Subscribe(THIS-OBJECT:tbEntityName_EditorButtonClick).
        THIS-OBJECT:tbEntityName:KeyDown:Subscribe(THIS-OBJECT:tbEntityName_KeyDown).
        THIS-OBJECT:tbEntityName:Leave:Subscribe(THIS-OBJECT:tbEntityName_Leave).
        /*  */
        /* tbViewTitle */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewTitle:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbViewTitle:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbViewTitle:Location = NEW System.Drawing.Point(183, 96).
        THIS-OBJECT:tbViewTitle:Name = "tbViewTitle":U.
        THIS-OBJECT:tbViewTitle:Size = NEW System.Drawing.Size(609, 21).
        THIS-OBJECT:tbViewTitle:TabIndex = 7.
        /*  */
        /* ultraLabel6 */
        /*  */
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(6, 100).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 6.
        THIS-OBJECT:ultraLabel6:Text = "View Title:":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(6, 46).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 2.
        THIS-OBJECT:ultraLabel5:Text = "Business Entity View:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(6, 73).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(154, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 4.
        THIS-OBJECT:ultraLabel1:Text = "Business Entity Table:":U.
        /*  */
        /* coEntityTable */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coEntityTable:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coEntityTable:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:coEntityTable:Location = NEW System.Drawing.Point(183, 69).
        THIS-OBJECT:coEntityTable:Name = "coEntityTable":U.
        THIS-OBJECT:coEntityTable:Size = NEW System.Drawing.Size(609, 21).
        THIS-OBJECT:coEntityTable:TabIndex = 5.
        /*  */
        /* coEntityView */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coEntityView:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coEntityView:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:coEntityView:Location = NEW System.Drawing.Point(183, 42).
        THIS-OBJECT:coEntityView:Name = "coEntityView":U.
        THIS-OBJECT:coEntityView:Size = NEW System.Drawing.Size(609, 21).
        THIS-OBJECT:coEntityView:TabIndex = 3.
        /*  */
        /* tbParameter */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbParameter:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbParameter:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        THIS-OBJECT:tbParameter:Location = NEW System.Drawing.Point(186, 30).
        THIS-OBJECT:tbParameter:Multiline = TRUE.
        THIS-OBJECT:tbParameter:Name = "tbParameter":U.
        THIS-OBJECT:tbParameter:Size = NEW System.Drawing.Size(615, 74).
        THIS-OBJECT:tbParameter:TabIndex = 3.
        /*  */
        /* coViewUri */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coViewUri:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:coViewUri:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2010.
        valueListItem1:DataValue = "Consultingwerk/Web2/entityview.html":U.
        valueListItem1:DisplayText = "Consultingwerk/Web2/entityview.html":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.ValueListItem EXTENT 1 NO-UNDO.
        arrayvar0[1] = valueListItem1.
        THIS-OBJECT:coViewUri:Items:AddRange(arrayvar0).
        THIS-OBJECT:coViewUri:Location = NEW System.Drawing.Point(186, 3).
        THIS-OBJECT:coViewUri:Name = "coViewUri":U.
        THIS-OBJECT:coViewUri:Size = NEW System.Drawing.Size(615, 21).
        THIS-OBJECT:coViewUri:TabIndex = 1.
        /*  */
        /* groupBox1 */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:groupBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:tbViewTitle).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:tbEntityName).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:coEntityTable).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:coEntityView).
        THIS-OBJECT:groupBox1:Location = NEW System.Drawing.Point(3, 110).
        THIS-OBJECT:groupBox1:Name = "groupBox1":U.
        THIS-OBJECT:groupBox1:Size = NEW System.Drawing.Size(798, 127).
        THIS-OBJECT:groupBox1:TabIndex = 4.
        THIS-OBJECT:groupBox1:TabStop = FALSE.
        THIS-OBJECT:groupBox1:Text = "Simple Business Entity View":U.
        THIS-OBJECT:groupBox1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* LaunchWebViewCallParameterControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:BackColor = System.Drawing.SystemColors:Control.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:groupBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:coViewUri).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:tbParameter).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Name = "LaunchWebViewCallParameterControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(804, 251).
        CAST(THIS-OBJECT:tbEntityName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbViewTitle, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coEntityTable, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coEntityView, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbParameter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:coViewUri, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:groupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:groupBox1:PerformLayout().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Clear all editor fields
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID InternalClearDataFields ():
        
        ASSIGN
            coViewUri:Text     = "":U
            tbParameter:Text   = "":U
            tbEntityName:Text  = "":U
            coEntityTable:Text = "":U
            coEntityView:Text  = "":U
            tbViewTitle:Text   = "":U
            .

        coEntityTable:ValueList:ValueListItems:Clear () .
        coEntityView:ValueList:ValueListItems:Clear () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Open a file dialog to select a BusinessEntity
        Notes:
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbEntityName_EditorButtonClick (sender AS System.Object, 
	                                                    e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
        DEFINE VARIABLE oOpenFileDialog AS System.Windows.Forms.OpenFileDialog NO-UNDO .
        DEFINE VARIABLE oDialogResult   AS System.Windows.Forms.DialogResult   NO-UNDO . 
        DEFINE VARIABLE oClass          AS Progress.Lang.Class                 NO-UNDO .
        
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
        
        oClass = ClassHelper:FileNameToClass (oOpenFileDialog:FileName) .
        
        IF ClassHelper:FileNameToClass (oOpenFileDialog:FileName) = ? THEN
            RETURN.
        
        ASSIGN
            tbEntityName:Text = oClass:TypeName .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown handler of the Text boxes with an 
                 EditorButton (Lookup)
        Notes:   Maps F3 to EditorButtonClick
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbEntityName_KeyDown (sender AS System.Object, 
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
        Purpose: Event handler for the Leave event of the tbEntityName
        Notes:   
        @param sender The reference to te object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbEntityName_Leave (sender AS System.Object, 
	                                        e AS System.EventArgs):
		
		DEFINE VARIABLE oParameter AS GetBusinessEntityDescriptorParameter NO-UNDO . 
		DEFINE VARIABLE hDataset   AS HANDLE                               NO-UNDO .
		DEFINE VARIABLE hBuffer    AS HANDLE                               NO-UNDO .
		DEFINE VARIABLE i          AS INTEGER                              NO-UNDO .
		
		coEntityView:ValueList:ValueListItems:Clear () .
        coEntityTable:ValueList:ValueListItems:Clear () .
		
		IF tbEntityName:Text > "":U THEN DO:
		
            oParameter = NEW GetBusinessEntityDescriptorParameter () .
            oParameter:ReturnDataset = TRUE .  
         
            FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                           tbEntityName:Text,
                                                           "GetBusinessEntityDescriptor":U,
                                                           INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                           oParameter) .

            IF VALID-OBJECT (oParameter:Descriptor) THEN DO:
                {Consultingwerk/foreachABL.i BusinessEntityViewDescriptor oView in oParameter:Descriptor:Views}
                    coEntityView:ValueList:ValueListItems:Add (oView:Name) .
                END.
		    END.
		    
		    DO i = 1 TO hDataset:NUM-BUFFERS:
		        ASSIGN hBuffer = hDataset:GET-BUFFER-HANDLE (i) .
		        coEntityTable:ValueList:ValueListItems:Add (hBuffer:NAME) .
		    END.
		    
		    coEntityView:DropDown () .
		END.
		
		CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset . 
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID UpdateUiData ():
        
        DEFINE VARIABLE oCallParameter AS LaunchWebViewCallParameter NO-UNDO.
    
        IF NOT TYPE-OF (CallParameterObject, LaunchWebViewCallParameter) THEN 
            RETURN.
            
        oCallParameter = CAST (CallParameterObject, LaunchWebViewCallParameter).
        
        ASSIGN
            coViewUri:Text     = oCallParameter:ViewUri
            tbParameter:Text   = oCallParameter:Parameter
            tbEntityName:Text  = oCallParameter:EntityName
            coEntityTable:Text = oCallParameter:EntityTable
            coEntityView:Text  = oCallParameter:EntityView
            tbViewTitle:Text   = oCallParameter:ViewTitle
            .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID WritebackCallParameterToTextProperty ():
        
        DEFINE VARIABLE oCallParameter      AS LaunchWebViewCallParameter NO-UNDO .
        DEFINE VARIABLE cFormStartTypeValue AS CHARACTER                  NO-UNDO .
        
        /* Marko Rüterbories, Consultingwerk Ltd. 20.09.2013
           If the CallParameterObject is not valid or the datatype does not match the needs
           the instance gets deleted and a new instance of the correct type is created. (SCL-119)
           
           Invalid cast from Consultingwerk.Windows.Framework.LaunchWebViewCallParameter to 
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF NOT VALID-OBJECT (CallParameterObject) OR 
           NOT TYPE-OF (CallParameterObject, LaunchWebViewCallParameter) THEN DO:  
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).
            CallParameterObject = NEW LaunchWebViewCallParameter ().
        END.

        oCallParameter = CAST (CallParameterObject, LaunchWebViewCallParameter).

        ASSIGN
            oCallParameter:ViewUri    = coViewUri:Text      
            oCallParameter:Parameter  = tbParameter:Text    
            oCallParameter:EntityName = tbEntityName:Text   
            oCallParameter:EntityTable= coEntityTable:Text  
            oCallParameter:EntityView = coEntityView:Text   
            oCallParameter:ViewTitle  = tbViewTitle:Text    
            .
        
        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the LaunchWebViewCallParameterControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC LaunchWebViewCallParameterControl ():

    END DESTRUCTOR .

END CLASS.
