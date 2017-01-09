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
    File        : InsertClientCallableMethodWizard
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 30 14:57:16 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.UI.*      FROM PROPATH .  
USING Consultingwerk.Forms.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*              FROM ASSEMBLY .
USING Progress.Lang.*                                 FROM PROPATH .
USING System.Windows.Forms.*                          FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.UI.InsertClientCallableMethodWizard 
    INHERITS BaseForm: 

    DEFINE PRIVATE VARIABLE btnOk AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.
    DEFINE PRIVATE VARIABLE tbParameterType AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbMethodName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE toUpdateAnnotations AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    /*------------------------------------------------------------------------------
        Purpose: Returns the Method Name  
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY MethodName AS CHARACTER NO-UNDO 
	GET:
		RETURN tbMethodName:Text.
	END GET.
	 
    /*------------------------------------------------------------------------------
        Purpose: Returns the Parameter Type   
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ParameterType AS CHARACTER NO-UNDO 
    GET:
        RETURN tbParameterType:Text.
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the class annotations should be updated 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY UpdateAnnotations AS LOGICAL NO-UNDO 
	GET:
		RETURN toUpdateAnnotations:Checked .
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the InsertClientCallableMethodWizard class  
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC InsertClientCallableMethodWizard ():
		
        SUPER().
        InitializeComponent().
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF
        
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
        DEFINE VARIABLE editorButton1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.
        editorButton1 = NEW Infragistics.Win.UltraWinEditors.EditorButton().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.InsertClientCallableMethodWizard":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:toUpdateAnnotations = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbParameterType = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbMethodName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:btnOk = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:panel1:SuspendLayout().
        CAST(THIS-OBJECT:toUpdateAnnotations, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbParameterType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbMethodName, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:toUpdateAnnotations).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbParameterType).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:tbMethodName).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(478, 150).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* toUpdateAnnotations */
        /*  */
        THIS-OBJECT:toUpdateAnnotations:Location = NEW System.Drawing.Point(149, 120).
        THIS-OBJECT:toUpdateAnnotations:Name = "toUpdateAnnotations":U.
        THIS-OBJECT:toUpdateAnnotations:Size = NEW System.Drawing.Size(318, 20).
        THIS-OBJECT:toUpdateAnnotations:TabIndex = 5.
        THIS-OBJECT:toUpdateAnnotations:Text = "Update Annotations":U.
        THIS-OBJECT:toUpdateAnnotations:CheckedChanged:Subscribe(THIS-OBJECT:toUpdateAnnotations_CheckedChanged).
        /*  */
        /* ultraLabel3 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(149, 87).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(318, 53).
        THIS-OBJECT:ultraLabel3:TabIndex = 4.
        THIS-OBJECT:ultraLabel3:Text = "The Parameter Type needs to be implementing the Consultingwerk.ISerializable Interface":U.
        /*  */
        /* tbParameterType */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbParameterType:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        editorButton1:Text = "...":U.
        THIS-OBJECT:tbParameterType:ButtonsRight:Add(editorButton1).
        THIS-OBJECT:tbParameterType:Location = NEW System.Drawing.Point(149, 60).
        THIS-OBJECT:tbParameterType:Name = "tbParameterType":U.
        THIS-OBJECT:tbParameterType:Size = NEW System.Drawing.Size(318, 21).
        THIS-OBJECT:tbParameterType:TabIndex = 3.
        THIS-OBJECT:tbParameterType:Text = "Consultingwerk.ISerializable":U.
        THIS-OBJECT:tbParameterType:EditorButtonClick:Subscribe(THIS-OBJECT:tbParameterType_EditorButtonClick).
        THIS-OBJECT:tbParameterType:TextChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_TextChanged).
        THIS-OBJECT:tbParameterType:KeyDown:Subscribe(THIS-OBJECT:LookupKeyDownHandler).
        /*  */
        /* ultraLabel2 */
        /*  */
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(12, 64).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "Parameter Type:":U.
        /*  */
        /* tbMethodName */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbMethodName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:tbMethodName:Location = NEW System.Drawing.Point(149, 33).
        THIS-OBJECT:tbMethodName:Name = "tbMethodName":U.
        THIS-OBJECT:tbMethodName:Size = NEW System.Drawing.Size(318, 21).
        THIS-OBJECT:tbMethodName:TabIndex = 1.
        THIS-OBJECT:tbMethodName:TextChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_TextChanged).
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 37).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(131, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Method Name:":U.
        /*  */
        /* btnOk */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOk:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:btnOk:Enabled = FALSE.
        THIS-OBJECT:btnOk:Location = NEW System.Drawing.Point(311, 164).
        THIS-OBJECT:btnOk:Name = "btnOk":U.
        THIS-OBJECT:btnOk:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOk:TabIndex = 1.
        THIS-OBJECT:btnOk:Text = "&OK":U.
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(392, 164).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 2.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* InsertClientCallableMethodWizard */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(479, 199).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "InsertClientCallableMethodWizard":U.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Invokable method wizard":U.
        THIS-OBJECT:WindowPositionRegistryKey = "InsertClientCallableMethodWizard":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        CAST(THIS-OBJECT:toUpdateAnnotations, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbParameterType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbMethodName, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

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

&IF NOT PROVERSION BEGINS "10" &THEN
            ASSIGN cControlName = CAST (sender, System.Windows.Forms.Control):Name .

            DYNAMIC-INVOKE (THIS-OBJECT, 
                            SUBSTITUTE ("&1_EditorButtonClick":U, cControlName), 
                            sender, 
                            NEW EditorButtonEventArgs (?, ?)) .
&ELSE 
            /* Mike Fechner, Consultingwerk Ltd. 17.08.2015
               DYNAMIC-INVOKE seems to crash here on 10.2B */
            CASE sender:
                WHEN tbParameterType 
                    THEN THIS-OBJECT:tbParameterType_EditorButtonClick (sender, NEW EditorButtonEventArgs (?, ?)) .
            END CASE . 
&ENDIF
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .                
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event 
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
	
	    DEFINE VARIABLE cSettings AS CHARACTER NO-UNDO.
		
		SUPER:OnShown (e) . 

        ASSIGN cSettings = THIS-OBJECT:LoadSettings ("UpdateAnnotations":U) .

        IF cSettings > "":U THEN DO ON ERROR UNDO, THROW: 
            ASSIGN toUpdateAnnotations:Checked = DataTypeHelper:ToLogical (cSettings) .
            
            CATCH err AS Progress.Lang.Error:
            	/* ignore */	
            END CATCH. 
        END.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the tbParameterType
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID tbParameterType_EditorButtonClick (sender AS System.Object, 
	                                                       e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):
		
        DEFINE VARIABLE oForm            AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult    AS System.Windows.Forms.DialogResult                               NO-UNDO .

        DEFINE VARIABLE oInfo            AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO . 

        DEFINE VARIABLE lExternalRuntime AS LOGICAL                                                         NO-UNDO .
        DEFINE VARIABLE cShortClassName  AS CHARACTER                                                       NO-UNDO .

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartComponent Library":U,
                                                                                "Scanning Project Folder(s) for Smart Viewer Control."{&TRAN}) .

        ASSIGN lExternalRuntime = Consultingwerk.Util.ClassHelper:UseExternalRuntimeForGetClassNames .  

        Consultingwerk.Util.ClassHelper:UseExternalRuntimeForGetClassNames = TRUE . 

        IF VALID-OBJECT (Consultingwerk.Framework.FrameworkSettings:WaitStateManager) THEN 
            Consultingwerk.Framework.FrameworkSettings:WaitStateManager:SetWaitState ("Searching class names"{&TRAN}) .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath ("Consultingwerk.ISerializable":U, 
                                                                  FALSE, 
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        IF VALID-OBJECT (Consultingwerk.Framework.FrameworkSettings:WaitStateManager) THEN 
            Consultingwerk.Framework.FrameworkSettings:WaitStateManager:ClearWaitState ("Searching class names"{&TRAN}) .

        oInfo:Close () .

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .
        oForm:Text = "Select Parameter Type"{&TRAN} .
        
        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .
        
/*        WAIT-FOR oForm:ShowDialog () SET oDialogResult .*/

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .
        
        IF Progress.Util.EnumHelper:AreEqual(oDialogResult, 
                                             System.Windows.Forms.DialogResult:Ok) THEN DO: 
                                    
            ASSIGN THIS-OBJECT:tbParameterType:Text = oForm:GetSelectedBusinessEntityName () .
        
            IF THIS-OBJECT:tbMethodName:Text = "":U THEN DO:
                ASSIGN cShortClassName = ENTRY (NUM-ENTRIES (THIS-OBJECT:tbParameterType:Text, ".":U), THIS-OBJECT:tbParameterType:Text, ".":U) .

                IF cShortClassName MATCHES "*Parameter":U THEN 
                    ASSIGN THIS-OBJECT:tbMethodName:Text = SUBSTRING (cShortClassName, 1, LENGTH (cShortClassName) - 9) .
                
                THIS-OBJECT:tbMethodName:Focus () . 
            END.    
        END.
        
        FINALLY:
            ASSIGN Consultingwerk.Util.ClassHelper:UseExternalRuntimeForGetClassNames = lExternalRuntime .		
        END FINALLY.
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged event of the toUpdateAnnotations
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID toUpdateAnnotations_CheckedChanged (sender AS System.Object, 
	                                                        e AS System.EventArgs):
		
        THIS-OBJECT:StoreSettings (STRING (toUpdateAnnotations:Checked),
                                   "UpdateAnnotations":U) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the UltraTextEditor Controls 
        Notes:   
        @param sender The reference to the object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraTextEditor1_TextChanged (sender AS System.Object, 
	                                                  e AS System.EventArgs):
		
		THIS-OBJECT:btnOK:Enabled = (THIS-OBJECT:tbMethodName:Text > "":U AND 
		                             THIS-OBJECT:tbParameterType:Text > "":U) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the InsertClientCallableMethodWizard class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC InsertClientCallableMethodWizard ():

    END DESTRUCTOR .

END CLASS.
