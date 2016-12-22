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
    File        : CreateBusinessEntityWizardForm
    Purpose     : Wizard Form to create a new Business Entity and save it 
                  as a .bedgm file 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 09 17:49:40 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                               FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*                      FROM PROPATH .
USING Consultingwerk.Forms.*                                                FROM PROPATH .
USING Consultingwerk.Framework.*                                            FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                                 FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.CreateBusinessEntityWizard.* FROM PROPATH .  
USING Consultingwerk.Util.*                                                 FROM PROPATH .
USING Progress.Lang.*                                                       FROM PROPATH .
USING System.Windows.Forms.*                                                FROM ASSEMBLY .
USING Consultingwerk.Windows.API.* FROM PROPATH.

CLASS Consultingwerk.Studio.LegacyGuiMigration.CreateBusinessEntityWizard.CreateBusinessEntityWizardForm 
    INHERITS BaseForm: 

    DEFINE PRIVATE VARIABLE btnOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE btnCancel AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE businessEntityViewerControl1 AS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityViewerControl NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE saveFileDialog1 AS System.Windows.Forms.SaveFileDialog NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPanel1 AS Infragistics.Win.Misc.UltraPanel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Raised to allow the caller of the wizard to modify (add/enhance)
                 the Business Entity information
        Notes:   
        @param sender The object that raised the CustomizeBusinessEntity event
        @param e The CustomizeBusinessEntityEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CustomizeBusinessEntity SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                e AS CustomizeBusinessEntityEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Raised when the status of the class has changed
        Notes:   
        @param sender The object that raised the StatusChanged event
        @param e The StatusChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT StatusChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                      e AS StatusChangedEventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets the refrence to the list of Table names to be used for selecting 
                 the name of the Business Entity temp-table
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Tables AS CharacterList NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CreateBusinessEntityWizardForm class  
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CreateBusinessEntityWizardForm ():
		
		
        SUPER().
        InitializeComponent().
        
        
        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        IF BusinessEntityDesignerSettings:CustomServices > "":U THEN 
            ServiceLoader:LoadFromFile (BusinessEntityDesignerSettings:CustomServices, TRUE) . 
                
        THIS-OBJECT:Tables = NEW CharacterList () .                 

        CAST (smartBusinessEntityAdapter1:DatasetController,
              BusinessEntityDatasetController):SuppressQuestionAboutDefaultPackage = TRUE . 

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the Click event of the btnOK
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnOK_Click (sender AS System.Object, 
	                                 e AS System.EventArgs):

        DEFINE VARIABLE oResult          AS DialogResult NO-UNDO . 
        DEFINE VARIABLE cDefaultFileName AS CHARACTER    NO-UNDO .
		
		businessEntityViewerControl1:SaveChanges() .
		
	    THIS-OBJECT:OnCustomizeBusinessEntity (NEW CustomizeBusinessEntityEventArgs (CAST (smartBusinessEntityAdapter1:DatasetController,
                                                                                           BusinessEntityDatasetController):DatasetHandle,
                                                                                     CAST (smartBusinessEntityAdapter1:DatasetController,
                                                                                           BusinessEntityDatasetController))) .	
		
        cDefaultFileName = CAST (smartBusinessEntityAdapter1:DatasetController,
                                 BusinessEntityDatasetController):GetDefaultFileName () . 
                                 
        IF cDefaultFileName > "":U THEN                          
            saveFileDialog1:FileName = cDefaultFileName . 
        ELSE 
            saveFileDialog1:FileName = "":U .
		
        WAIT-FOR saveFileDialog1:ShowDialog () SET oResult . 

        {Consultingwerk/check-dialogresult-ok.i oResult}
		
		
		CAST (smartBusinessEntityAdapter1:DatasetController,
              BusinessEntityDatasetController):Save (saveFileDialog1:FileName) .

        Win32:ShellExecute ("open":U, saveFileDialog1:FileName) .		
		
        THIS-OBJECT:Close () . 		

        CATCH err AS Progress.Lang.Error :
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

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
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Studio.LegacyGuiMigration.CreateBusinessEntityWizard.CreateBusinessEntityWizardForm":U).
        THIS-OBJECT:smartBusinessEntityAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:businessEntityViewerControl1 = NEW Consultingwerk.BusinessEntityDesigner.UI.BusinessEntityViewerControl().
        THIS-OBJECT:btnOK = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:btnCancel = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:saveFileDialog1 = NEW System.Windows.Forms.SaveFileDialog().
        THIS-OBJECT:ultraPanel1 = NEW Infragistics.Win.Misc.UltraPanel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:businessEntityViewerControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraPanel1:ClientArea:SuspendLayout().
        THIS-OBJECT:ultraPanel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* smartBusinessEntityAdapter1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter1:DatasetControllerType = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityTable = "eBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkNavigationSource = ?.
        /*  */
        /* businessEntityViewerControl1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:businessEntityViewerControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:businessEntityViewerControl1:LinkDataSource = THIS-OBJECT:smartBusinessEntityAdapter1.
        THIS-OBJECT:businessEntityViewerControl1:LinkGroupAssignSource = ?.
        THIS-OBJECT:businessEntityViewerControl1:LinkTableIOSource = ?.
        THIS-OBJECT:businessEntityViewerControl1:Location = NEW System.Drawing.Point(0, 49).
        THIS-OBJECT:businessEntityViewerControl1:Name = "businessEntityViewerControl1":U.
        THIS-OBJECT:businessEntityViewerControl1:Size = NEW System.Drawing.Size(610, 312).
        THIS-OBJECT:businessEntityViewerControl1:SmartTableIOState = "FieldsDisabled":U.
        THIS-OBJECT:businessEntityViewerControl1:TabIndex = 0.
        /*  */
        /* btnOK */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnOK:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnOK:Location = NEW System.Drawing.Point(442, 367).
        THIS-OBJECT:btnOK:Name = "btnOK":U.
        THIS-OBJECT:btnOK:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnOK:TabIndex = 1.
        THIS-OBJECT:btnOK:Text = "&OK":U.
        THIS-OBJECT:btnOK:Click:Subscribe(THIS-OBJECT:btnOK_Click).
        /*  */
        /* btnCancel */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnCancel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:btnCancel:Location = NEW System.Drawing.Point(523, 367).
        THIS-OBJECT:btnCancel:Name = "btnCancel":U.
        THIS-OBJECT:btnCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnCancel:TabIndex = 2.
        THIS-OBJECT:btnCancel:Text = "&Cancel":U.
        /*  */
        /* saveFileDialog1 */
        /*  */
        THIS-OBJECT:saveFileDialog1:DefaultExt = "bedgm":U.
        THIS-OBJECT:saveFileDialog1:Filter = "Business Entity Design (*.bedgm)|*.bedgm|All files (*.*)|*.*":U.
        THIS-OBJECT:saveFileDialog1:RestoreDirectory = TRUE.
        THIS-OBJECT:saveFileDialog1:Title = "Save As":U.
        /*  */
        /* ultraPanel1 */
        /*  */
        appearance1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraPanel1:Appearance = appearance1.
        /*  */
        /* ultraPanel1.ClientArea */
        /*  */
        THIS-OBJECT:ultraPanel1:ClientArea:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraPanel1:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:ultraPanel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraPanel1:Name = "ultraPanel1":U.
        THIS-OBJECT:ultraPanel1:Size = NEW System.Drawing.Size(610, 43).
        THIS-OBJECT:ultraPanel1:TabIndex = 3.
        /*  */
        /* ultraLabel1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(12, 3).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(586, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Please update the Business Entity Properties in the Form below and press OK to create a Business Entity.":U.
        /*  */
        /* CreateBusinessEntityWizardForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:btnOK.
        THIS-OBJECT:CancelButton = THIS-OBJECT:btnCancel.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(610, 402).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPanel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:businessEntityViewerControl1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(626, 383).
        THIS-OBJECT:Name = "CreateBusinessEntityWizardForm":U.
        THIS-OBJECT:Text = "Create new Business Entity":U.
        THIS-OBJECT:WindowPositionRegistryKey = "CreateBusinessEntityWizardForm":U.
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:businessEntityViewerControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraPanel1:ClientArea:ResumeLayout(FALSE).
        THIS-OBJECT:ultraPanel1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Raises the CustomizeBusinessEntity Event
        Notes:   
        @param e The CustomizeBusinessEntityEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnCustomizeBusinessEntity (e AS CustomizeBusinessEntityEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "CustominzeBusinessEntity":U) .
     
        THIS-OBJECT:CustomizeBusinessEntity:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the StatusChanged
        Notes:   
        @param e The StatusChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnStatusChanged (e AS StatusChangedEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "StatusChanged":U) .
         
        THIS-OBJECT:StatusChanged:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
        
        DEFINE VARIABLE cTable AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE oList  AS ListNameValuePair NO-UNDO . 
        
        DEFINE VARIABLE oDatasetController AS BusinessEntityDatasetController NO-UNDO . 
        
        SUPER:OnShown (e) .
        
        
        oDatasetController = CAST (smartBusinessEntityAdapter1:DatasetController, BusinessEntityDatasetController) . 

        IF VALID-OBJECT (FrameworkSettings:ServiceContainer:GetService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController":U))) THEN 
            FrameworkSettings:ServiceContainer:RemoveService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController":U)) .

        FrameworkSettings:ServiceContainer:AddService (Progress.Lang.Class:GetClass ("Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController":U),
                                                       oDatasetController) .
        
        oDatasetController:BusinessEntityViewerControl = businessEntityViewerControl1 .
        oDatasetController:New () . 

        oList = NEW ListNameValuePair () .  

        {Consultingwerk/foreachPrimitiveList.i Character cEntry in this-object:Tables}

            IF cTable = "":U THEN 
                ASSIGN cTable = cEntry .
                
            oList:Add (cEntry, cEntry) .
        END.
        
        IF cTable = "":U THEN DO:  
            ASSIGN cTable = "ImportedTable":U .
            
            oList:Add (cTable, cTable) .
        END.
            
        IF oList:Count > 1 THEN DO:             
            IF NOT InputPromptHelper:PromptForCharacterValue ("Select base table for temp-table name"{&TRAN}, 
                                                              "The base table is used for naming the Business Entity Temp-Table"{&TRAN},
                                                              oList,
                                                              INPUT-OUTPUT cTable) THEN 
            DO:
                THIS-OBJECT:Close () .                                                             
                RETURN . 
            END.
        END.

        oDatasetController:CreateTempTable (cTable) .

        businessEntityViewerControl1:EnableFields (EnableFieldsEnum:Update) .

        CATCH err AS Progress.Lang.Error:
        	ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Desstructor for the CreateBusinessEntityWizardForm class  
        Notes:   
    ------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC CreateBusinessEntityWizardForm ():

	END DESTRUCTOR.

END CLASS.
