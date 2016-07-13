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
    File        : BusinessEntityLookupDesignerForm
    Purpose     : Lookup Designer (typically used within Progress Developer
                  Studio) 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 14 14:18:42 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                      FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Consultingwerk.Util.*                 FROM PROPATH.
USING Consultingwerk.Windows.Util.Forms.*   FROM PROPATH.
USING Progress.Lang.*                       FROM PROPATH .
USING Progress.Windows.Form                 FROM ASSEMBLY .
USING Consultingwerk.Framework.Enum.* FROM PROPATH.

CLASS Consultingwerk.SmartComponents.Support.BusinessEntityLookupDesignerForm INHERITS Form: 
    
    DEFINE PRIVATE VARIABLE bsBrowseColumns AS Consultingwerk.SmartComponents.Implementation.SmartBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE bsFieldControls AS Consultingwerk.SmartComponents.Implementation.SmartBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE bsFilter AS Consultingwerk.SmartComponents.Implementation.SmartBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE btnQueryWizard AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonOK AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE buttonCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE cbKeyField AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE cbKeyValueColumn AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE daBrowseColumns AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE daFilter AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE panelBrowseColumns AS Consultingwerk.SmartComponents.Implementation.SmartPanelController NO-UNDO.
    DEFINE PRIVATE VARIABLE panelFieldControls AS Consultingwerk.SmartComponents.Implementation.SmartPanelController NO-UNDO.
    DEFINE PRIVATE VARIABLE daFieldControls AS Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE panelFilter AS Consultingwerk.SmartComponents.Implementation.SmartPanelController NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBoxHeader AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser1 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser2 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE smartUpdatableBrowser3 AS Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLookupQueryString AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLookupDialogTitle AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE tbLookupDialogQueryString AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel4 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel5 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel6 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox2 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGroupBox1 AS Infragistics.Win.Misc.UltraGroupBox NO-UNDO.

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE"}
        
    DEFINE PRIVATE QUERY qClassNames FOR ttClassNames SCROLLING .     
    DEFINE PRIVATE VARIABLE oControlValueList   AS Infragistics.Win.ValueList NO-UNDO .    
    DEFINE PRIVATE VARIABLE oFieldValueList     AS Infragistics.Win.ValueList NO-UNDO . 
    DEFINE PRIVATE VARIABLE oFieldValueList2    AS Infragistics.Win.ValueList NO-UNDO . 
    DEFINE PRIVATE VARIABLE oFieldValueList3    AS Infragistics.Win.ValueList NO-UNDO . 
    DEFINE PRIVATE VARIABLE oFieldValueList4    AS Infragistics.Win.ValueList NO-UNDO . 
    DEFINE PRIVATE VARIABLE oOperatorsValueList AS Infragistics.Win.ValueList NO-UNDO . 

    DEFINE TEMP-TABLE ttFieldControls NO-UNDO 
        FIELD FieldName AS CHARACTER
        FIELD ControlName AS CHARACTER
        INDEX FieldName FieldName .

    DEFINE TEMP-TABLE ttFilter NO-UNDO
        FIELD Order AS INTEGER  
        FIELD FieldName AS CHARACTER
        FIELD Operator AS CHARACTER
        INDEX Order Order .

    DEFINE TEMP-TABLE ttBrowseColumn NO-UNDO
        FIELD Order AS INTEGER  
        FIELD FieldName AS CHARACTER
        INDEX Order Order .
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the browse columns list                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY BrowseColumns AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the controls list                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY ControlList AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the field list                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY FieldList AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of filter fields                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY FilterFields AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of filter operators (currently used)                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY FilterOperators AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the key field name                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY KeyField AS CHARACTER NO-UNDO 
    GET:
        RETURN cbKeyField:Text . 
    END GET.
    SET (INPUT arg AS CHARACTER):
        ASSIGN cbKeyField:Text = arg .         
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the key value field name                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/ 
    DEFINE PUBLIC PROPERTY KeyValueColumn AS CHARACTER NO-UNDO 
    GET:
        RETURN cbKeyValueColumn:Text . 
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN cbKeyValueColumn:Text = arg .      
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the LookupDialogQueryString                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY LookupDialogQueryString AS CHARACTER NO-UNDO 
    GET:
        RETURN tbLookupDialogQueryString:Text . 
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN tbLookupDialogQueryString:Text = arg .      
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the LookupDialogTitle
        Notes:                                                                        
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY LookupDialogTitle AS CHARACTER NO-UNDO 
    GET:
        RETURN tbLookupDialogTitle:Text . 
    END GET.
    SET (arg AS CHARACTER):
        ASSIGN tbLookupDialogTitle:Text = arg .      
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Lookup Entity Table
        Notes:   Required for the QueryString wizard
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY LookupEntityTable AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of field names available to the lookup                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY LookupFields AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the LookupQueryString                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/   
    DEFINE PUBLIC PROPERTY LookupQueryString AS CHARACTER NO-UNDO 
    GET:
        RETURN tbLookupQueryString:Text . 
    END GET.
    SET (INPUT arg AS CHARACTER):
        ASSIGN tbLookupQueryString:Text = arg .      
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of child control field names of the parent
                 (typically SmartViewerControl)                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC PROPERTY ParentViewerControls AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityLookupDesignerForm class                                                               
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    CONSTRUCTOR PUBLIC BusinessEntityLookupDesignerForm ():
        
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnQueryWizard
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnQueryWizard_Click (sender AS System.Object, 
	                                          e AS System.EventArgs):
		
		DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO .
		
		IF CharacterType:IsNullOrEmpty (THIS-OBJECT:cbKeyField:Text) THEN DO:
		    MessageFormHelper:ShowMessage ("Please select a Key Field first!"{&TRAN},
		                                   THIS-OBJECT:Text,
		                                   MessageFormImages:ImageWarning) .
		    
		    RETURN . 
		END.
		
		ASSIGN cQueryString = SUBSTITUTE ("FOR EACH &1 WHERE &1.&2 BEGINS ~"&&1~"":U,
		                                  THIS-OBJECT:LookupEntityTable,
		                                  THIS-OBJECT:cbKeyField:Text) .

		IF MessageFormHelper:AskQuestion (cQueryString,
		                                  THIS-OBJECT:Text) = DialogResultEnum:DialogResultYes THEN 

            THIS-OBJECT:tbLookupQueryString:Text = cQueryString . 		                                      
		

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                               
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    METHOD PRIVATE VOID InitializeComponent(  ):
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Support.BusinessEntityLookupDesignerForm":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand1 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand1 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("Table":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn1 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn1 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn2 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn2 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("ControlName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance6 AS Infragistics.Win.Appearance NO-UNDO.
        appearance6 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance7 AS Infragistics.Win.Appearance NO-UNDO.
        appearance7 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance8 AS Infragistics.Win.Appearance NO-UNDO.
        appearance8 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand2 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand2 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("Table":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn3 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn3 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Order":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn4 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn4 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
        appearance24 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
        appearance25 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
        appearance26 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
        appearance27 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
        appearance28 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc2 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc2 = NEW Progress.Data.TableDesc("Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridBand3 AS Infragistics.Win.UltraWinGrid.UltraGridBand NO-UNDO.
        ultraGridBand3 = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("Table":U, -1).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn5 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn5 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Order":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn13 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn13 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("FieldName":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraGridColumn14 AS Infragistics.Win.UltraWinGrid.UltraGridColumn NO-UNDO.
        ultraGridColumn14 = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn("Operator":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
        appearance30 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
        appearance31 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
        appearance32 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
        appearance33 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
        appearance34 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance35 AS Infragistics.Win.Appearance NO-UNDO.
        appearance35 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance36 AS Infragistics.Win.Appearance NO-UNDO.
        appearance36 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance37 AS Infragistics.Win.Appearance NO-UNDO.
        appearance37 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance38 AS Infragistics.Win.Appearance NO-UNDO.
        appearance38 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance39 AS Infragistics.Win.Appearance NO-UNDO.
        appearance39 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance40 AS Infragistics.Win.Appearance NO-UNDO.
        appearance40 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc3 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc3 = NEW Progress.Data.TableDesc("Table":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance41 AS Infragistics.Win.Appearance NO-UNDO.
        appearance41 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance42 AS Infragistics.Win.Appearance NO-UNDO.
        appearance42 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:buttonCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:buttonOK = NEW System.Windows.Forms.Button().
        THIS-OBJECT:pictureBoxHeader = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraGroupBox1 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbKeyValueColumn = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:tbLookupQueryString = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel5 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:panelFieldControls = NEW Consultingwerk.SmartComponents.Implementation.SmartPanelController().
        THIS-OBJECT:smartUpdatableBrowser1 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bsFieldControls = NEW Consultingwerk.SmartComponents.Implementation.SmartBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:daFieldControls = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:ultraLabel4 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:cbKeyField = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraGroupBox2 = NEW Infragistics.Win.Misc.UltraGroupBox().
        THIS-OBJECT:smartUpdatableBrowser3 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bsBrowseColumns = NEW Consultingwerk.SmartComponents.Implementation.SmartBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:daBrowseColumns = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:panelFilter = NEW Consultingwerk.SmartComponents.Implementation.SmartPanelController().
        THIS-OBJECT:panelBrowseColumns = NEW Consultingwerk.SmartComponents.Implementation.SmartPanelController().
        THIS-OBJECT:smartUpdatableBrowser2 = NEW Consultingwerk.SmartComponents.Implementation.SmartUpdatableBrowser().
        THIS-OBJECT:bsFilter = NEW Consultingwerk.SmartComponents.Implementation.SmartBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:daFilter = NEW Consultingwerk.SmartComponents.Implementation.SmartTempTableAdapter(THIS-OBJECT:components).
        THIS-OBJECT:tbLookupDialogQueryString = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel6 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:tbLookupDialogTitle = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:btnQueryWizard = NEW Infragistics.Win.Misc.UltraButton().
        CAST(THIS-OBJECT:pictureBoxHeader, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox1:SuspendLayout().
        CAST(THIS-OBJECT:cbKeyValueColumn, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbLookupQueryString, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bsFieldControls, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:daFieldControls, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:cbKeyField, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraGroupBox2:SuspendLayout().
        CAST(THIS-OBJECT:smartUpdatableBrowser3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bsBrowseColumns, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:daBrowseColumns, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:bsFilter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:daFilter, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbLookupDialogQueryString, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:tbLookupDialogTitle, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* buttonCancel */
        /*  */
        THIS-OBJECT:buttonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:buttonCancel:Location = NEW System.Drawing.Point(718, 557).
        THIS-OBJECT:buttonCancel:Name = "buttonCancel":U.
        THIS-OBJECT:buttonCancel:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:buttonCancel:TabIndex = 5.
        THIS-OBJECT:buttonCancel:Text = "&Cancel":U.
        THIS-OBJECT:buttonCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:buttonCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* buttonOK */
        /*  */
        THIS-OBJECT:buttonOK:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:buttonOK:Location = NEW System.Drawing.Point(637, 557).
        THIS-OBJECT:buttonOK:Name = "buttonOK":U.
        THIS-OBJECT:buttonOK:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:buttonOK:TabIndex = 4.
        THIS-OBJECT:buttonOK:Text = "&OK":U.
        THIS-OBJECT:buttonOK:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:buttonOK:UseVisualStyleBackColor = TRUE.
        /*  */
        /* pictureBoxHeader */
        /*  */
        THIS-OBJECT:pictureBoxHeader:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:pictureBoxHeader:Image = CAST(resources:GetObject("pictureBoxHeader.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBoxHeader:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:pictureBoxHeader:Name = "pictureBoxHeader":U.
        THIS-OBJECT:pictureBoxHeader:Size = NEW System.Drawing.Size(805, 61).
        THIS-OBJECT:pictureBoxHeader:TabIndex = 3.
        THIS-OBJECT:pictureBoxHeader:TabStop = FALSE.
        /*  */
        /* ultraGroupBox1 */
        /*  */
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:btnQueryWizard).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:cbKeyValueColumn).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:tbLookupQueryString).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel5).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:panelFieldControls).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:smartUpdatableBrowser1).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:ultraLabel4).
        THIS-OBJECT:ultraGroupBox1:Controls:Add(THIS-OBJECT:cbKeyField).
        THIS-OBJECT:ultraGroupBox1:Location = NEW System.Drawing.Point(12, 67).
        THIS-OBJECT:ultraGroupBox1:Name = "ultraGroupBox1":U.
        THIS-OBJECT:ultraGroupBox1:Size = NEW System.Drawing.Size(398, 484).
        THIS-OBJECT:ultraGroupBox1:TabIndex = 6.
        THIS-OBJECT:ultraGroupBox1:Text = "Lookup":U.
        THIS-OBJECT:ultraGroupBox1:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2007.
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance2:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance2.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(6, 52).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(115, 23).
        THIS-OBJECT:ultraLabel2:TabIndex = 8.
        THIS-OBJECT:ultraLabel2:Text = "Key Value Column:":U.
        /*  */
        /* cbKeyValueColumn */
        /*  */
        THIS-OBJECT:cbKeyValueColumn:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:cbKeyValueColumn:Location = NEW System.Drawing.Point(127, 48).
        THIS-OBJECT:cbKeyValueColumn:Name = "cbKeyValueColumn":U.
        THIS-OBJECT:cbKeyValueColumn:Size = NEW System.Drawing.Size(252, 21).
        THIS-OBJECT:cbKeyValueColumn:TabIndex = 7.
        /*  */
        /* tbLookupQueryString */
        /*  */
        THIS-OBJECT:tbLookupQueryString:Location = NEW System.Drawing.Point(6, 398).
        THIS-OBJECT:tbLookupQueryString:Multiline = TRUE.
        THIS-OBJECT:tbLookupQueryString:Name = "tbLookupQueryString":U.
        THIS-OBJECT:tbLookupQueryString:Size = NEW System.Drawing.Size(386, 80).
        THIS-OBJECT:tbLookupQueryString:TabIndex = 6.
        THIS-OBJECT:tbLookupQueryString:Text = "ultraTextEditor2":U.
        /*  */
        /* ultraLabel5 */
        /*  */
        appearance3:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel5:Appearance = appearance3.
        THIS-OBJECT:ultraLabel5:Location = NEW System.Drawing.Point(6, 384).
        THIS-OBJECT:ultraLabel5:Name = "ultraLabel5":U.
        THIS-OBJECT:ultraLabel5:Size = NEW System.Drawing.Size(386, 23).
        THIS-OBJECT:ultraLabel5:TabIndex = 5.
        THIS-OBJECT:ultraLabel5:Text = "Lookup Query-String:":U.
        /*  */
        /* panelFieldControls */
        /*  */
        THIS-OBJECT:panelFieldControls:Location = NEW System.Drawing.Point(6, 81).
        THIS-OBJECT:panelFieldControls:MaximumSize = NEW System.Drawing.Size(5000, 27).
        THIS-OBJECT:panelFieldControls:MinimumSize = NEW System.Drawing.Size(100, 27).
        THIS-OBJECT:panelFieldControls:Name = "panelFieldControls":U.
        THIS-OBJECT:panelFieldControls:Size = NEW System.Drawing.Size(386, 27).
        THIS-OBJECT:panelFieldControls:TabIndex = 4.
        THIS-OBJECT:panelFieldControls:VisibleNavigationTools = "":U.
        /*  */
        /* smartUpdatableBrowser1 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser1:DataSource = THIS-OBJECT:bsFieldControls.
        appearance4:BackColor = System.Drawing.SystemColors:Window.
        appearance4:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Appearance = appearance4.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
        ultraGridColumn1:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn1:Header:Caption = "Mapped Field":U.
        ultraGridColumn1:Header:VisiblePosition = 0.
        ultraGridColumn1:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDown.
        ultraGridColumn1:Width = 184.
        ultraGridColumn2:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn2:Header:Caption = "Mapped Control":U.
        ultraGridColumn2:Header:VisiblePosition = 1.
        ultraGridColumn2:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDown.
        ultraGridColumn2:Width = 200.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Object EXTENT 2 NO-UNDO.
        arrayvar0[1] = ultraGridColumn1.
        arrayvar0[2] = ultraGridColumn2.
        ultraGridBand1:Columns:AddRange(arrayvar0).
        ultraGridBand1:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand1:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand1:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BandsSerializer:Add(ultraGridBand1).
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance5:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance5:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance5:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:Appearance = appearance5.
        appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance6.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance7:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance7:BackColor2 = System.Drawing.SystemColors:Control.
        appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:GroupByBox:PromptAppearance = appearance7.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:MaxRowScrollRegions = 1.
        appearance8:BackColor = System.Drawing.SystemColors:Window.
        appearance8:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveCellAppearance = appearance8.
        appearance9:BackColor = System.Drawing.SystemColors:Highlight.
        appearance9:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:ActiveRowAppearance = appearance9.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance10:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CardAreaAppearance = appearance10.
        appearance11:BorderColor = System.Drawing.Color:Silver.
        appearance11:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellAppearance = appearance11.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:CellPadding = 0.
        appearance12:BackColor = System.Drawing.SystemColors:Control.
        appearance12:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance12:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance12:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance12:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:GroupByRowAppearance = appearance12.
        appearance13:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderAppearance = appearance13.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance14:BackColor = System.Drawing.SystemColors:Window.
        appearance14:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowAppearance = appearance14.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance15:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:Override:TemplateAddRowAppearance = appearance15.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser1:LinkDataSource = THIS-OBJECT:daFieldControls.
        THIS-OBJECT:smartUpdatableBrowser1:LinkTableIOSource = THIS-OBJECT:panelFieldControls.
        THIS-OBJECT:smartUpdatableBrowser1:Location = NEW System.Drawing.Point(6, 107).
        THIS-OBJECT:smartUpdatableBrowser1:Name = "smartUpdatableBrowser1":U.
        THIS-OBJECT:smartUpdatableBrowser1:SettingsKey = "ffa069ee-be14-74ad-e011-213d2dbab4e6":U.
        THIS-OBJECT:smartUpdatableBrowser1:Size = NEW System.Drawing.Size(386, 271).
        THIS-OBJECT:smartUpdatableBrowser1:TabIndex = 3.
        THIS-OBJECT:smartUpdatableBrowser1:Text = "smartUpdatableBrowser1":U.
        /*  */
        /* bsFieldControls */
        /*  */
        THIS-OBJECT:bsFieldControls:MaxDataGuess = 0.
        THIS-OBJECT:bsFieldControls:NoLOBs = FALSE.
        THIS-OBJECT:bsFieldControls:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar1 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar1.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Progress.Data.ColumnPropDesc EXTENT 2 NO-UNDO.
        arrayvar2[1] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "FieldName":U, Progress.Data.DataType:CHARACTER).
        arrayvar2[2] = NEW Progress.Data.ColumnPropDesc("ControlName":U, "Control":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar2.
        THIS-OBJECT:bsFieldControls:TableSchema = tableDesc1.
        /*  */
        /* daFieldControls */
        /*  */
        THIS-OBJECT:daFieldControls:LinkDataSource = ?.
        THIS-OBJECT:daFieldControls:LinkNavigationSource = THIS-OBJECT:panelFieldControls.
        /*  */
        /* ultraLabel4 */
        /*  */
        appearance16:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel4:Appearance = appearance16.
        THIS-OBJECT:ultraLabel4:Location = NEW System.Drawing.Point(6, 25).
        THIS-OBJECT:ultraLabel4:Name = "ultraLabel4":U.
        THIS-OBJECT:ultraLabel4:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel4:TabIndex = 2.
        THIS-OBJECT:ultraLabel4:Text = "Key Field:":U.
        /*  */
        /* cbKeyField */
        /*  */
        THIS-OBJECT:cbKeyField:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:cbKeyField:Location = NEW System.Drawing.Point(127, 21).
        THIS-OBJECT:cbKeyField:Name = "cbKeyField":U.
        THIS-OBJECT:cbKeyField:Size = NEW System.Drawing.Size(252, 21).
        THIS-OBJECT:cbKeyField:TabIndex = 0.
        /*  */
        /* ultraGroupBox2 */
        /*  */
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:smartUpdatableBrowser3).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:panelBrowseColumns).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:panelFilter).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:smartUpdatableBrowser2).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:tbLookupDialogQueryString).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:ultraLabel6).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraGroupBox2:Controls:Add(THIS-OBJECT:tbLookupDialogTitle).
        THIS-OBJECT:ultraGroupBox2:Location = NEW System.Drawing.Point(416, 67).
        THIS-OBJECT:ultraGroupBox2:Name = "ultraGroupBox2":U.
        THIS-OBJECT:ultraGroupBox2:Size = NEW System.Drawing.Size(377, 484).
        THIS-OBJECT:ultraGroupBox2:TabIndex = 7.
        THIS-OBJECT:ultraGroupBox2:Text = "Lookup Dialog":U.
        THIS-OBJECT:ultraGroupBox2:ViewStyle = Infragistics.Win.Misc.GroupBoxViewStyle:Office2007.
        /*  */
        /* smartUpdatableBrowser3 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser3:DataSource = THIS-OBJECT:bsBrowseColumns.
        appearance17:BackColor = System.Drawing.SystemColors:Window.
        appearance17:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Appearance = appearance17.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn.
        ultraGridColumn3:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn3:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn3:Header:VisiblePosition = 0.
        ultraGridColumn3:Width = 80.
        ultraGridColumn4:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn4:Header:Caption = "Browse Column":U.
        ultraGridColumn4:Header:VisiblePosition = 1.
        ultraGridColumn4:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDown.
        ultraGridColumn4:Width = 229.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS System.Object EXTENT 2 NO-UNDO.
        arrayvar3[1] = ultraGridColumn3.
        arrayvar3[2] = ultraGridColumn4.
        ultraGridBand2:Columns:AddRange(arrayvar3).
        ultraGridBand2:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand2:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand2:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:BandsSerializer:Add(ultraGridBand2).
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance18:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance18:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance18:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:GroupByBox:Appearance = appearance18.
        appearance19:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance19.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance20:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance20:BackColor2 = System.Drawing.SystemColors:Control.
        appearance20:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance20:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:GroupByBox:PromptAppearance = appearance20.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:MaxRowScrollRegions = 1.
        appearance21:BackColor = System.Drawing.SystemColors:Window.
        appearance21:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:ActiveCellAppearance = appearance21.
        appearance22:BackColor = System.Drawing.SystemColors:Highlight.
        appearance22:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:ActiveRowAppearance = appearance22.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance23:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:CardAreaAppearance = appearance23.
        appearance24:BorderColor = System.Drawing.Color:Silver.
        appearance24:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:CellAppearance = appearance24.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:CellPadding = 0.
        appearance25:BackColor = System.Drawing.SystemColors:Control.
        appearance25:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance25:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance25:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance25:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:GroupByRowAppearance = appearance25.
        appearance26:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:HeaderAppearance = appearance26.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance27:BackColor = System.Drawing.SystemColors:Window.
        appearance27:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:RowAppearance = appearance27.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance28:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:Override:TemplateAddRowAppearance = appearance28.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser3:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser3:LinkDataSource = THIS-OBJECT:daBrowseColumns.
        THIS-OBJECT:smartUpdatableBrowser3:LinkTableIOSource = THIS-OBJECT:panelBrowseColumns.
        THIS-OBJECT:smartUpdatableBrowser3:Location = NEW System.Drawing.Point(6, 241).
        THIS-OBJECT:smartUpdatableBrowser3:Name = "smartUpdatableBrowser3":U.
        THIS-OBJECT:smartUpdatableBrowser3:SettingsKey = "ffa069ee-be14-74ad-e011-213dc0ab93e7":U.
        THIS-OBJECT:smartUpdatableBrowser3:Size = NEW System.Drawing.Size(365, 137).
        THIS-OBJECT:smartUpdatableBrowser3:TabIndex = 15.
        THIS-OBJECT:smartUpdatableBrowser3:Text = "smartUpdatableBrowser3":U.
        /*  */
        /* bsBrowseColumns */
        /*  */
        THIS-OBJECT:bsBrowseColumns:MaxDataGuess = 0.
        THIS-OBJECT:bsBrowseColumns:NoLOBs = FALSE.
        THIS-OBJECT:bsBrowseColumns:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar4 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc2:ChildTables = arrayvar4.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar5 AS Progress.Data.ColumnPropDesc EXTENT 2 NO-UNDO.
        arrayvar5[1] = NEW Progress.Data.ColumnPropDesc("Order":U, "Order":U, Progress.Data.DataType:INTEGER).
        arrayvar5[2] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "FieldName":U, Progress.Data.DataType:CHARACTER).
        tableDesc2:Columns = arrayvar5.
        THIS-OBJECT:bsBrowseColumns:TableSchema = tableDesc2.
        /*  */
        /* daBrowseColumns */
        /*  */
        THIS-OBJECT:daBrowseColumns:LinkDataSource = ?.
        THIS-OBJECT:daBrowseColumns:LinkNavigationSource = THIS-OBJECT:panelFilter.
        /*  */
        /* panelFilter */
        /*  */
        THIS-OBJECT:panelFilter:Location = NEW System.Drawing.Point(6, 54).
        THIS-OBJECT:panelFilter:MaximumSize = NEW System.Drawing.Size(5000, 27).
        THIS-OBJECT:panelFilter:MinimumSize = NEW System.Drawing.Size(100, 27).
        THIS-OBJECT:panelFilter:Name = "panelFilter":U.
        THIS-OBJECT:panelFilter:ShowNavigationTools = FALSE.
        THIS-OBJECT:panelFilter:Size = NEW System.Drawing.Size(365, 27).
        THIS-OBJECT:panelFilter:TabIndex = 13.
        /*  */
        /* panelBrowseColumns */
        /*  */
        THIS-OBJECT:panelBrowseColumns:Location = NEW System.Drawing.Point(6, 215).
        THIS-OBJECT:panelBrowseColumns:MaximumSize = NEW System.Drawing.Size(5000, 27).
        THIS-OBJECT:panelBrowseColumns:MinimumSize = NEW System.Drawing.Size(100, 27).
        THIS-OBJECT:panelBrowseColumns:Name = "panelBrowseColumns":U.
        THIS-OBJECT:panelBrowseColumns:ShowNavigationTools = FALSE.
        THIS-OBJECT:panelBrowseColumns:Size = NEW System.Drawing.Size(365, 27).
        THIS-OBJECT:panelBrowseColumns:TabIndex = 14.
        /*  */
        /* smartUpdatableBrowser2 */
        /*  */
        THIS-OBJECT:smartUpdatableBrowser2:DataSource = THIS-OBJECT:bsFilter.
        appearance29:BackColor = System.Drawing.SystemColors:Window.
        appearance29:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Appearance = appearance29.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
        ultraGridColumn5:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn5:FilterOperatorDefaultValue = Infragistics.Win.UltraWinGrid.FilterOperatorDefaultValue:GreaterThanOrEqualTo.
        ultraGridColumn5:Header:VisiblePosition = 0.
        ultraGridColumn5:Width = 80.
        ultraGridColumn13:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn13:Header:Caption = "Field Name":U.
        ultraGridColumn13:Header:VisiblePosition = 1.
        ultraGridColumn13:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDown.
        ultraGridColumn13:Width = 142.
        ultraGridColumn14:CellActivation = Infragistics.Win.UltraWinGrid.Activation:NoEdit.
        ultraGridColumn14:Header:VisiblePosition = 2.
        ultraGridColumn14:Style = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDown.
        ultraGridColumn14:Width = 141.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar6 AS System.Object EXTENT 3 NO-UNDO.
        arrayvar6[1] = ultraGridColumn5.
        arrayvar6[2] = ultraGridColumn13.
        arrayvar6[3] = ultraGridColumn14.
        ultraGridBand3:Columns:AddRange(arrayvar6).
        ultraGridBand3:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        ultraGridBand3:Override:SelectTypeCell = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand3:Override:SelectTypeCol = Infragistics.Win.UltraWinGrid.SelectType:None.
        ultraGridBand3:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:BandsSerializer:Add(ultraGridBand3).
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        appearance30:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance30:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance30:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance30:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:GroupByBox:Appearance = appearance30.
        appearance31:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance31.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        appearance32:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance32:BackColor2 = System.Drawing.SystemColors:Control.
        appearance32:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance32:ForeColor = System.Drawing.SystemColors:GrayText.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:GroupByBox:PromptAppearance = appearance32.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:MaxRowScrollRegions = 1.
        appearance33:BackColor = System.Drawing.SystemColors:Window.
        appearance33:ForeColor = System.Drawing.SystemColors:ControlText.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:ActiveCellAppearance = appearance33.
        appearance34:BackColor = System.Drawing.SystemColors:Highlight.
        appearance34:ForeColor = System.Drawing.SystemColors:HighlightText.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:ActiveRowAppearance = appearance34.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:AllowAddNew = Infragistics.Win.UltraWinGrid.AllowAddNew:No.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:AllowDelete = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        appearance35:BackColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:CardAreaAppearance = appearance35.
        appearance36:BorderColor = System.Drawing.Color:Silver.
        appearance36:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:CellAppearance = appearance36.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:CellPadding = 0.
        appearance37:BackColor = System.Drawing.SystemColors:Control.
        appearance37:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance37:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance37:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance37:BorderColor = System.Drawing.SystemColors:Window.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:GroupByRowAppearance = appearance37.
        appearance38:TextHAlignAsString = "Left":U.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:HeaderAppearance = appearance38.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:ExternalSortMulti.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        appearance39:BackColor = System.Drawing.SystemColors:Window.
        appearance39:BorderColor = System.Drawing.Color:Silver.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:RowAppearance = appearance39.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:SelectTypeRow = Infragistics.Win.UltraWinGrid.SelectType:SingleAutoDrag.
        appearance40:BackColor = System.Drawing.SystemColors:ControlLight.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:Override:TemplateAddRowAppearance = appearance40.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:smartUpdatableBrowser2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:smartUpdatableBrowser2:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:smartUpdatableBrowser2:LinkDataSource = THIS-OBJECT:daFilter.
        THIS-OBJECT:smartUpdatableBrowser2:LinkTableIOSource = THIS-OBJECT:panelFilter.
        THIS-OBJECT:smartUpdatableBrowser2:Location = NEW System.Drawing.Point(6, 77).
        THIS-OBJECT:smartUpdatableBrowser2:Name = "smartUpdatableBrowser2":U.
        THIS-OBJECT:smartUpdatableBrowser2:SettingsKey = "ffa069ee-be14-74ad-e011-213d42bb6de8":U.
        THIS-OBJECT:smartUpdatableBrowser2:Size = NEW System.Drawing.Size(365, 132).
        THIS-OBJECT:smartUpdatableBrowser2:TabIndex = 12.
        THIS-OBJECT:smartUpdatableBrowser2:Text = "smartUpdatableBrowser2":U.
        /*  */
        /* bsFilter */
        /*  */
        THIS-OBJECT:bsFilter:MaxDataGuess = 0.
        THIS-OBJECT:bsFilter:NoLOBs = FALSE.
        THIS-OBJECT:bsFilter:Position = 0.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar7 AS "Progress.Data.TableDesc[]" NO-UNDO.
        arrayvar7 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc3:ChildTables = arrayvar7.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar8 AS Progress.Data.ColumnPropDesc EXTENT 3 NO-UNDO.
        arrayvar8[1] = NEW Progress.Data.ColumnPropDesc("Order":U, "Order":U, Progress.Data.DataType:INTEGER).
        arrayvar8[2] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "FieldName":U, Progress.Data.DataType:CHARACTER).
        arrayvar8[3] = NEW Progress.Data.ColumnPropDesc("Operator":U, "Operator":U, Progress.Data.DataType:CHARACTER).
        tableDesc3:Columns = arrayvar8.
        THIS-OBJECT:bsFilter:TableSchema = tableDesc3.
        /*  */
        /* daFilter */
        /*  */
        THIS-OBJECT:daFilter:LinkDataSource = ?.
        THIS-OBJECT:daFilter:LinkNavigationSource = THIS-OBJECT:panelFilter.
        /*  */
        /* tbLookupDialogQueryString */
        /*  */
        THIS-OBJECT:tbLookupDialogQueryString:Location = NEW System.Drawing.Point(6, 398).
        THIS-OBJECT:tbLookupDialogQueryString:Multiline = TRUE.
        THIS-OBJECT:tbLookupDialogQueryString:Name = "tbLookupDialogQueryString":U.
        THIS-OBJECT:tbLookupDialogQueryString:Size = NEW System.Drawing.Size(365, 80).
        THIS-OBJECT:tbLookupDialogQueryString:TabIndex = 11.
        THIS-OBJECT:tbLookupDialogQueryString:Text = "ultraTextEditor2":U.
        /*  */
        /* ultraLabel6 */
        /*  */
        appearance41:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel6:Appearance = appearance41.
        THIS-OBJECT:ultraLabel6:Location = NEW System.Drawing.Point(6, 384).
        THIS-OBJECT:ultraLabel6:Name = "ultraLabel6":U.
        THIS-OBJECT:ultraLabel6:Size = NEW System.Drawing.Size(365, 23).
        THIS-OBJECT:ultraLabel6:TabIndex = 10.
        THIS-OBJECT:ultraLabel6:Text = "Lookup Dialog Query-String:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance42:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance42.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(6, 25).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(100, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 1.
        THIS-OBJECT:ultraLabel1:Text = "Title:":U.
        /*  */
        /* tbLookupDialogTitle */
        /*  */
        THIS-OBJECT:tbLookupDialogTitle:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:tbLookupDialogTitle:Location = NEW System.Drawing.Point(112, 21).
        THIS-OBJECT:tbLookupDialogTitle:Name = "tbLookupDialogTitle":U.
        THIS-OBJECT:tbLookupDialogTitle:Size = NEW System.Drawing.Size(259, 21).
        THIS-OBJECT:tbLookupDialogTitle:TabIndex = 0.
        /*  */
        /* btnQueryWizard */
        /*  */
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:btnQueryWizard:Appearance = appearance1.
        THIS-OBJECT:btnQueryWizard:Location = NEW System.Drawing.Point(371, 379).
        THIS-OBJECT:btnQueryWizard:Name = "btnQueryWizard":U.
        THIS-OBJECT:btnQueryWizard:Size = NEW System.Drawing.Size(21, 21).
        THIS-OBJECT:btnQueryWizard:TabIndex = 9.
        THIS-OBJECT:btnQueryWizard:Click:Subscribe(THIS-OBJECT:btnQueryWizard_Click).
        /*  */
        /* BusinessEntityLookupDesignerForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(805, 592).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGroupBox2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGroupBox1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonOK).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBoxHeader).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:Name = "BusinessEntityLookupDesignerForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:Text = "SmartBusinessEntityLookup Designer ":U.
        CAST(THIS-OBJECT:pictureBoxHeader, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGroupBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox1:PerformLayout().
        CAST(THIS-OBJECT:cbKeyValueColumn, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbLookupQueryString, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bsFieldControls, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:daFieldControls, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:cbKeyField, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGroupBox2, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraGroupBox2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraGroupBox2:PerformLayout().
        CAST(THIS-OBJECT:smartUpdatableBrowser3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bsBrowseColumns, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:daBrowseColumns, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartUpdatableBrowser2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:bsFilter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:daFilter, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbLookupDialogQueryString, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:tbLookupDialogTitle, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the FormClosing event                                                                       
        Notes:
        @param e The FormClosingEventArgs with the data for this event                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosing (e AS System.Windows.Forms.FormClosingEventArgs):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        /* Mike Fechner, Consultingwerk Ltd. 09.07.2015
           SCL-906 - save pending changes */
        IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:
            smartUpdatableBrowser1:SaveChanges() . 
            
            IF smartUpdatableBrowser1:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                e:Cancel = TRUE . 
        END .        

        IF smartUpdatableBrowser2:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:
            smartUpdatableBrowser2:SaveChanges() . 
            
            IF smartUpdatableBrowser2:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                e:Cancel = TRUE . 
        END .        

        IF smartUpdatableBrowser3:SmartTableIOState = TableIOStateEnum:ModifyingData THEN DO:
            smartUpdatableBrowser3:SaveChanges() . 
            
            IF smartUpdatableBrowser3:SmartTableIOState = TableIOStateEnum:ModifyingData THEN 
                e:Cancel = TRUE . 
        END .        

        SUPER:OnFormClosing (e).

        /* Mike Fechner, Consultingwerk Ltd. 09.07.2015
           SCL-906 - don't serialize tt's into list properties on Cancel */
        IF e:Cancel = TRUE THEN 
            RETURN .
            
        FOR EACH ttFieldControls:
            ASSIGN i = i + 1.
            
            IF i > 1 THEN 
                ASSIGN THIS-OBJECT:FieldList   = THIS-OBJECT:FieldList + ",":U
                       THIS-OBJECT:ControlList = THIS-OBJECT:ControlList + ",":U .
            ELSE 
                ASSIGN THIS-OBJECT:FieldList   = "":U
                       THIS-OBJECT:ControlList = "":U .
                        
            ASSIGN THIS-OBJECT:FieldList   = THIS-OBJECT:FieldList + ttFieldControls.FieldName
                   THIS-OBJECT:ControlList = THIS-OBJECT:ControlList + ttFieldControls.ControlName .
        END. 
        
        ASSIGN i = 0 . 
        
        FOR EACH ttFilter:
            ASSIGN i = i + 1.
            
            IF i > 1 THEN 
                ASSIGN THIS-OBJECT:FilterFields    = THIS-OBJECT:FilterFields + ",":U
                       THIS-OBJECT:FilterOperators = THIS-OBJECT:FilterOperators + ",":U .
            ELSE 
                ASSIGN THIS-OBJECT:FilterFields    = "":U
                       THIS-OBJECT:FilterOperators = "":U .
                        
            ASSIGN THIS-OBJECT:FilterFields    = THIS-OBJECT:FilterFields + ttFilter.FieldName
                   THIS-OBJECT:FilterOperators = THIS-OBJECT:FilterOperators + ttFilter.Operator .
        END. 
        
        ASSIGN i = 0 . 
        
        FOR EACH ttBrowseColumn:
            ASSIGN i = i + 1.

            IF i > 1 THEN 
                ASSIGN THIS-OBJECT:BrowseColumns = THIS-OBJECT:BrowseColumns + ",":U .
            ELSE 
                ASSIGN THIS-OBJECT:BrowseColumns = "":U .
                        
            ASSIGN THIS-OBJECT:BrowseColumns = THIS-OBJECT:BrowseColumns + ttBrowseColumn.FieldName .
        END . 
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                        
        Notes:                                                                        
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):
        
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        
        oControlValueList = NEW Infragistics.Win.ValueList () .
        oFieldValueList = NEW Infragistics.Win.ValueList () .
        oOperatorsValueList = NEW Infragistics.Win.ValueList () .
        
        oControlValueList:SortStyle = Infragistics.Win.ValueListSortStyle:Ascending .
        oFieldValueList:SortStyle = Infragistics.Win.ValueListSortStyle:Ascending .
        oOperatorsValueList:SortStyle = Infragistics.Win.ValueListSortStyle:Ascending .

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ParentViewerControls):
            oControlValueList:ValueListItems:Add (ENTRY (i, THIS-OBJECT:ParentViewerControls), 
                                                ENTRY (i, THIS-OBJECT:ParentViewerControls)) .
        END.

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:LookupFields):
            oFieldValueList:ValueListItems:Add (ENTRY (i, THIS-OBJECT:LookupFields), 
                                                ENTRY (i, THIS-OBJECT:LookupFields)) .
        END.

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:FieldList):
            CREATE ttFieldControls . 
            ASSIGN ttFieldControls.FieldName   = ENTRY (i, THIS-OBJECT:FieldList)
                   ttFieldControls.ControlName = ENTRY (i, THIS-OBJECT:ControlList)
                   NO-ERROR .
        END.

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:FilterFields):
            CREATE ttFilter.
            ASSIGN ttFilter.Order = i
                   ttFilter.FieldName = ENTRY (i, THIS-OBJECT:FilterFields) 
                   ttFilter.Operator  = ENTRY (i, THIS-OBJECT:FilterOperators) 
                 NO-ERROR .
                 
            IF ttFilter.Operator = "=":U THEN 
                ttFilter.Operator = "EQ":U .                  

            IF ttFilter.Operator = ">=":U THEN 
                ttFilter.Operator = "GE":U .                  

            IF ttFilter.Operator = "<=":U THEN 
                ttFilter.Operator = "LE":U .                  
        END.
        
        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:BrowseColumns):
            CREATE ttBrowseColumn.
            ASSIGN ttBrowseColumn.Order     = i
                   ttBrowseColumn.FieldName = ENTRY (i, THIS-OBJECT:BrowseColumns)
                   .
        END.
        
        oFieldValueList2 = oFieldValueList:Clone()  .
        oFieldValueList3 = oFieldValueList:Clone()  .
        oFieldValueList4 = oFieldValueList:Clone()  .
        
        oOperatorsValueList:ValueListItems:Add ("EQ":U, "EQ":U) .
        oOperatorsValueList:ValueListItems:Add ("GE":U, "GE":U) .
        oOperatorsValueList:ValueListItems:Add ("LE":U, "LE":U) .
        oOperatorsValueList:ValueListItems:Add ("BEGINS":U, "BEGINS":U) .
        oOperatorsValueList:ValueListItems:Add ("CONTAINS":U, "CONTAINS":U) .
        oOperatorsValueList:ValueListItems:Add ("MATCHES":U, "MATCHES":U) .
        oOperatorsValueList:ValueListItems:Add ("BEGINSMATCHES":U, "BEGINS or MATCHES":U) .
        
        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["FieldName":U]:ValueList = oFieldValueList .
        smartUpdatableBrowser1:DisplayLayout:Bands[0]:Columns["ControlName":U]:ValueList = oControlValueList .

        smartUpdatableBrowser2:DisplayLayout:Bands[0]:Columns["FieldName":U]:ValueList = oFieldValueList2  .
        smartUpdatableBrowser2:DisplayLayout:Bands[0]:Columns["Operator":U]:ValueList = oOperatorsValueList .

        smartUpdatableBrowser3:DisplayLayout:Bands[0]:Columns["FieldName":U]:ValueList = oFieldValueList3 .

        daFieldControls:TempTableHandle = TEMP-TABLE ttFieldControls:HANDLE . 
        daFieldControls:RetrieveData () .
        
        daFilter:TempTableHandle = TEMP-TABLE ttFilter:HANDLE . 
        daFilter:RetrieveData () .

        daBrowseColumns:TempTableHandle = TEMP-TABLE ttBrowseColumn:HANDLE . 
        daBrowseColumns:RetrieveData () .

        cbKeyField:ValueList = oFieldValueList .         
        cbKeyValueColumn:ValueList = oFieldValueList4 .        
        
        SUPER:OnLoad(INPUT e).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the BusinessEntityLookupDesignerForm class                                                               
        Notes:                                                                        
    ------------------------------------------------------------------------------*/            
    DESTRUCTOR PUBLIC BusinessEntityLookupDesignerForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.