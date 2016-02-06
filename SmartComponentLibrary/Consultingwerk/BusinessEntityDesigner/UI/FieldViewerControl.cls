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
    File        : FieldViewerControl
    Purpose     : Viewer Control for the eField Table
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 23 20:58:03 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                     FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*                          FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*                          FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH . 
USING Consultingwerk.Windows.Controls.*                              FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .
USING Infragistics.Win.UltraWinTabControl.*                          FROM ASSEMBLY .
USING System.Windows.Forms.*                                         FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.UI.FieldViewerControl INHERITS SmartViewerControl: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE eField_FieldCustomControlName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE eField_FieldCustomControlName AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldExtent_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldExtent AS Infragistics.Win.UltraWinEditors.UltraNumericEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraCheckEditor1 AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraCheckEditor2 AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor1 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor3 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraComboEditor2 AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl4 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl3 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl2 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldXmlNodeType AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldXmlNodeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldXmlDataType AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldSource_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldSource AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldSerializeName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldSerializeHidden AS Infragistics.Win.UltraWinEditors.UltraCheckEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldName_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldName AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldLabel_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldLabel AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldInitial_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldInitial AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldFormat_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldFormat AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldDescription AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityBindingSource1_eField_FieldDataType_Label AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.

    &IF NOT PROVERSION BEGINS "10" &THEN
    DEFINE VARIABLE oValidationSettings AS FieldValidationPropertiesControl NO-UNDO . 
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the FieldViewerControl class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FieldViewerControl ():
        
        DEFINE VARIABLE oTab AS UltraTab NO-UNDO . 
        
        SUPER().
        InitializeComponent().
        
        /* Mike Fechner, Consultingwerk Ltd. 18.05.2015
           SCL-424 : Validation properties when not running on OE10 */
        &IF NOT PROVERSION BEGINS "10" &THEN
        oTab = ultraTabControl1:Tabs:Add ("Validation":U, "Validation Settings":U) .         
        oValidationSettings = NEW FieldValidationPropertiesControl () .
        oValidationSettings:ValueChanged:Subscribe (ValueChangedHandler) .
        oValidationSettings:Dock = DockStyle:Fill .
        oTab:TabPage:Controls:Add (oValidationSettings) .
        &ENDIF
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
       Purpose: This Method attaches a new BindingSource given as the Parameter 
                poBindingSource to the Objects of the current Class. The new 
                BindingSource may have been created by a DataAdapter or reused from a 
                SmartBrowserControl
       Notes:   It is called by the SmartDataAdapter Class when creating a new 
                BindingSource or when a new SmartDataSource is registered to the
                Class implementing the Interface ISmartDataTarget.
                This method is also called from the SmartViewerControl destructor (while
                a form is closed) to de-attach from the Binding Source. In this case
                the poBindingSource reference is passed in as the unknown value. 
       @param poBindingSource The BindingSource to be attached to the Controls contained in the Viewer 
   ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource):
		
		SUPER:AttachBindingSource (poBindingSource).

        &IF NOT PROVERSION BEGINS "10" &THEN
        IF VALID-OBJECT (oValidationSettings) AND VALID-OBJECT (poBindingSource) THEN 
            oValidationSettings:DataBindings:Add ("Text":U, poBindingSource, "FieldValidationInstance":U) .
        &ENDIF        

	END METHOD.

	/*------------------------------------------------------------------------------
	   Purpose: Event handler for the ValueChanged event of the ultraComboEditor1
	   Notes:
	   @param sender The reference to the Control that raised the event
	   @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID DataTypeComboValueChanged (sender AS System.Object,
	                                               e AS System.EventArgs):
		
		DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.
		
		IF NOT ultraComboEditor1:Focused THEN 
		    RETURN . 

        IF PROGRAM-NAME (2) BEGINS "CancelUpdate ":U THEN 
            RETURN . 

        ASSIGN cDataType = ultraComboEditor1:Text .

        IF DataTypeHelper:IsAblDataType (cDataType) 
           AND NOT (cDataType = "BLOB":U OR 
                    cDataType = "COM-HANDLE":U OR 
                    cDataType = "HANDLE":U OR 
                    cDataType = "Progress.Lang.Object":U OR 
                    cDataType = "RAW":U OR 
                    cDataType = "RECID":U OR 
                    cDataType = "ROWID":U)
            THEN 

            THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:Text = DataTypeHelper:DefaultFormat (cDataType) .
        ELSE 
            THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:Text = "":U .

        IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN 
            THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns a copy of the value list with the data types 
        Notes:   
        @return The copy of the value list with the data types 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC Infragistics.Win.ValueList GetDataTypeValueList ():

        DEFINE VARIABLE oValueList AS Infragistics.Win.ValueList NO-UNDO . 
        
        oValueList = CAST (THIS-OBJECT:ultraComboEditor1:ValueList:Clone (),
                           Infragistics.Win.ValueList) .
		
		oValueList:ValueListItems:Clear () .
		
		{Consultingwerk/foreach.i Infragistics.Win.ValueListItem oItem in THIS-OBJECT:ultraComboEditor1:ValueList:ValueListItems}
		    oValueList:ValueListItems:Add (oItem:DataValue, oItem:DisplayText) . 
		END.
		
		RETURN oValueList .

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
        tableDesc1 = NEW Progress.Data.TableDesc("eField":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem8 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem8 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.BusinessEntityDesigner.UI.FieldViewerControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem18 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem18 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem19 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem19 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem20 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem20 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem21 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem21 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance5 AS Infragistics.Win.Appearance NO-UNDO.
        appearance5 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem22 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem22 = NEW Infragistics.Win.ValueListItem().
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
        DEFINE VARIABLE valueListItem1 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem1 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance9 AS Infragistics.Win.Appearance NO-UNDO.
        appearance9 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem2 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem2 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance10 AS Infragistics.Win.Appearance NO-UNDO.
        appearance10 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem3 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem3 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
        appearance11 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem4 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem4 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
        appearance12 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem5 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem5 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
        appearance13 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem6 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem6 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance14 AS Infragistics.Win.Appearance NO-UNDO.
        appearance14 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem7 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem7 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance15 AS Infragistics.Win.Appearance NO-UNDO.
        appearance15 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem9 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem9 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
        appearance16 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem10 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem10 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance17 AS Infragistics.Win.Appearance NO-UNDO.
        appearance17 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem11 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem11 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance18 AS Infragistics.Win.Appearance NO-UNDO.
        appearance18 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem17 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem17 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance19 AS Infragistics.Win.Appearance NO-UNDO.
        appearance19 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem12 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem12 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
        appearance20 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem16 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem16 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
        appearance21 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem13 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem13 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
        appearance22 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem14 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem14 = NEW Infragistics.Win.ValueListItem().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
        appearance23 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE valueListItem15 AS Infragistics.Win.ValueListItem NO-UNDO.
        valueListItem15 = NEW Infragistics.Win.ValueListItem().
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
        DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
        appearance29 = NEW Infragistics.Win.Appearance().
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
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraCheckEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityBindingSource(THIS-OBJECT:components).
        THIS-OBJECT:eField_FieldCustomControlName = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraCheckEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:ultraComboEditor3 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraComboEditor2 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraComboEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent = NEW Infragistics.Win.UltraWinEditors.UltraNumericEditor().
        THIS-OBJECT:eField_FieldCustomControlName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraTabPageControl2 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabPageControl3 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden = NEW Infragistics.Win.UltraWinEditors.UltraCheckEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabPageControl4 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraCheckEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:eField_FieldCustomControlName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraCheckEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor3, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor2, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl2:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl3:SuspendLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabPageControl4:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:AutoScroll = TRUE.
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraCheckEditor2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:eField_FieldCustomControlName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraCheckEditor1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraComboEditor3).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraComboEditor2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraComboEditor1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:eField_FieldCustomControlName_Label).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(554, 220).
        /*  */
        /* ultraCheckEditor2 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraCheckEditor2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraCheckEditor2:AutoSize = TRUE.
        THIS-OBJECT:ultraCheckEditor2:DataBindings:Add(NEW System.Windows.Forms.Binding("Checked":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldInitialUnknownValue":U, TRUE)).
        THIS-OBJECT:ultraCheckEditor2:Location = NEW System.Drawing.Point(437, 58).
        THIS-OBJECT:ultraCheckEditor2:Name = "ultraCheckEditor2":U.
        THIS-OBJECT:ultraCheckEditor2:Size = NEW System.Drawing.Size(98, 17).
        THIS-OBJECT:ultraCheckEditor2:TabIndex = 9.
        THIS-OBJECT:ultraCheckEditor2:Text = "Initial Unknown":U.
        THIS-OBJECT:ultraCheckEditor2:CheckedChanged:Subscribe(THIS-OBJECT:ultraCheckEditor2_CheckedChanged).
        /*  */
        /* smartBusinessEntityBindingSource1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1:BindTo = "QUERY":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityName = "Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:EntityTable = "eField":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:NoLOBs = FALSE.
        tableDesc1:ChildTables = ?.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Progress.Data.ColumnPropDesc EXTENT 22 NO-UNDO.
        arrayvar0[1] = NEW Progress.Data.ColumnPropDesc("BusinessEntityName":U, "BusinessEntityName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[2] = NEW Progress.Data.ColumnPropDesc("TempTableName":U, "TempTableName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[3] = NEW Progress.Data.ColumnPropDesc("FieldName":U, "FieldName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[4] = NEW Progress.Data.ColumnPropDesc("FieldOrder":U, "FieldOrder":U, Progress.Data.DataType:INTEGER).
        arrayvar0[5] = NEW Progress.Data.ColumnPropDesc("FieldDataType":U, "FieldDataType":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[6] = NEW Progress.Data.ColumnPropDesc("FieldDescription":U, "FieldDescription":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[7] = NEW Progress.Data.ColumnPropDesc("FieldInitial":U, "FieldInitial":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[8] = NEW Progress.Data.ColumnPropDesc("FieldInitialUnknownValue":U, "Initial Value Unknown":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[9] = NEW Progress.Data.ColumnPropDesc("FieldLabel":U, "FieldLabel":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[10] = NEW Progress.Data.ColumnPropDesc("FieldFormat":U, "FieldFormat":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[11] = NEW Progress.Data.ColumnPropDesc("FieldSerializeHidden":U, "FieldSerializeHidden":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[12] = NEW Progress.Data.ColumnPropDesc("FieldSerializeName":U, "FieldSerializeName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[13] = NEW Progress.Data.ColumnPropDesc("FieldXmlDataType":U, "FieldXmlDataType":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[14] = NEW Progress.Data.ColumnPropDesc("FieldXmlNodeType":U, "FieldXmlNodeType":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[15] = NEW Progress.Data.ColumnPropDesc("FieldXmlNodeName":U, "FieldXmlNodeName":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[16] = NEW Progress.Data.ColumnPropDesc("FieldSource":U, "FieldSource":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[17] = NEW Progress.Data.ColumnPropDesc("FieldExtent":U, "FieldExtent":U, Progress.Data.DataType:INTEGER).
        arrayvar0[18] = NEW Progress.Data.ColumnPropDesc("FieldGetAccess":U, "&Get Access":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[19] = NEW Progress.Data.ColumnPropDesc("FieldSetAccess":U, "&Set Access":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[20] = NEW Progress.Data.ColumnPropDesc("FieldCalcExpression":U, "Calc Expression":U, Progress.Data.DataType:CHARACTER).
        arrayvar0[21] = NEW Progress.Data.ColumnPropDesc("FieldCaseSensitive":U, "Case Sensitive":U, Progress.Data.DataType:LOGICAL).
        arrayvar0[22] = NEW Progress.Data.ColumnPropDesc("FieldCustomControlName":U, "Field Custom Control Name":U, Progress.Data.DataType:CHARACTER).
        tableDesc1:Columns = arrayvar0.
        THIS-OBJECT:smartBusinessEntityBindingSource1:TableSchema = tableDesc1.
        /*  */
        /* eField_FieldCustomControlName */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eField_FieldCustomControlName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:eField_FieldCustomControlName:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldCustomControlName":U, TRUE)).
        THIS-OBJECT:eField_FieldCustomControlName:Location = NEW System.Drawing.Point(150, 193).
        THIS-OBJECT:eField_FieldCustomControlName:Name = "eField_FieldCustomControlName":U.
        THIS-OBJECT:eField_FieldCustomControlName:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:eField_FieldCustomControlName:TabIndex = 21.
        /*  */
        /* ultraCheckEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:ultraCheckEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraCheckEditor1:AutoSize = TRUE.
        THIS-OBJECT:ultraCheckEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Checked":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldCaseSensitive":U, TRUE)).
        THIS-OBJECT:ultraCheckEditor1:Location = NEW System.Drawing.Point(437, 31).
        THIS-OBJECT:ultraCheckEditor1:Name = "ultraCheckEditor1":U.
        THIS-OBJECT:ultraCheckEditor1:Size = NEW System.Drawing.Size(97, 17).
        THIS-OBJECT:ultraCheckEditor1:TabIndex = 6.
        THIS-OBJECT:ultraCheckEditor1:Text = "Case Sensitive":U.
        /*  */
        /* ultraComboEditor3 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor3:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor3:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldSetAccess":U, TRUE)).
        THIS-OBJECT:ultraComboEditor3:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraComboEditor3:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        valueListItem8:Appearance = appearance1.
        valueListItem8:DataValue = "PUBLIC":U.
        valueListItem8:DisplayText = "public":U.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        valueListItem18:Appearance = appearance2.
        valueListItem18:DataValue = "PROTECTED":U.
        valueListItem18:DisplayText = "protected":U.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image":U), System.Object).
        valueListItem19:Appearance = appearance3.
        valueListItem19:DataValue = "PRIVATE":U.
        valueListItem19:DisplayText = "private (not recommended)":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.ValueListItem EXTENT 3 NO-UNDO.
        arrayvar1[1] = valueListItem8.
        arrayvar1[2] = valueListItem18.
        arrayvar1[3] = valueListItem19.
        THIS-OBJECT:ultraComboEditor3:Items:AddRange(arrayvar1).
        THIS-OBJECT:ultraComboEditor3:Location = NEW System.Drawing.Point(393, 165).
        THIS-OBJECT:ultraComboEditor3:Name = "ultraComboEditor3":U.
        THIS-OBJECT:ultraComboEditor3:Size = NEW System.Drawing.Size(158, 21).
        THIS-OBJECT:ultraComboEditor3:TabIndex = 19.
        /*  */
        /* ultraComboEditor2 */
        /*  */
        THIS-OBJECT:ultraComboEditor2:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldGetAccess":U, TRUE)).
        THIS-OBJECT:ultraComboEditor2:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraComboEditor2:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        appearance4:Image = CAST(resources:GetObject("appearance4.Image":U), System.Object).
        valueListItem20:Appearance = appearance4.
        valueListItem20:DataValue = "PUBLIC":U.
        valueListItem20:DisplayText = "public":U.
        appearance5:Image = CAST(resources:GetObject("appearance5.Image":U), System.Object).
        valueListItem21:Appearance = appearance5.
        valueListItem21:DataValue = "PROTECTED":U.
        valueListItem21:DisplayText = "protected":U.
        appearance6:Image = CAST(resources:GetObject("appearance6.Image":U), System.Object).
        valueListItem22:Appearance = appearance6.
        valueListItem22:DataValue = "PRIVATE":U.
        valueListItem22:DisplayText = "private (not recommended)":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.ValueListItem EXTENT 3 NO-UNDO.
        arrayvar2[1] = valueListItem20.
        arrayvar2[2] = valueListItem21.
        arrayvar2[3] = valueListItem22.
        THIS-OBJECT:ultraComboEditor2:Items:AddRange(arrayvar2).
        THIS-OBJECT:ultraComboEditor2:Location = NEW System.Drawing.Point(150, 165).
        THIS-OBJECT:ultraComboEditor2:Name = "ultraComboEditor2":U.
        THIS-OBJECT:ultraComboEditor2:Size = NEW System.Drawing.Size(167, 21).
        THIS-OBJECT:ultraComboEditor2:TabIndex = 17.
        THIS-OBJECT:ultraComboEditor2:ValueChanged:Subscribe(THIS-OBJECT:ultraComboEditor2_ValueChanged).
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance7:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel2:Appearance = appearance7.
        THIS-OBJECT:ultraLabel2:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(323, 169).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(64, 14).
        THIS-OBJECT:ultraLabel2:TabIndex = 18.
        THIS-OBJECT:ultraLabel2:Text = "&Set Access:":U.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance8:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance8.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(4, 169).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(65, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 16.
        THIS-OBJECT:ultraLabel1:Text = "&Get Access:":U.
        /*  */
        /* ultraComboEditor1 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraComboEditor1:CharacterCasing = System.Windows.Forms.CharacterCasing:Upper.
        THIS-OBJECT:ultraComboEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldDataType":U, TRUE)).
        THIS-OBJECT:ultraComboEditor1:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:ultraComboEditor1:DropDownButtonDisplayStyle = Infragistics.Win.ButtonDisplayStyle:OnMouseEnter.
        THIS-OBJECT:ultraComboEditor1:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        appearance9:Image = CAST(resources:GetObject("appearance9.Image":U), System.Object).
        valueListItem1:Appearance = appearance9.
        valueListItem1:DataValue = "BLOB":U.
        valueListItem1:DisplayText = "BLOB":U.
        appearance10:Image = CAST(resources:GetObject("appearance10.Image":U), System.Object).
        valueListItem2:Appearance = appearance10.
        valueListItem2:DataValue = "CHARACTER":U.
        valueListItem2:DisplayText = "CHARACTER":U.
        appearance11:Image = CAST(resources:GetObject("appearance11.Image":U), System.Object).
        valueListItem3:Appearance = appearance11.
        valueListItem3:DataValue = "CLOB":U.
        valueListItem3:DisplayText = "CLOB":U.
        appearance12:Image = CAST(resources:GetObject("appearance12.Image":U), System.Object).
        valueListItem4:Appearance = appearance12.
        valueListItem4:DataValue = "COM-HANDLE":U.
        valueListItem4:DisplayText = "COM-HANDLE":U.
        appearance13:Image = CAST(resources:GetObject("appearance13.Image":U), System.Object).
        valueListItem5:Appearance = appearance13.
        valueListItem5:DataValue = "DATE":U.
        valueListItem5:DisplayText = "DATE":U.
        appearance14:Image = CAST(resources:GetObject("appearance14.Image":U), System.Object).
        valueListItem6:Appearance = appearance14.
        valueListItem6:DataValue = "DATETIME":U.
        valueListItem6:DisplayText = "DATETIME":U.
        appearance15:Image = CAST(resources:GetObject("appearance15.Image":U), System.Object).
        valueListItem7:Appearance = appearance15.
        valueListItem7:DataValue = "DATETIME-TZ":U.
        valueListItem7:DisplayText = "DATETIME-TZ":U.
        appearance16:Image = CAST(resources:GetObject("appearance16.Image":U), System.Object).
        valueListItem9:Appearance = appearance16.
        valueListItem9:DataValue = "DECIMAL":U.
        valueListItem9:DisplayText = "DECIMAL":U.
        appearance17:Image = CAST(resources:GetObject("appearance17.Image":U), System.Object).
        valueListItem10:Appearance = appearance17.
        valueListItem10:DataValue = "HANDLE":U.
        valueListItem10:DisplayText = "HANDLE":U.
        appearance18:Image = CAST(resources:GetObject("appearance18.Image":U), System.Object).
        valueListItem11:Appearance = appearance18.
        valueListItem11:DataValue = "INT64":U.
        valueListItem11:DisplayText = "INT64":U.
        appearance19:Image = CAST(resources:GetObject("appearance19.Image":U), System.Object).
        valueListItem17:Appearance = appearance19.
        valueListItem17:DataValue = "INTEGER":U.
        valueListItem17:DisplayText = "INTEGER":U.
        appearance20:Image = CAST(resources:GetObject("appearance20.Image":U), System.Object).
        valueListItem12:Appearance = appearance20.
        valueListItem12:DataValue = "LOGICAL":U.
        valueListItem12:DisplayText = "LOGICAL":U.
        appearance21:Image = CAST(resources:GetObject("appearance21.Image":U), System.Object).
        valueListItem16:Appearance = appearance21.
        valueListItem16:DataValue = "PROGRESS.LANG.OBJECT":U.
        valueListItem16:DisplayText = "Progress.Lang.Object":U.
        appearance22:Image = CAST(resources:GetObject("appearance22.Image":U), System.Object).
        valueListItem13:Appearance = appearance22.
        valueListItem13:DataValue = "RAW":U.
        valueListItem13:DisplayText = "RAW":U.
        appearance23:Image = CAST(resources:GetObject("appearance23.Image":U), System.Object).
        valueListItem14:Appearance = appearance23.
        valueListItem14:DataValue = "RECID":U.
        valueListItem14:DisplayText = "RECID":U.
        appearance24:Image = CAST(resources:GetObject("appearance24.Image":U), System.Object).
        valueListItem15:Appearance = appearance24.
        valueListItem15:DataValue = "ROWID":U.
        valueListItem15:DisplayText = "ROWID":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.ValueListItem EXTENT 16 NO-UNDO.
        arrayvar3[1] = valueListItem1.
        arrayvar3[2] = valueListItem2.
        arrayvar3[3] = valueListItem3.
        arrayvar3[4] = valueListItem4.
        arrayvar3[5] = valueListItem5.
        arrayvar3[6] = valueListItem6.
        arrayvar3[7] = valueListItem7.
        arrayvar3[8] = valueListItem9.
        arrayvar3[9] = valueListItem10.
        arrayvar3[10] = valueListItem11.
        arrayvar3[11] = valueListItem17.
        arrayvar3[12] = valueListItem12.
        arrayvar3[13] = valueListItem16.
        arrayvar3[14] = valueListItem13.
        arrayvar3[15] = valueListItem14.
        arrayvar3[16] = valueListItem15.
        THIS-OBJECT:ultraComboEditor1:Items:AddRange(arrayvar3).
        THIS-OBJECT:ultraComboEditor1:Location = NEW System.Drawing.Point(150, 30).
        THIS-OBJECT:ultraComboEditor1:Name = "ultraComboEditor1":U.
        THIS-OBJECT:ultraComboEditor1:Size = NEW System.Drawing.Size(167, 21).
        THIS-OBJECT:ultraComboEditor1:TabIndex = 3.
        THIS-OBJECT:ultraComboEditor1:Text = "CHARACTER":U.
        THIS-OBJECT:ultraComboEditor1:ValueChanged:Subscribe(THIS-OBJECT:DataTypeComboValueChanged).
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldName */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:Location = NEW System.Drawing.Point(150, 3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:Name = "smartBusinessEntityBindingSource1_eField_FieldName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:TabIndex = 1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:Leave:Subscribe(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Leave).
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldName_Label */
        /*  */
        appearance25:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label:Appearance = appearance25.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label:Location = NEW System.Drawing.Point(3, 7).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label:Size = NEW System.Drawing.Size(38, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label:TabIndex = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName_Label:Text = "&Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldSource */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldSource":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource:Location = NEW System.Drawing.Point(150, 138).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource:Name = "smartBusinessEntityBindingSource1_eField_FieldSource":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource:TabIndex = 15.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldSource_Label */
        /*  */
        appearance26:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label:Appearance = appearance26.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label:Location = NEW System.Drawing.Point(4, 142).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldSource_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label:Size = NEW System.Drawing.Size(71, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label:TabIndex = 14.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource_Label:Text = "&Source Field:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldFormat */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldFormat":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:Location = NEW System.Drawing.Point(150, 111).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:Name = "smartBusinessEntityBindingSource1_eField_FieldFormat":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat:TabIndex = 13.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldFormat_Label */
        /*  */
        appearance27:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label:Appearance = appearance27.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label:Location = NEW System.Drawing.Point(4, 115).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldFormat_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label:Size = NEW System.Drawing.Size(43, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label:TabIndex = 12.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat_Label:Text = "&Format:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldDataType_Label */
        /*  */
        appearance28:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label:Appearance = appearance28.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label:Location = NEW System.Drawing.Point(4, 34).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldDataType_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label:Size = NEW System.Drawing.Size(59, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label:TabIndex = 2.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDataType_Label:Text = "&Data Type:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldLabel */
        /*  */
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar8 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldLabel":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:Location = NEW System.Drawing.Point(150, 84).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:Name = "smartBusinessEntityBindingSource1_eField_FieldLabel":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:TabIndex = 11.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldLabel_Label */
        /*  */
        appearance29:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label:Appearance = appearance29.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label:Location = NEW System.Drawing.Point(4, 88).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldLabel_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label:Size = NEW System.Drawing.Size(35, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label:TabIndex = 10.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel_Label:Text = "&Label:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldInitial_Label */
        /*  */
        appearance30:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label:Appearance = appearance30.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label:Location = NEW System.Drawing.Point(3, 61).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldInitial_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label:Size = NEW System.Drawing.Size(34, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label:TabIndex = 7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial_Label:Text = "&Initial:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldInitial */
        /*  */
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar9 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldInitial":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial:Location = NEW System.Drawing.Point(150, 57).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial:Name = "smartBusinessEntityBindingSource1_eField_FieldInitial":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial:Size = NEW System.Drawing.Size(278, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial:TabIndex = 8.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldExtent_Label */
        /*  */
        DEFINE VARIABLE nestedvar10 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar10 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar10, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance31:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:Appearance = appearance31.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:Location = NEW System.Drawing.Point(332, 34).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldExtent_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:Size = NEW System.Drawing.Size(39, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:TabIndex = 4.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent_Label:Text = "&Extent:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldExtent */
        /*  */
        DEFINE VARIABLE nestedvar11 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar11 = System.Windows.Forms.AnchorStyles:Top.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar11, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldExtent":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:Office2007.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:Location = NEW System.Drawing.Point(377, 30).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:MaskInput = "nnnn":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:MaxValue = 9999.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:MinValue = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:Name = "smartBusinessEntityBindingSource1_eField_FieldExtent":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:Size = NEW System.Drawing.Size(51, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent:TabIndex = 5.
        /*  */
        /* eField_FieldCustomControlName_Label */
        /*  */
        appearance32:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:eField_FieldCustomControlName_Label:Appearance = appearance32.
        THIS-OBJECT:eField_FieldCustomControlName_Label:AutoSize = TRUE.
        THIS-OBJECT:eField_FieldCustomControlName_Label:Location = NEW System.Drawing.Point(4, 196).
        THIS-OBJECT:eField_FieldCustomControlName_Label:Name = "eField_FieldCustomControlName_Label":U.
        THIS-OBJECT:eField_FieldCustomControlName_Label:Size = NEW System.Drawing.Size(147, 14).
        THIS-OBJECT:eField_FieldCustomControlName_Label:TabIndex = 20.
        THIS-OBJECT:eField_FieldCustomControlName_Label:Text = "&Field Custom Control Name:":U.
        /*  */
        /* ultraTabPageControl2 */
        /*  */
        THIS-OBJECT:ultraTabPageControl2:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription).
        THIS-OBJECT:ultraTabPageControl2:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl2:Name = "ultraTabPageControl2":U.
        THIS-OBJECT:ultraTabPageControl2:Size = NEW System.Drawing.Size(554, 220).
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldDescription */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldDescription":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:Multiline = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:Name = "smartBusinessEntityBindingSource1_eField_FieldDescription":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:Size = NEW System.Drawing.Size(554, 220).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription:TabIndex = 0.
        /*  */
        /* ultraTabPageControl3 */
        /*  */
        THIS-OBJECT:ultraTabPageControl3:AutoScroll = TRUE.
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label).
        THIS-OBJECT:ultraTabPageControl3:Controls:Add(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType).
        THIS-OBJECT:ultraTabPageControl3:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabPageControl3:Name = "ultraTabPageControl3":U.
        THIS-OBJECT:ultraTabPageControl3:Size = NEW System.Drawing.Size(554, 220).
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldSerializeHidden */
        /*  */
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden:DataBindings:Add(NEW System.Windows.Forms.Binding("CheckedValue":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldSerializeHidden":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden:Location = NEW System.Drawing.Point(150, 3).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden:Name = "smartBusinessEntityBindingSource1_eField_FieldSerializeHidden":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden:Size = NEW System.Drawing.Size(150, 20).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden:TabIndex = 0.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden:Text = "Serialize &Hidden":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden:UseMnemonics = TRUE.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldXmlNodeName */
        /*  */
        DEFINE VARIABLE nestedvar12 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar12 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar12, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldXmlNodeName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName:Location = NEW System.Drawing.Point(150, 110).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName:Name = "smartBusinessEntityBindingSource1_eField_FieldXmlNodeName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName:TabIndex = 8.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label */
        /*  */
        appearance33:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label:Appearance = appearance33.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label:Location = NEW System.Drawing.Point(3, 114).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label:Size = NEW System.Drawing.Size(68, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label:TabIndex = 7.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName_Label:Text = "&Node Name:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label */
        /*  */
        appearance34:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label:Appearance = appearance34.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label:Location = NEW System.Drawing.Point(3, 33).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label:Size = NEW System.Drawing.Size(81, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label:TabIndex = 1.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName_Label:Text = "&SerializeName:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldXmlNodeType */
        /*  */
        DEFINE VARIABLE nestedvar13 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar13 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar13, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldXmlNodeType":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType:Location = NEW System.Drawing.Point(150, 83).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType:Name = "smartBusinessEntityBindingSource1_eField_FieldXmlNodeType":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType:TabIndex = 6.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldSerializeName */
        /*  */
        DEFINE VARIABLE nestedvar14 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar14 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar14, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldSerializeName":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName:Location = NEW System.Drawing.Point(150, 29).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName:Name = "smartBusinessEntityBindingSource1_eField_FieldSerializeName":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName:TabIndex = 2.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label */
        /*  */
        appearance35:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label:Appearance = appearance35.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label:Location = NEW System.Drawing.Point(3, 87).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label:Size = NEW System.Drawing.Size(63, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label:TabIndex = 5.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType_Label:Text = "Node &Type:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label */
        /*  */
        appearance36:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label:Appearance = appearance36.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label:AutoSize = TRUE.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label:Location = NEW System.Drawing.Point(3, 60).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label:Name = "smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label:Size = NEW System.Drawing.Size(59, 14).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label:TabIndex = 3.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType_Label:Text = "Data &Type:":U.
        /*  */
        /* smartBusinessEntityBindingSource1_eField_FieldXmlDataType */
        /*  */
        DEFINE VARIABLE nestedvar15 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar15 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar15, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType:DataBindings:Add(NEW System.Windows.Forms.Binding("Text":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldXmlDataType":U, TRUE)).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType:Location = NEW System.Drawing.Point(150, 56).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType:Name = "smartBusinessEntityBindingSource1_eField_FieldXmlDataType":U.
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType:Size = NEW System.Drawing.Size(401, 21).
        THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType:TabIndex = 4.
        /*  */
        /* ultraTabPageControl4 */
        /*  */
        THIS-OBJECT:ultraTabPageControl4:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ultraTabPageControl4:Location = NEW System.Drawing.Point(1, 22).
        THIS-OBJECT:ultraTabPageControl4:Name = "ultraTabPageControl4":U.
        THIS-OBJECT:ultraTabPageControl4:Size = NEW System.Drawing.Size(554, 220).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:AlwaysInEditMode = TRUE.
        appearance37:BackColor = System.Drawing.Color:White.
        appearance37:BackColor2 = System.Drawing.Color:White.
        appearance37:FontData:Name = "Courier New":U.
        appearance37:FontData:SizeInPoints = Progress.Util.CastUtil:ToSingle(11).
        appearance37:ForeColor = System.Drawing.Color:Black.
        THIS-OBJECT:ultraTextEditor1:Appearance = appearance37.
        THIS-OBJECT:ultraTextEditor1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ultraTextEditor1:DataBindings:Add(NEW System.Windows.Forms.Binding("Value":U, THIS-OBJECT:smartBusinessEntityBindingSource1, "FieldCalcExpression":U, TRUE)).
        THIS-OBJECT:ultraTextEditor1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTextEditor1:Multiline = TRUE.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(554, 220).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 0.
        THIS-OBJECT:ultraTextEditor1:UseAppStyling = FALSE.
        /*  */
        /* ultraTabControl1 */
        /*  */
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl2).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl3).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl4).
        THIS-OBJECT:ultraTabControl1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(556, 243).
        THIS-OBJECT:ultraTabControl1:TabIndex = 0.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Field":U.
        ultraTab2:TabPage = THIS-OBJECT:ultraTabPageControl2.
        ultraTab2:Text = "Description":U.
        ultraTab3:TabPage = THIS-OBJECT:ultraTabPageControl3.
        ultraTab3:Text = "XML":U.
        ultraTab4:TabPage = THIS-OBJECT:ultraTabPageControl4.
        ultraTab4:Text = "Calculated Field Expression":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 4 NO-UNDO.
        arrayvar4[1] = ultraTab1.
        arrayvar4[2] = ultraTab2.
        arrayvar4[3] = ultraTab3.
        arrayvar4[4] = ultraTab4.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar4).
        THIS-OBJECT:ultraTabControl1:ViewStyle = Infragistics.Win.UltraWinTabControl.ViewStyle:Office2007.
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(554, 220).
        /*  */
        /* FieldViewerControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:Name = "FieldViewerControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(556, 243).
        THIS-OBJECT:SmartTableIOState = "NoDataSource":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraCheckEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:eField_FieldCustomControlName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraCheckEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor3, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor2, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraComboEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSource, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldFormat, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldExtent, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl2:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl2:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldDescription, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl3:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl3:PerformLayout().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeHidden, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlNodeType, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldSerializeName, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldXmlDataType, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabPageControl4:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl4:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        CAST(THIS-OBJECT, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Raises the Load event
	    Notes:
	    @param e The System.EventArgs with the data for this event
	------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
		
        DEFINE VARIABLE oValueList                      AS Infragistics.Win.ValueList                        NO-UNDO .
        DEFINE VARIABLE oAlternativeControlTypeProvider AS ISmartViewerBuilderAlternativeControlTypeProvider NO-UNDO .   
		
		SUPER:OnLoad (e) .

        oValueList = NEW Infragistics.Win.ValueList () . 
        oValueList:ValueListItems:Add ("":U) .                

        ASSIGN oAlternativeControlTypeProvider = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ISmartViewerBuilderAlternativeControlTypeProvider} .                
                
        IF VALID-OBJECT (oAlternativeControlTypeProvider) THEN 
            oAlternativeControlTypeProvider:AddCustomControlTypes (AlternativeControlTypeProviderOriginEnum:AblFrameMigrationForm,
                                                                   oValueList) .                
    
        eField_FieldCustomControlName:ValueList = oValueList .           

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves the current changes to the database.
        Notes:   This Method is called by the Toolbar Classes SmartToolbarController 
                 or SmartToolStripContainer by pressing the corresponding Tool.
                 Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PUBLIC VOID SaveChanges ():
		
        &IF NOT PROVERSION BEGINS "10" &THEN
        IF VALID-OBJECT (oValidationSettings) THEN 
            oValidationSettings:Assign () . 
        &ENDIF	    	
		
		SUPER:SaveChanges().

	END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the Leave event of the smartBusinessEntityBindingSource1_eField_FieldName 
	             Control 
	    Notes:   Builds a well formed label from a camel cased field name
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID smartBusinessEntityBindingSource1_eField_FieldName_Leave (sender AS System.Object, 
	                                                                              e AS System.EventArgs):
		
		DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.
		
		IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData AND 
		   THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:Text = "":U THEN DO:
		       
            ASSIGN cLabel = StringHelper:WellFormFromCamelCase (THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldName:Text) .
            
            THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldLabel:Text = cLabel .              		       
		END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the CheckedChanged event of the ultraCheckEditor2 
                 instance                                                                         
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID ultraCheckEditor2_CheckedChanged (sender AS System.Object, 
	                                                      e AS System.EventArgs):
		
		IF ultraCheckEditor2:Checked THEN 
		    THIS-OBJECT:SetControlEnabled (THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial,
		                                   ControlEnabledEnum:Never) .
        ELSE 
            THIS-OBJECT:SetControlEnabled (THIS-OBJECT:smartBusinessEntityBindingSource1_eField_FieldInitial,
                                           ControlEnabledEnum:Update) .        

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ValueChanged event of the ultraComboEditor2 
                 instance                                                                         
        Notes:           
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraComboEditor2_ValueChanged (sender AS System.Object, 
                                                        e AS System.EventArgs):
        
        IF NOT ultraComboEditor2:Focused THEN 
            RETURN .
            
        IF UNBOX (ultraComboEditor2:Value) = "PROTECTED":U AND UNBOX (ultraComboEditor3:Value) = "PUBLIC":U THEN 
            ultraComboEditor3:Value = BOX ("PROTECTED":U) .

        IF UNBOX (ultraComboEditor2:Value) = "PRIVATE":U AND UNBOX (ultraComboEditor3:Value) <> "PRIVATE":U THEN 
            ultraComboEditor3:Value = BOX ("PRIVATE":U) .

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Event handler for the ValueChanged event of the FieldValidationPropertiesControl
	    Notes:
	    @param sender The reference to the object that raised the event
	    @param e The Consultingwerk.EventArgs with the data for this event   
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ValueChangedHandler (sender AS Progress.Lang.Object,
	                                        e AS Consultingwerk.EventArgs):
		
		IF THIS-OBJECT:SmartTableIOState = TableIOStateEnum:FieldsEnabled THEN 
		    THIS-OBJECT:SmartTableIOState = TableIOStateEnum:ModifyingData .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the FieldViewerControl class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC FieldViewerControl ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
