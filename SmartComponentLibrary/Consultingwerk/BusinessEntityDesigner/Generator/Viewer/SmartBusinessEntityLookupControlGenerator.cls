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
    File        : SmartBusinessEntityLookupControlGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 17 08:12:34 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*        FROM PROPATH .  
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .  
USING Consultingwerk.Util.*                                    FROM PROPATH . 
USING Progress.Lang.*                                          FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.SmartBusinessEntityLookupControlGenerator
    INHERITS ControlGenerator
    IMPLEMENTS IControlGenerator, IHasWidthAndHeight, IHasValidatingEvent, IHasValueChangedEvent:

    DEFINE VARIABLE cEditorButtonName AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Container Control 
        Notes:   Leave Empty if the Control is not part of a Container such as a 
                 GroupBox
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Retusn the TypeName for to be used for this UltraEditorControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlTypeName AS CHARACTER NO-UNDO INIT "Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityLookup":U
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: The DataBindigns for the generated Control
        Notes:   The ControlGenerator implementation determines the actual properties
                 that we'll bind to (Text, Value, ...). So this propery contains the 
                 names of the BindingSource Fields. Multiple field names are supported,
                 in that case we'll pass in a comma delimited list of property names
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataBindings AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Height of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Height AS INTEGER NO-UNDO INIT 21
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Position of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Left AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma delimited list of additional fields that 
                 will be retrieved by the lookup operations  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupAdditionalFields AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Columns (comma delimited list of Column Names) not to be shown in the 
                 LookupBrowser.
        Notes:   The value of this Property is defined during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserExcludeFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Columns (comma delimited list of Column Names) to be shown in the 
                 LookupBrowser.
        Notes:   The value of this Property is defined during DesignTime.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.    
        
    /*------------------------------------------------------------------------------
        Purpose: A comma seperated list of Control names. The value of the corresponding 
                 Columns gets filled into those Controls.
                 The value of this Property is defined during DesignTime. 
        Notes:   Corresponding to the LookupFields
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupControls AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Class Name of the Lookup Dialog Form
        Notes:   The class needs to implement IBusinessEntityLookupDialogForm and 
                 inherits System.Windows.Forms.Form      
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogClassName AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Fields that allow to Filter on the Lookup Browser Dialog
        Notes:   Comma-Delimited, first entry is default                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogFilterFields AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Operators for the LookupDialogFilterFields 
        Notes:   Comma-Delimited, first entry is default                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogFilterOperators AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: QuerySort used when opening the Lookup Browser Dialog
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogQuerySort AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: QueryString used when opening the Lookup Browser Dialog
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupDialogQueryString AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Dialog title for the LookupDialogForm.
                 The value of this Property is defined during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupDialogTitle AS CHARACTER INITIAL "Lookup"{&TRAN} NO-UNDO 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Name of the BusinessEntity for the Lookup.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityName AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: RootTable for this Instance of the BusinessEntityLookup.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityTable AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.    
        
    /*------------------------------------------------------------------------------
        Purpose: List of Tables to read with the EntityTable.
                 This Property is set by the Developer during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupEntityView AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: A comma seperated list of Column names. The value of those fields 
                 gets filled into the corresponding Controls of the Property 
                 LookupControls.
                 The value of this Property is defined during DesignTime.
        Notes:   Corresponding to the LookupControls
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupFields AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.    

      /*------------------------------------------------------------------------------
        Purpose: The value of the field which is returned by the SmartLookup.
                 The value of this Property is defined during DesignTime.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyField AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Gets/Sets the name of the Data Source column which is used for 
                 retrieving the LookupKeyValue property 
        Notes:   Data-Bindable Property   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LookupKeyValueColumn AS CHARACTER NO-UNDO INIT "":U
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: QuerySort used in PerformLookup method                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupQuerySort AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: QueryString used in PerformLookup method                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY LookupQueryString AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Control
        Notes:   Removes Designer unsafe characters in the Name
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO 
    GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:Name = DesignerHelper:DesignerSafeName (arg) . 
    END .  

    /*------------------------------------------------------------------------------
        Purpose: Logical value indicating if the Control supports the ISupportInitialize 
                 interface
        Notes:   Used for generating the BeginInit and EndInit method calls
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SupportInitialize AS LOGICAL NO-UNDO INIT TRUE 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: The TabOrder property of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TabOrder AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Tag property of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tag AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template File to be used by this class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TemplateFileName AS CHARACTER NO-UNDO INIT "UI/smartbusinessentitylookup.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template File to be used by this class
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY TemplateFileNameUnbound AS CHARACTER NO-UNDO INIT "UI/smartbusinessentitylookup_unbound.template":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Position of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Top AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Code block for the Validating (LEAVE) event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ValidatingTriggerBlock AS LONGCHAR NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Code block for the ValueChanged (VALUE-CHANGED) event
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ValueChangedTriggerBlock AS LONGCHAR NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: The Width of the Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Width AS INTEGER NO-UNDO INIT 150 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Generats the source code that assigns the property values for this Control
        Notes:   
        @return The source code for the code block that assigns the property values for this Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateAssignPropertiesBlock ():
        
        DEFINE VARIABLE lcCode      AS LONGCHAR  NO-UNDO .

        IF THIS-OBJECT:DataBindings > "":U THEN 
            FILE-INFO:FILE-NAME = TemplateHelper:Find (SmartBusinessEntityLookupControlGenerator:TemplateFileName) .
        ELSE 
            FILE-INFO:FILE-NAME = TemplateHelper:Find (SmartBusinessEntityLookupControlGenerator:TemplateFileNameUnbound) .

        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO lcCode . 
        
        ASSIGN lcCode = SUBSTITUTE (lcCode, 
                                    THIS-OBJECT:Name,
                                    THIS-OBJECT:DataBindings,
                                    "":U /* no appearance object */,
                                    THIS-OBJECT:Left,
                                    THIS-OBJECT:Top,
                                    THIS-OBJECT:TabOrder,
                                    cEditorButtonName) + "~n":U +
                                    (IF THIS-OBJECT:Tag > "":U THEN SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                                                    "        THIS-OBJECT:&1:Tag = &2:U.~n":U
                                                                                ELSE 
                                                                                    "        this-object:&1:Tag = &2:U.~n":U,
                                                                                THIS-OBJECT:Name, 
                                                                                QUOTER(THIS-OBJECT:Tag)) ELSE "":U)
                                    . 

        ASSIGN lcCode = REPLACE (REPLACE (lcCode, 
                                          "@Width@":U, 
                                          STRING (THIS-OBJECT:Width)), 
                                 "@Height@":U,
                                 STRING (THIS-OBJECT:Height)) .        
        
        ASSIGN lcCode = REPLACE (lcCode, "@LookupAdditionalFields@":U,      THIS-OBJECT:LookupAdditionalFields) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupBrowserExcludeFields@":U,  THIS-OBJECT:LookupBrowserExcludeFields) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupBrowserFields@":U,         THIS-OBJECT:LookupBrowserFields) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupControls@":U,              THIS-OBJECT:LookupControls) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupDialogClassName@":U,       THIS-OBJECT:LookupDialogClassName) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupDialogFilterFields@":U,    THIS-OBJECT:LookupDialogFilterFields) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupDialogFilterOperators@":U, THIS-OBJECT:LookupDialogFilterOperators) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupDialogQuerySort@":U,       THIS-OBJECT:LookupDialogQuerySort) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupDialogQueryString@":U,     THIS-OBJECT:LookupDialogQueryString) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupDialogTitle@":U,           THIS-OBJECT:LookupDialogTitle) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupEntityName@":U,            THIS-OBJECT:LookupEntityName) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupEntityTable@":U,           THIS-OBJECT:LookupEntityTable) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupEntityView@":U,            THIS-OBJECT:LookupEntityView) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupFields@":U,                THIS-OBJECT:LookupFields) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupKeyField@":U,              THIS-OBJECT:LookupKeyField) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupKeyValueColumn@":U,        THIS-OBJECT:LookupKeyValueColumn) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupQuerySort@":U,             THIS-OBJECT:LookupQuerySort) .
        ASSIGN lcCode = REPLACE (lcCode, "@LookupQueryString@":U,           THIS-OBJECT:LookupQueryString) .

        IF THIS-OBJECT:ValidatingTriggerBlock > "":U THEN DO:
            ASSIGN lcCode = lcCode + SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                    "        THIS-OBJECT:&1:Validating:Subscribe(&1_Validating).~n":U
                                                 ELSE
                                                    "        this-object:&1:Validating:Subscribe(&1_Validating).~n":U,
                                                 THIS-OBJECT:Name) .
                                                 
            THIS-OBJECT:InsertEventHandler (SUBSTITUTE ("&1_Validating":U, THIS-OBJECT:Name), 
                                            "Validating":U,
                                            THIS-OBJECT:Name,
                                            "System.ComponentModel.CancelEventArgs":U, 
                                            THIS-OBJECT:ValidatingTriggerBlock) .                                                 
        END.

        IF THIS-OBJECT:ValueChangedTriggerBlock > "":U THEN DO:
            ASSIGN lcCode = lcCode + SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                    "        THIS-OBJECT:&1:ValueChanged:Subscribe(&1_ValueChanged).~n":U
                                                 ELSE 
                                                    "        this-object:&1:ValueChanged:Subscribe(&1_ValueChanged).~n":U,
                                                 THIS-OBJECT:Name) .

            THIS-OBJECT:InsertEventHandler (SUBSTITUTE ("&1_ValueChanged":U, THIS-OBJECT:Name), 
                                            "ValueChanged":U,
                                            THIS-OBJECT:Name,
                                            "System.EventArgs":U, 
                                            THIS-OBJECT:ValueChangedTriggerBlock) .                                                 
        END.
        
        RETURN lcCode . 

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Generates the NEW statement used to create an intance of the Control
        Notes:   
        @return The source code for the NEW statement to create an instanc of the Control
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateControlNew ():
        
        RETURN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("        THIS-OBJECT:&1 = NEW &2().~n":U),
                           THIS-OBJECT:Name,
                           THIS-OBJECT:ControlTypeName) .
        
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the code required to build supporting objects, like 
                 Appearance objects, Editor-Buttons etc.
        Notes:   
        @return The source code for the supporting object definition and instantiation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateSupportingObjectDefinition ():

        ASSIGN cEditorButtonName  

&IF NOT PROVERSION BEGINS "10.2" &THEN
         = THIS-OBJECT:ContainerControlGenerator:GenerateObjectName ("editorButton":U). 
&ELSE        
         = CAST (THIS-OBJECT:ContainerControlGenerator, IContainerControlGenerator):GenerateObjectName ("editorButton":U). 
&ENDIF     

        IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
            RETURN SUBSTITUTE ("        @VisualDesigner.FormMember (NeedsInitialize=~"true~":U).":U + 
                               "        DEFINE VARIABLE &1 AS Infragistics.Win.UltraWinEditors.EditorButton NO-UNDO.":U + 
                               "        &1 = NEW Infragistics.Win.UltraWinEditors.EditorButton(~"ShowLookupDialog~":U).":U,
                               cEditorButtonName) .          
        ELSE 
            RETURN SUBSTITUTE ("        @VisualDesigner.FormMember (NeedsInitialize=~"true~":U).":U + 
                               "        define variable &1 as Infragistics.Win.UltraWinEditors.EditorButton no-undo.":U + 
                               "        &1 = new Infragistics.Win.UltraWinEditors.EditorButton(~"ShowLookupDialog~":u).":U,
                               cEditorButtonName) .          
                
    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the variable definition ABL statementfor the Control
        Notes:   
        @return The source code for the variable definition section
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GenerateVariableDefinition ():
        
        RETURN SUBSTITUTE (BusinessEntityGenerator:CaseKeywords ("    DEFINE PRIVATE VARIABLE &1 AS &2 NO-UNDO.~n":U),
                           THIS-OBJECT:Name,
                           THIS-OBJECT:ControlTypeName) .
        
    END METHOD .

END CLASS.
