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
    File        : SmartViewerBuilder
    Purpose     : Initializes IControlGenerator instances for the ISmartViewerGenerator
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 21 11:16:31 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.BusinessEntityDesigner.*                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.*        FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                   FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*          FROM PROPATH .        
USING Consultingwerk.Util.*                                    FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .
USING Progress.Util.*                                          FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.SmartViewerBuilder: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the BindingSource that defines the schema 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS SmartBusinessEntityBindingSource NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:ClassName . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:ClassName = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the generated User Control should contain a 
                 Binding Source or not
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CreateBindingSource AS LOGICAL INITIAL TRUE NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:CreateBindingSource . 
    END GET . 
    SET (arg AS LOGICAL):
        THIS-OBJECT:Generator:CreateBindingSource = arg . 
    END SET . 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityJoin property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:EntityJoin . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:EntityJoin = arg . 
    END SET .  
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityName property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:EntityName . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:EntityName = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityTable property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:EntityTable . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:EntityTable = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the EntityView property of the SmartBusinessEntityBindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:EntityView . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:EntityView = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartViewerControl
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PackageName AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:PackageName . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:PackageName = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Generator instance 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Generator AS ISmartViewerGenerator NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartViewerBuilder class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartViewerBuilder ():
        SUPER ().
        
        THIS-OBJECT:Generator = NEW SmartViewerGenerator () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assigns the events form the Viewer Field Spec to the Editor
        Notes:
        @param poViewerFieldSpec The viewer field spec 
        @param poControlGenerator The reference to the control generator
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AssignEvents (poViewerFieldSpec  AS ViewerFieldSpec,
                                        poControlGenerator AS IControlGenerator):
        
        IF poViewerFieldSpec:ClickTriggerBlock > "":U AND TYPE-OF (poControlGenerator,
                                                                   IHasClickEvent) THEN 
            ASSIGN CAST (poControlGenerator, IHasClickEvent):ClickTriggerBlock = poViewerFieldSpec:ClickTriggerBlock .                                                                    

        IF poViewerFieldSpec:ValidatingTriggerBlock > "":U AND TYPE-OF (poControlGenerator,
                                                                        IHasValidatingEvent) THEN 
            ASSIGN CAST (poControlGenerator, IHasValidatingEvent):ValidatingTriggerBlock = poViewerFieldSpec:ValidatingTriggerBlock .                                                                 

        IF poViewerFieldSpec:ValueChangedTriggerBlock > "":U AND TYPE-OF (poControlGenerator,
                                                                          IHasValueChangedEvent) THEN 
            ASSIGN CAST (poControlGenerator, IHasValueChangedEvent):ValueChangedTriggerBlock = poViewerFieldSpec:ValueChangedTriggerBlock .                                                                 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Height of the given control generator instance
        Notes:
        @param poControl The Control generator instance to return the height for
        @return The height used for the generated control
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED INTEGER ControlHeight (poControl AS Progress.Lang.Object):
        
        IF TYPE-OF (poControl, IHasWidthAndHeight) THEN
            RETURN CAST (poControl, IHasWidthAndHeight):Height . 
        ELSE 
            RETURN 21 . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Iterates the List of Controls and checks if controls are surrounded
                 by a GroupBox 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FindContainedControls ():
        
        {Consultingwerk/foreachABL.i IControlGenerator oContainer in THIS-OBJECT:Generator:ControlGenerators}

            IF NOT TYPE-OF (oContainer, UltraGroupBoxControlGenerator) THEN 
                NEXT .  

            {Consultingwerk/foreachABL.i IControlGenerator oControl in THIS-OBJECT:Generator:ControlGenerators}

                IF oControl = oContainer THEN 
                    NEXT . 

                /* Control is already in a container */
                IF oControl:ContainerName > "":U THEN 
                    NEXT . 
                
                IF TYPE-OF (oControl, IHasWidthAndHeight) THEN DO:

                    IF THIS-OBJECT:PointIsInRectangle (oControl:Left,
                                                       oControl:Top,
                                                       oContainer:Left,
                                                       oContainer:Top,
                                                       CAST (oContainer, UltraGroupBoxControlGenerator):Width,
                                                       CAST (oContainer, UltraGroupBoxControlGenerator):Height) AND 
                                                                                                  
                       THIS-OBJECT:PointIsInRectangle (oControl:Left + CAST (oControl, IHasWidthAndHeight):Width,
                                                       oControl:Top + CAST (oControl, IHasWidthAndHeight):Height,
                                                       oContainer:Left,
                                                       oContainer:Top,
                                                       CAST (oContainer, UltraGroupBoxControlGenerator):Width,
                                                       CAST (oContainer, UltraGroupBoxControlGenerator):Height) THEN 
                        
                        ASSIGN oControl:ContainerName = oContainer:Name
                               oControl:Left          = oControl:Left - oContainer:Left 
                               oControl:Top           = oControl:Top - oContainer:Top 
                               .                                                         
                END.
                ELSE DO:
                    IF THIS-OBJECT:PointIsInRectangle (oControl:Left,
                                                       oControl:Top,
                                                       oContainer:Left,
                                                       oContainer:Top,
                                                       CAST (oContainer, UltraGroupBoxControlGenerator):Width,
                                                       CAST (oContainer, UltraGroupBoxControlGenerator):Height) THEN 
                        
                        ASSIGN oControl:ContainerName = oContainer:Name                                                         
                               oControl:Left          = oControl:Left - oContainer:Left 
                               oControl:Top           = oControl:Top - oContainer:Top 
                               . 
                END.
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the SmartViewer
        Notes:   This version of the method is used by the Business Entity Designer 
                 plugin to generate a viewer
        @param poColumns The list of Columns to add
        @param poCustomControlNames The dictionary of custom control names in the Business Entity Design
        @return The file name of the generated SmartViewerControl
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate (poColumns AS CharacterList,
                                      poCustomControlNames AS CharacterDictionary):
        
        DEFINE VARIABLE oLabel                      AS UltraLabelControlGenerator          NO-UNDO .
        DEFINE VARIABLE oField                      AS UltraEditorControlGenerator         NO-UNDO .
        DEFINE VARIABLE oNumericField               AS UltraNumericEditorControlGenerator  NO-UNDO .
        DEFINE VARIABLE oLogicalField               AS UltraCheckEditorControlGenerator    NO-UNDO .
        DEFINE VARIABLE oDateTimeField              AS UltraDateTimeEditorControlGenerator NO-UNDO .
        DEFINE VARIABLE i                           AS INTEGER                             NO-UNDO .
        DEFINE VARIABLE iTop                        AS INTEGER                             NO-UNDO INITIAL 20 .
        DEFINE VARIABLE iHeight                     AS INTEGER                             NO-UNDO .
        
        DEFINE VARIABLE cFieldName                  AS CHARACTER                           NO-UNDO .
        DEFINE VARIABLE cCustomControl              AS CHARACTER                           NO-UNDO .
        DEFINE VARIABLE oCustomControlGenerator     AS IControlGenerator                   NO-UNDO . 
        DEFINE VARIABLE oControlGeneratorFactory    AS IControlGeneratorFactory            NO-UNDO . 
        DEFINE VARIABLE oControlGeneratorFactoryExt AS IControlGeneratorFactoryExt         NO-UNDO . 
        
        oControlGeneratorFactory = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Generator.Viewer.IControlGeneratorFactory} .

        IF NOT VALID-OBJECT (oControlGeneratorFactory) THEN 
            oControlGeneratorFactoryExt = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Generator.Viewer.IControlGeneratorFactoryExt} .
        
        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in THIS-OBJECT:BindingSource:TableSchema:Columns}
            THIS-OBJECT:Generator:BindingSourceColumns:Add (oColumn) .
        END.

        {Consultingwerk/foreachPrimitiveList.i Character cColumn in poColumns}
            
            ASSIGN cFieldName = BindingSourceHelper:BindingBufferFieldName (THIS-OBJECT:BindingSource, cColumn) .
            
            IF poCustomControlNames:ContainsKey (cFieldName) THEN 
                ASSIGN cCustomControl = poCustomControlNames:GetValue (cFieldName) .
            ELSE 
                ASSIGN cCustomControl = "":U .
                        
            oColumn = THIS-OBJECT:GetColumn (cColumn) .
            
            /* Label */            
            oLabel = NEW UltraLabelControlGenerator () .
            oLabel:Left = 20 .
            oLabel:Name = SUBSTITUTE ("&1_&2_Label":U, THIS-OBJECT:EntityTable, cColumn) .
            oLabel:ContainerControlGenerator = THIS-OBJECT:Generator .
            oLabel:TabOrder = i * 2 + 1 .
            oLabel:Top = iTop + 4 .
            oLabel:Text = oColumn:ColumnLabel .  
            
            THIS-OBJECT:Generator:ControlGenerators:Add (oLabel) .

            /* Custom control generator */
            ASSIGN oCustomControlGenerator = ? . 
            
            IF VALID-OBJECT (oControlGeneratorFactory) THEN 
                oCustomControlGenerator = oControlGeneratorFactory:GetControlGenerator (THIS-OBJECT, 
                                                                                        cColumn,
                                                                                        cFieldName,
                                                                                        cCustomControl) . 
            ELSE 
            IF VALID-OBJECT (oControlGeneratorFactoryExt) THEN 
                oCustomControlGenerator = oControlGeneratorFactoryExt:GetControlGenerator (THIS-OBJECT, 
                                                                                           NEW ControlGeneratorFactoryParameter (cColumn,
                                                                                                                                 cFieldName,
                                                                                                                                 cCustomControl, 
                                                                                                                                 oColumn:ColumnType:ToString())) . 
            
            IF VALID-OBJECT (oCustomControlGenerator) THEN DO:

                oCustomControlGenerator:DataBindings = cColumn .
                oCustomControlGenerator:Left = 130 . 
                oCustomControlGenerator:Name = SUBSTITUTE ("&1_&2":U, THIS-OBJECT:EntityTable, cColumn) .
                oCustomControlGenerator:ContainerControlGenerator = THIS-OBJECT:Generator .
                oCustomControlGenerator:TabOrder = i * 2 + 2 .
                oCustomControlGenerator:Top = iTop.
    
                iHeight = THIS-OBJECT:ControlHeight (oCustomControlGenerator) .
  
                THIS-OBJECT:Generator:ControlGenerators:Add (oCustomControlGenerator) .
            END.
            ELSE DO:
                CASE oColumn:ColumnType:ToString():
                    WHEN "INTEGER":U OR WHEN "INT64":U OR WHEN "DECIMAL":U THEN DO:
                        /* UltraNumericEditor */
                        oNumericField = NEW UltraNumericEditorControlGenerator () .
                        oNumericField:DataBindings = cColumn .
                        oNumericField:Left = 130 . 
                        
                        IF EnumHelper:AreEqual (oColumn:ColumnType,
                                                Progress.Data.DataType:DECIMAL) THEN 
                            oNumericField:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Double . 
                        
                        oNumericField:Name = SUBSTITUTE ("&1_&2":U, THIS-OBJECT:EntityTable, cColumn) .
                        oNumericField:ContainerControlGenerator = THIS-OBJECT:Generator .
                        oNumericField:TabOrder = i * 2 + 2 .
                        oNumericField:Top = iTop .                     
    
                        iHeight = THIS-OBJECT:ControlHeight (oNumericField) .
                    
                        THIS-OBJECT:Generator:ControlGenerators:Add (oNumericField) .                    
                    END.
                    WHEN "LOGICAL":U THEN DO:
                        /* UltraCheckEditor */
                        oLogicalField = NEW UltraCheckEditorControlGenerator () .
                        oLogicalField:DataBindings = cColumn .
                        oLogicalField:Left = 130 . 
                        oLogicalField:Name = SUBSTITUTE ("&1_&2":U, THIS-OBJECT:EntityTable, cColumn) .
                        oLogicalField:ContainerControlGenerator = THIS-OBJECT:Generator .
                        oLogicalField:TabOrder = i * 2 + 2 .
                        oLogicalField:Text = oColumn:ColumnLabel . 
                        oLogicalField:Top = iTop .

                        iHeight = THIS-OBJECT:ControlHeight (oLogicalField) .
            
                        THIS-OBJECT:Generator:ControlGenerators:Add (oLogicalField) .
                    END.
                    WHEN "DATE":U OR WHEN "DATETIME":U THEN DO:
                        /* UltraDateTimeEditor */
                        oDateTimeField = NEW UltraDateTimeEditorControlGenerator () .
                        oDateTimeField:DataBindings = cColumn .
                        oDateTimeField:Left = 130 . 
                        oDateTimeField:Name = SUBSTITUTE ("&1_&2":U, THIS-OBJECT:EntityTable, cColumn) .
                        oDateTimeField:ContainerControlGenerator = THIS-OBJECT:Generator .
                        oDateTimeField:TabOrder = i * 2 + 2 .
                        oDateTimeField:Top = iTop .
            
                        iHeight = THIS-OBJECT:ControlHeight (oDateTimeField) .
            
                        THIS-OBJECT:Generator:ControlGenerators:Add (oDateTimeField) .
                    END.
                    OTHERWISE DO:
                        /* TextBox */
                        oField = NEW UltraEditorControlGenerator () .
                        oField:DataBindings = cColumn .
                        oField:Left = 130 . 
                        oField:Name = SUBSTITUTE ("&1_&2":U, THIS-OBJECT:EntityTable, cColumn) .
                        oField:ContainerControlGenerator = THIS-OBJECT:Generator .
                        oField:TabOrder = i * 2 + 2 .
                        oField:Top = iTop.
            
                        iHeight = THIS-OBJECT:ControlHeight (oField) .
            
                        THIS-OBJECT:Generator:ControlGenerators:Add (oField) .
                    END.
                    
                END CASE . 
            END .
            
            i = i + 1 .
            
            ASSIGN iTop = iTop + iHeight + 5 . 
        END.

        RETURN THIS-OBJECT:Generator:Generate () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the SmartViewer based on a List of ViewerFieldSpec objects
        Notes:   This version of the Generate method is used by tools like the AblFrameMigrationForm
        @param poList The list of ViewerFieldSpec objects to add
        @return The file name of the generated SmartViewerControl
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate (poList AS ListViewerFieldSpec):
        
        DEFINE VARIABLE oLabel                      AS UltraLabelControlGenerator                NO-UNDO .
        DEFINE VARIABLE oField                      AS UltraEditorControlGenerator               NO-UNDO .
        DEFINE VARIABLE oButton                     AS UltraButtonControlGenerator               NO-UNDO .
        DEFINE VARIABLE oGroupBox                   AS UltraGroupBoxControlGenerator             NO-UNDO .  
        DEFINE VARIABLE oNumericField               AS UltraNumericEditorControlGenerator        NO-UNDO .
        DEFINE VARIABLE oLogicalField               AS UltraCheckEditorControlGenerator          NO-UNDO .
        DEFINE VARIABLE oDateTimeField              AS UltraDateTimeEditorControlGenerator       NO-UNDO .
        DEFINE VARIABLE oOptionSet                  AS UltraOptionSetControlGenerator            NO-UNDO . 
        DEFINE VARIABLE oComboBox                   AS UltraComboEditorControlGenerator          NO-UNDO . 
        DEFINE VARIABLE oLookup                     AS SmartBusinessEntityLookupControlGenerator NO-UNDO . 
        DEFINE VARIABLE oComboEditor                AS SmartComboEditorControlGenerator          NO-UNDO . 
        DEFINE VARIABLE i                           AS INTEGER                                   NO-UNDO .
        DEFINE VARIABLE cColumn                     AS CHARACTER                                 NO-UNDO .
        DEFINE VARIABLE iLiteral                    AS INTEGER                                   NO-UNDO .
        DEFINE VARIABLE oValueListItem              AS Infragistics.Win.ValueListItem            NO-UNDO .
        DEFINE VARIABLE iItem                       AS INTEGER                                   NO-UNDO . 
        
        DEFINE VARIABLE oCustomControlGenerator     AS IControlGenerator                         NO-UNDO .
        DEFINE VARIABLE oControlGeneratorFactory    AS IControlGeneratorFactory                  NO-UNDO .
        DEFINE VARIABLE oControlGeneratorFactoryExt AS IControlGeneratorFactoryExt               NO-UNDO .

        oControlGeneratorFactory = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Generator.Viewer.IControlGeneratorFactory} .
        
        IF NOT VALID-OBJECT (oControlGeneratorFactory) THEN 
            oControlGeneratorFactoryExt = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Generator.Viewer.IControlGeneratorFactoryExt} .
        
        IF THIS-OBJECT:CreateBindingSource THEN DO:
            {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in THIS-OBJECT:BindingSource:TableSchema:Columns}
                THIS-OBJECT:Generator:BindingSourceColumns:Add (oColumn) .
            END.
        END.

        {Consultingwerk/foreachABL.i ViewerFieldSpec oViewerFieldSpec in poList}
            
            ASSIGN cColumn = oViewerFieldSpec:ColumnName .
            
            /* Mike Fechner, Consultingwerk Ltd. 19.08.2014
               SCL-376: Ability to create User Control with no binding source */
            IF THIS-OBJECT:CreateBindingSource = TRUE THEN 
                oColumn = THIS-OBJECT:GetColumn (cColumn) .

            /* Custom control generator */
            ASSIGN oCustomControlGenerator = ? .

            IF VALID-OBJECT (oControlGeneratorFactory) THEN 
                oCustomControlGenerator = oControlGeneratorFactory:GetControlGenerator (THIS-OBJECT, 
                                                                                        oViewerFieldSpec:ColumnName,
                                                                                        oViewerFieldSpec:ControlName,
                                                                                        oViewerFieldSpec:AlternativeControlType) . 
            ELSE 
            IF VALID-OBJECT (oControlGeneratorFactoryExt) THEN 
                oCustomControlGenerator = oControlGeneratorFactoryExt:GetControlGenerator (THIS-OBJECT, 
                                                                                           NEW ControlGeneratorFactoryParameter (oViewerFieldSpec:ColumnName,
                                                                                                                                 oViewerFieldSpec:ControlName,
                                                                                                                                 oViewerFieldSpec:AlternativeControlType,
                                                                                                                                 oViewerFieldSpec:DataType)) . 

            IF oViewerFieldSpec:NoLabel = FALSE AND oViewerFieldSpec:WidgetType <> "LITERAL":U THEN DO: 
                /* Label */            
                oLabel = NEW UltraLabelControlGenerator () .
                oLabel:Left = MAX (0, oViewerFieldSpec:Left - 60) .
                oLabel:Name = SUBSTITUTE ("&1_Label":U, oViewerFieldSpec:ControlName) .
                oLabel:ContainerControlGenerator = THIS-OBJECT:Generator .
                oLabel:TabOrder = i * 2 + 1 .
                oLabel:Tag = oViewerFieldSpec:PrivateData .
                oLabel:Top = oViewerFieldSpec:Top + 4 .
            
                IF oViewerFieldSpec:FieldLabel > "":U THEN 
                    oLabel:Text = oViewerFieldSpec:FieldLabel .  
                ELSE IF VALID-OBJECT (oColumn) THEN 
                    oLabel:Text = oColumn:ColumnLabel .  
                ELSE 
                    oLabel:Text = "<no label>":U . 
                 
                THIS-OBJECT:Generator:ControlGenerators:Add (oLabel) .
            END.
            
            /* Mike Fechner, Consultingwerk Ltd. 24.11.2014
               SCL-545 : Support for custom control generator */
            IF VALID-OBJECT (oCustomControlGenerator) THEN DO:

                IF VALID-OBJECT (oColumn) THEN 
                    oCustomControlGenerator:DataBindings = cColumn .
                    
                oCustomControlGenerator:Left = oViewerFieldSpec:Left . 
                oCustomControlGenerator:Name = SUBSTITUTE ("&3&1_&2&4":U, THIS-OBJECT:EntityTable, 
                                                                          cColumn,
                                                                          BusinessEntityDesignerSettings:DefaultTablePrefix,
                                                                          BusinessEntityDesignerSettings:DefaultTableSuffix) .
                oCustomControlGenerator:ContainerControlGenerator = THIS-OBJECT:Generator .
                oCustomControlGenerator:TabOrder = i * 2 + 2 .
                oCustomControlGenerator:Tag = oViewerFieldSpec:PrivateData . 
                oCustomControlGenerator:Top = oViewerFieldSpec:Top .

                THIS-OBJECT:Generator:ControlGenerators:Add (oCustomControlGenerator) .
                
                THIS-OBJECT:AssignEvents (oViewerFieldSpec, oCustomControlGenerator) .   
            END. /* custom control generator */
            ELSE DO:
                CASE oViewerFieldSpec:WidgetType:
                    WHEN "BUTTON":U THEN DO:
                        oButton = NEW UltraButtonControlGenerator () . 
                        
                        oButton:Left = oViewerFieldSpec:Left . 
                        oButton:Name = oViewerFieldSpec:ControlName .
                        oButton:ContainerControlGenerator = THIS-OBJECT:Generator .
                        oButton:TabOrder = i * 2 + 2 .
                        oButton:Tag = oViewerFieldSpec:PrivateData .
                        oButton:Top = oViewerFieldSpec:Top.        
                        oButton:Width = oViewerFieldSpec:Width .               
                        oButton:Height = oViewerFieldSpec:Height .               
    
                        THIS-OBJECT:Generator:ControlGenerators:Add (oButton) .               
                        
                        THIS-OBJECT:AssignEvents (oViewerFieldSpec, oButton) .     
                    END. 
                    WHEN "LITERAL":U THEN DO:
                        ASSIGN iLiteral = iLiteral + 1.
                        
                        /* Label */            
                        oLabel = NEW UltraLabelControlGenerator () .
                        oLabel:AddColonToText = FALSE  .
                        oLabel:Left = oViewerFieldSpec:Left .
                        oLabel:Name = SUBSTITUTE ("literal_&1":U, iLiteral) .
                        oLabel:ContainerControlGenerator = THIS-OBJECT:Generator .
                        oLabel:TabOrder = i * 2 + 1 .
                        oLabel:Tag = oViewerFieldSpec:PrivateData .
                        oLabel:Top = oViewerFieldSpec:Top .
                    
                        oLabel:Text = oViewerFieldSpec:WidgetValue . 
                         
                        THIS-OBJECT:Generator:ControlGenerators:Add (oLabel) .
                    END.
                    WHEN "RECTANGLE":U THEN DO:
                        oGroupBox = NEW UltraGroupBoxControlGenerator () . 
    
                        oGroupBox:Left = oViewerFieldSpec:Left . 
                        oGroupBox:Name = oViewerFieldSpec:ControlName .
                        oGroupBox:ContainerControlGenerator = THIS-OBJECT:Generator .
                        oGroupBox:TabOrder = i * 2 + 2 .
                        oGroupBox:Tag = oViewerFieldSpec:PrivateData .
                        oGroupBox:Top = oViewerFieldSpec:Top.        
                        oGroupBox:Width = oViewerFieldSpec:Width .               
                        oGroupBox:Height = oViewerFieldSpec:Height .               
    
                        THIS-OBJECT:Generator:ControlGenerators:Add (oGroupBox) .                    
                    END.
                    OTHERWISE DO:
    
                        CASE oViewerFieldSpec:AlternativeControlType:
                            WHEN "SmartBusinessEntityLookup":U THEN DO:
                                /* Lookup */
                                oLookup = NEW SmartBusinessEntityLookupControlGenerator () .
                                
                                IF VALID-OBJECT (oColumn) THEN 
                                    oLookup:DataBindings = cColumn .
                                    
                                oLookup:Left = oViewerFieldSpec:Left . 
                                oLookup:Name = oViewerFieldSpec:ControlName .
                                oLookup:ContainerControlGenerator = THIS-OBJECT:Generator .
                                oLookup:TabOrder = i * 2 + 2 .
                                oLookup:Tag = oViewerFieldSpec:PrivateData .
                                oLookup:Top = oViewerFieldSpec:Top.
                                oLookup:Width = oViewerFieldSpec:Width .               
                                oLookup:Height = oViewerFieldSpec:Height .               
                    
                                THIS-OBJECT:Generator:ControlGenerators:Add (oLookup) .
                                
                                THIS-OBJECT:AssignEvents (oViewerFieldSpec, oLookup) .
                            END.
                            WHEN "SmartComboEditor":U THEN DO:
                                /* Combo */
                                oComboEditor = NEW SmartComboEditorControlGenerator () .
                                
                                IF VALID-OBJECT (oColumn) THEN 
                                    oComboEditor:DataBindings = cColumn .
                                    
                                oComboEditor:Left = oViewerFieldSpec:Left . 
                                oComboEditor:Name = oViewerFieldSpec:ControlName .
                                oComboEditor:ContainerControlGenerator = THIS-OBJECT:Generator .
                                oComboEditor:TabOrder = i * 2 + 2 .
                                oComboEditor:Tag = oViewerFieldSpec:PrivateData .
                                oComboEditor:Top = oViewerFieldSpec:Top.
                                oComboEditor:Width = oViewerFieldSpec:Width .               
                                oComboEditor:Height = oViewerFieldSpec:Height .               
                    
                                THIS-OBJECT:Generator:ControlGenerators:Add (oComboEditor) .
                                
                                THIS-OBJECT:AssignEvents (oViewerFieldSpec, oComboEditor) .
                            END . 
                          
                            OTHERWISE DO:
                                CASE oViewerFieldSpec:ViewAs:
                                    WHEN "RADIO-SET":U THEN DO:
                                        oOptionSet = NEW UltraOptionSetControlGenerator () . 
                                        
                                        IF VALID-OBJECT (oColumn) THEN 
                                            oOptionSet:DataBindings = cColumn .
                                        
                                        oOptionSet:Left = oViewerFieldSpec:Left . 
                                        oOptionSet:Name = oViewerFieldSpec:ControlName .
                                        oOptionSet:ContainerControlGenerator = THIS-OBJECT:Generator .
                                        oOptionSet:TabOrder = i * 2 + 2 .
                                        oOptionSet:Tag = oViewerFieldSpec:PrivateData .
                                        oOptionSet:Top = oViewerFieldSpec:Top.        
                                        oOptionSet:Width = oViewerFieldSpec:Width .               
                                        oOptionSet:Height = oViewerFieldSpec:Height .               
                    
                                        THIS-OBJECT:Generator:ControlGenerators:Add (oOptionSet) .          
                                
                                        THIS-OBJECT:AssignEvents (oViewerFieldSpec, oOptionSet) .                                    
                                       
                                        DO iItem = 1 TO NUM-ENTRIES (oViewerFieldSpec:RadioButtons, CHR (1)) BY 2:
                                            oValueListItem = NEW Infragistics.Win.ValueListItem () .
                                            oValueListItem:DisplayText = ENTRY (iItem, oViewerFieldSpec:RadioButtons, CHR (1)) .
                                            oValueListItem:DataValue = Consultingwerk.Util.DataTypeHelper:ToSystemObject (ENTRY (iItem + 1, oViewerFieldSpec:RadioButtons, CHR (1)),
                                                                                                                          oViewerFieldSpec:DataType) . 
                                            oOptionSet:Items:Add (oValueListItem) .    
                                        END.
                                    END.
                                    WHEN "COMBO-BOX":U THEN DO:
                                        oComboBox = NEW UltraComboEditorControlGenerator () . 
                                        
                                        IF VALID-OBJECT (oColumn) THEN 
                                            oComboBox:DataBindings = cColumn .
                                        
                                        oComboBox:Left = oViewerFieldSpec:Left . 
                                        oComboBox:Name = oViewerFieldSpec:ControlName .
                                        oComboBox:ContainerControlGenerator = THIS-OBJECT:Generator .
                                        oComboBox:TabOrder = i * 2 + 2 .
                                        oComboBox:Tag = oViewerFieldSpec:PrivateData .
                                        oComboBox:Top = oViewerFieldSpec:Top.        
                                        oComboBox:Width = oViewerFieldSpec:Width .               
                                        oComboBox:Height = oViewerFieldSpec:Height .               
                    
                                        THIS-OBJECT:Generator:ControlGenerators:Add (oComboBox) .          
    
                                        THIS-OBJECT:AssignEvents (oViewerFieldSpec, oComboBox) .
                                                                    
                                        IF oViewerFieldSpec:ListItems > "":U THEN 
                                            oComboBox:SetListItems (oViewerFieldSpec:ListItems) .
                                        ELSE IF oViewerFieldSpec:ListItemPairs > "":U THEN 
                                            oComboBox:SetListItemPairs (oViewerFieldSpec:ListItempairs) .
                                    END.
                                    WHEN "TEXT":U THEN DO:
                                        /* Label */            
                                        oLabel = NEW UltraLabelControlGenerator () .
                                        oLabel:AddColonToText = FALSE  .
                                        oLabel:Left = oViewerFieldSpec:Left .
                                        oLabel:Name = oViewerFieldSpec:ControlName .
                                        oLabel:ContainerControlGenerator = THIS-OBJECT:Generator .
                                        oLabel:TabOrder = i * 2 + 1 .
                                        oLabel:Tag = oViewerFieldSpec:PrivateData .
                                        oLabel:Top = oViewerFieldSpec:Top .
                                    
                                        oLabel:Text = oViewerFieldSpec:WidgetValue . 
                                         
                                        THIS-OBJECT:Generator:ControlGenerators:Add (oLabel) .
                                    END.
            
                                    OTHERWISE DO:
                                        /* EditorControl based on data type */
                                        CASE oViewerFieldSpec:DataType:
                                            WHEN "INTEGER":U OR WHEN "INT64":U OR WHEN "DECIMAL":U THEN DO:
                                                /* UltraNumericEditor */
                                                oNumericField = NEW UltraNumericEditorControlGenerator () .
                                                
                                                IF VALID-OBJECT (oColumn) THEN 
                                                    oNumericField:DataBindings = cColumn .
                                                    
                                                oNumericField:Left = oViewerFieldSpec:Left . 
                                                
                                                IF oViewerFieldSpec:DataType = "DECIMAL":U THEN  
                                                    oNumericField:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Double . 
                                                
                                                oNumericField:Name = oViewerFieldSpec:ControlName .
                                                oNumericField:ContainerControlGenerator = THIS-OBJECT:Generator .
                                                oNumericField:TabOrder = i * 2 + 2 .
                                                oNumericField:Tag = oViewerFieldSpec:PrivateData .
                                                oNumericField:Top = oViewerFieldSpec:Top.        
                                                oNumericField:Width = oViewerFieldSpec:Width .               
                                                oNumericField:Height = oViewerFieldSpec:Height .               
                            
                                                THIS-OBJECT:Generator:ControlGenerators:Add (oNumericField) .         
                                                
                                                THIS-OBJECT:AssignEvents (oViewerFieldSpec, oNumericField) .                                                        
                                            END.
                                            WHEN "LOGICAL":U THEN DO:
                                                /* UltraCheckEditor */
                                                oLogicalField = NEW UltraCheckEditorControlGenerator () .
                                                
                                                IF VALID-OBJECT (oColumn) THEN 
                                                    oLogicalField:DataBindings = cColumn .
                                                    
                                                oLogicalField:Left = oViewerFieldSpec:Left . 
                                                oLogicalField:Name = oViewerFieldSpec:ControlName .
                                                oLogicalField:ContainerControlGenerator = THIS-OBJECT:Generator .
                                                oLogicalField:TabOrder = i * 2 + 2 .
                                                oLogicalField:Tag = oViewerFieldSpec:PrivateData .
                                                oLogicalField:Text = oViewerFieldSpec:FieldLabel . 
                                                oLogicalField:Top = oViewerFieldSpec:Top.
                                    
                                                THIS-OBJECT:Generator:ControlGenerators:Add (oLogicalField) .
                                                
                                                THIS-OBJECT:AssignEvents (oViewerFieldSpec, oLogicalField) .
                                            END.
                                            WHEN "DATE":U OR WHEN "DATETIME":U THEN DO:
                                                /* UltraDateTimeEditor */
                                                oDateTimeField = NEW UltraDateTimeEditorControlGenerator () .
                                                
                                                IF VALID-OBJECT (oColumn) THEN 
                                                    oDateTimeField:DataBindings = cColumn .
                                                    
                                                oDateTimeField:Left = oViewerFieldSpec:Left . 
                                                oDateTimeField:Name = oViewerFieldSpec:ControlName .
                                                oDateTimeField:ContainerControlGenerator = THIS-OBJECT:Generator .
                                                oDateTimeField:TabOrder = i * 2 + 2 .
                                                oDateTimeField:Tag = oViewerFieldSpec:PrivateData .
                                                oDateTimeField:Top = oViewerFieldSpec:Top.
                                                oDateTimeField:Width = oViewerFieldSpec:Width .               
                                                oDateTimeField:Height = oViewerFieldSpec:Height .               
                                    
                                                THIS-OBJECT:Generator:ControlGenerators:Add (oDateTimeField) .
                                                
                                                THIS-OBJECT:AssignEvents (oViewerFieldSpec, oDateTimeField) .                                            
                                            END.
                                            OTHERWISE DO:
                                                /* TextBox */
                                                oField = NEW UltraEditorControlGenerator () .
                                                
                                                IF VALID-OBJECT (oColumn) THEN 
                                                    oField:DataBindings = cColumn .
                                                    
                                                oField:Left = oViewerFieldSpec:Left . 
                                                oField:Name = oViewerFieldSpec:ControlName .
                                                oField:ContainerControlGenerator = THIS-OBJECT:Generator .
                                                oField:TabOrder = i * 2 + 2 .
                                                oField:Tag = oViewerFieldSpec:PrivateData .
                                                oField:Top = oViewerFieldSpec:Top.
                                                oField:Width = oViewerFieldSpec:Width .               
                                                oField:Height = oViewerFieldSpec:Height .               
                                    
                                                THIS-OBJECT:Generator:ControlGenerators:Add (oField) .
                                                
                                                THIS-OBJECT:AssignEvents (oViewerFieldSpec, oField) .                                            
                                            END.
                                        END CASE . 
                                        
                                    END.
                                END CASE .                            
                            END.                        
                        END CASE . 
    
                    END.
                END CASE . 
            END. /* custom control generator */
            
            i = i + 1 . 
        END.

        THIS-OBJECT:FindContainedControls () . 

        RETURN THIS-OBJECT:Generator:Generate () .

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ColumnProp Desc for the given Column
        Notes:   
        @param pcColumnName
        @return The Progress.Data.ColumnPropDesc for the named Column
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED Progress.Data.ColumnPropDesc GetColumn (pcColumnName AS CHARACTER):
        
        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in THIS-OBJECT:BindingSource:TableSchema:Columns}
        
            IF oColumn:ColumnName = pcColumnName THEN 
                RETURN oColumn . 
        END. 

        RETURN ? . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Verifies if a Point is in a Rectangle 
        Notes:   
        @param piPointLeft The left coordinate of the point
        @param piPointTop The top coordinate of the point 
        @param piRectLeft The left coordinate of the point
        @param piRectTop The top coordinate of the point
        @param piRectWidth The witdh of the point 
        @param piRectHeight The height of the point
        @return Logical value indicating if the point is located within the rectangle
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL PointIsInRectangle (piPointLeft AS INTEGER,
                                                 piPointTop AS INTEGER, 
                                                 piRectLeft AS INTEGER,
                                                 piRectTop AS INTEGER, 
                                                 piRectWidth AS INTEGER, 
                                                 piRectHeight AS INTEGER):
        
        RETURN (piPointLeft >= piRectLeft AND piPointLeft <= piRectLeft + piRectWidth) AND 
               (piPointTop >= piRectTop   AND piPointTop  <= piRectTop + piRectHeight)  .

    END METHOD .
 
END CLASS.
