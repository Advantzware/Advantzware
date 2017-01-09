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
    File        : SmartWindowBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 21 11:16:31 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.BusinessEntityDesigner.Generator.Form.* FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                 FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.*        FROM PROPATH .        
USING Progress.Lang.*                                        FROM PROPATH .
USING Progress.Util.*                                        FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Form.SmartWindowBuilder: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the BindingSource that defines the schema 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS SmartBusinessEntityBindingSource NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartWindowForm
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
        Purpose: Gets and sets the name of the Dataset Controller used
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY DatasetControllerName AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:DatasetControllerName . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:DatasetControllerName = arg . 
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
        Purpose: Gets and sets the Form Title (Text Property of the generated Form)
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY FormTitle AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:FormTitle . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:FormTitle = arg . 
    END SET .
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of Grid Columns
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY GridColumns AS CharacterList NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:GridColumns . 
    END GET . 
    SET (arg AS CharacterList):
        THIS-OBJECT:Generator:GridColumns = arg . 
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Class Name for the generated SmartWindowForm
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
        Purpose: Returns the Generator instance 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Generator AS ISmartWindowGenerator NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template to be used for generation  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ResxTemplateName AS CHARACTER INITIAL "":U NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:ResxTemplateName . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:ResxTemplateName = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Name of the Template to be used for generation  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TemplateName AS CHARACTER INITIAL "":U NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:TemplateName . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:TemplateName = arg . 
    END SET .  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the SmartViewer to be used in the SmartWindowForm
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ViewerName AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:Generator:ViewerName . 
    END GET . 
    SET (arg AS CHARACTER):
        THIS-OBJECT:Generator:ViewerName = arg . 
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartWindowBuilder class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWindowBuilder ():
        SUPER ().
        
        THIS-OBJECT:Generator = NEW SmartWindowGenerator () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Generates the SmartWindowForm 
        Notes:   
        @return The file name of the generated Form
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Generate ():
        
        DEFINE VARIABLE i              AS INTEGER                             NO-UNDO .

        {Consultingwerk/foreach.i Progress.Data.ColumnPropDesc oColumn in THIS-OBJECT:BindingSource:TableSchema:Columns}
            THIS-OBJECT:Generator:BindingSourceColumns:Add (oColumn) .
        END.

        {Consultingwerk/foreachPrimitiveList.i Character cColumn in THIS-OBJECT:GridColumns}

            oColumn = THIS-OBJECT:GetColumn (cColumn) .

/*            /* Label */                                                                                                 */
/*            oLabel = NEW UltraLabelControlGenerator () .                                                                */
/*            oLabel:Left = 20 .                                                                                          */
/*            oLabel:Name = SUBSTITUTE ("tb&1_Label":U, cColumn) .                                                        */
/*            oLabel:ContainerControlGenerator = THIS-OBJECT:Generator .                                                  */
/*            oLabel:TabOrder = i * 2 + 1 .                                                                               */
/*            oLabel:Top = 24 + (26 * i).                                                                                 */
/*            oLabel:Text = oColumn:ColumnLabel .                                                                         */
/*                                                                                                                        */
/*            THIS-OBJECT:Generator:ControlGenerators:Add (oLabel) .                                                      */
/*                                                                                                                        */
/*            CASE oColumn:ColumnType:ToString():                                                                         */
/*                WHEN "INTEGER":U OR WHEN "INT64":U OR WHEN "DECIMAL":U THEN DO:                                         */
/*                    oNumericField = NEW UltraNumericEditorControlGenerator () .                                         */
/*                    oNumericField:DataBindings = cColumn .                                                              */
/*                    oNumericField:Left = 130 .                                                                          */
/*                                                                                                                        */
/*                    IF EnumHelper:AreEqual (oColumn:ColumnType,                                                         */
/*                                            Progress.Data.DataType:DECIMAL) THEN                                        */
/*                        oNumericField:NumericType = Infragistics.Win.UltraWinEditors.NumericType:Double .               */
/*                                                                                                                        */
/*                    oNumericField:Name = SUBSTITUTE ("tb&1":U, cColumn)  .                                              */
/*                    oNumericField:ContainerControlGenerator = THIS-OBJECT:Generator .                                   */
/*                    oNumericField:TabOrder = i * 2 + 2 .                                                                */
/*                    oNumericField:TOP = 20 + (26 * i).                                                                  */
/*                                                                                                                        */
/*                    THIS-OBJECT:Generator:ControlGenerators:Add (oNumericField) .                                       */
/*                END.                                                                                                    */
/*                WHEN "LOGICAL":U THEN DO:                                                                               */
/*                    /* UltraCheckEditor */                                                                              */
/*                    oLogicalField = NEW UltraCheckEditorControlGenerator () .                                           */
/*                    oLogicalField:DataBindings = cColumn .                                                              */
/*                    oLogicalField:Left = 130 .                                                                          */
/*                    oLogicalField:Name = SUBSTITUTE ("tb&1":U, cColumn)  .                                              */
/*                    oLogicalField:ContainerControlGenerator = THIS-OBJECT:Generator .                                   */
/*                    oLogicalField:TabOrder = i * 2 + 2 .                                                                */
/*                    oLogicalField:Text = oColumn:ColumnLabel .                                                          */
/*                    oLogicalField:Top = 20 + (26 * i).                                                                  */
/*                                                                                                                        */
/*                    THIS-OBJECT:Generator:ControlGenerators:Add (oLogicalField) .                                       */
/*                END.                                                                                                    */
/*                WHEN "DATE":U OR WHEN "DATETIME":U THEN DO:                                                             */
/*                    oDateTimeField = NEW UltraDateTimeEditorControlGenerator () .                                       */
/*                    oDateTimeField:DataBindings = cColumn .                                                             */
/*                    oDateTimeField:Left = 130 .                                                                         */
/*                    oDateTimeField:Name = SUBSTITUTE ("tb&1":U, cColumn)  .                                             */
/*                    oDateTimeField:ContainerControlGenerator = THIS-OBJECT:Generator .                                  */
/*                    oDateTimeField:TabOrder = i * 2 + 2 .                                                               */
/*                    oDateTimeField:Top = 20 + (26 * i).                                                                 */
/*                                                                                                                        */
/*                    THIS-OBJECT:Generator:ControlGenerators:Add (oDateTimeField) .                                      */
/*                END.                                                                                                    */
/*                OTHERWISE DO:                                                                                           */
/*                    /* TextBox */                                                                                       */
/*                    oField = NEW UltraEditorControlGenerator () .                                                       */
/*                    oField:DataBindings = cColumn .                                                                     */
/*                    oField:Left = 130 .                                                                                 */
/*                    oField:Name = SUBSTITUTE ("tb&1":U, cColumn)  .                                                     */
/*                    oField:ContainerControlGenerator = THIS-OBJECT:Generator .                                          */
/*                    oField:TabOrder = i * 2 + 2 .                                                                       */
/*                    oField:Top = 20 + (26 * i).                                                                         */
/*                                                                                                                        */
/*                    THIS-OBJECT:Generator:ControlGenerators:Add (oField) .                                              */
/*                END.                                                                                                    */
/*                                                                                                                        */
/*            END CASE .                                                                                                  */
/*                                                                                                                        */
            i = i + 1 .
        END.

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

END CLASS.
