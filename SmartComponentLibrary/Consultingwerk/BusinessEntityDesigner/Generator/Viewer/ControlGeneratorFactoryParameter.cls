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
    File        : ControlGeneratorFactoryParameter
    Purpose     : Parameter class to the GetControlGenerator method of the
                  IControlGeneratorFactoryExt service interface
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 24 19:03:24 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .  
USING Progress.Lang.*                                          FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.ControlGeneratorFactoryParameter 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:


    {Consultingwerk/JsonSerializableProperty.i ColumnName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i FieldName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i CustomControlName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DataType CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ControlGeneratorFactoryParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorFactoryParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ControlGeneratorFactoryParameter class
        Notes:   
        @param pcColumnName The name of the current BindingSource Column
        @param pcFieldName The name (including buffer) of the current business entity field
        @param pcCustomControlName The name of the custom control as specified in the field properties in the Business Entity Designer
        @param pcDataType The data type of the current BindingSource Column
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ControlGeneratorFactoryParameter (pcColumnName AS CHARACTER, 
                                                         pcFieldName AS CHARACTER,
                                                         pcCustomControlName AS CHARACTER,
                                                         pcDataType AS CHARACTER):
        THIS-OBJECT ().
        
        ASSIGN THIS-OBJECT:ColumnName        = pcColumnName
               THIS-OBJECT:FieldName         = pcFieldName
               THIS-OBJECT:CustomControlName = pcCustomControlName
               THIS-OBJECT:DataType          = pcDataType 
               .
        
    END CONSTRUCTOR.


END CLASS.
