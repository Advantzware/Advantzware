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
    File        : Column
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike
    Created     : Mon Oct 26 16:12:51 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.* FROM PROPATH .  
USING Progress.Lang.*                                                 FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.TelerikMobile.Column 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ColumnName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ColumnDataType CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ColumnLabel CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Column class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Column ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
