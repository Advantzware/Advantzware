/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BusinessEntityTableDescriptor
    Purpose     : Describes a Entity Temp-Table
    Syntax      :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 08 16:10:02 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.BusinessEntityTableDescriptor
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IBusinessEntityTableDescriptor
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Fields Consultingwerk.OERA.BusinessEntityDescriptor.ListBusinessEntityFieldDescriptor} .

    {Consultingwerk/JsonSerializableProperty.i MandatoryColumns CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i DefaultSort CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i PrimaryKey CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ReadOnly LOGICAL "INITIAL FALSE"} .
    {Consultingwerk/JsonSerializableProperty.i ReadOnlyColumns CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i NonFilterableColumns CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i NonSortableColumns CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityTableDescriptor class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityTableDescriptor ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:Fields = NEW ListBusinessEntityFieldDescriptor () .

    END CONSTRUCTOR.

END CLASS.
