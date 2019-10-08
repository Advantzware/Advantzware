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
    File        : BaseComparisonValidationProperties
    Purpose     : Base Class for Validation of Comparisons (LE. LT, GE, GT, EQ, NE)
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Thu Dec 29 16:09:19 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Util.*                              FROM PROPATH .
USING Consultingwerk.Exceptions.*                        FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.*          FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH .
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Validation.BaseComparisonValidationProperties
    INHERITS BaseValidationProperties
    IMPLEMENTS IHasTypeDescriptor
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Value CHARACTER "INIT ?"} .

    DEFINE VARIABLE cCommand    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClassType  AS CHARACTER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BaseComparisonValidationProperties class
        Notes:
        @param pcCommand   The 4GL Command which is used for the comparison
        @param pcClassType The Class which is used for the PropertyDescriptors
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BaseComparisonValidationProperties (pcCommand   AS CHARACTER,
                                                           pcClassType AS CHARACTER):
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        ASSIGN
            cCommand   = pcCommand
            cClassType = pcClassType
            .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Source code for the validation
        Notes:
        @param pcBufferName The name of the buffer to validate
        @param pcFieldName The name of the field to validate
        @param pcDataType The data type of the temp-table field
        @param piExtent The field extent
        @return The source code represented by this validation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE LONGCHAR GenerateCode (pcBufferName AS CHARACTER,
                                                  pcFieldName AS CHARACTER,
                                                  pcDataType AS CHARACTER,
                                                  piExtent AS INTEGER):

        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

        ASSIGN cMessage = THIS-OBJECT:GetMessage (pcBufferName, pcFieldName, pcDataType, piExtent) .

        CASE pcDataType:
            WHEN "CHARACTER":U THEN
                RETURN SUBSTITUTE ("Consultingwerk.OERA.ValidationHelper:&5 (BUFFER &1:HANDLE, ~"&2~":U, &3, &4).":U,
                                   pcBufferName,
                                   pcFieldName,
                                   QUOTER(THIS-OBJECT:Value),
                                   cMessage,
                                   cCommand) .

            WHEN "DECIMAL":U OR WHEN "INT64":U OR WHEN "INTEGER":U OR
            WHEN "DATE":U OR WHEN "DATETIME":U OR WHEN "DATETIME-TZ":U THEN
                RETURN SUBSTITUTE ("Consultingwerk.OERA.ValidationHelper:&6 (BUFFER &1:HANDLE, ~"&2~":U, &5 (&3), &4).":U,
                                   pcBufferName,
                                   pcFieldName,
                                   QUOTER(THIS-OBJECT:Value),
                                   cMessage,
                                   pcDataType,
                                   cCommand) .

            OTHERWISE
                UNDO, THROW NEW Exception (SUBSTITUTE ("Unable to generate &1 validation for field of type &2"{&TRAN},
                                                       ClassHelper:ShortClassName(THIS-OBJECT:GetClass()),
                                                       pcDataType), 0)  .
        END CASE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Type Descriptor for the object instance
        Notes:
        @return The ITypeDescriptor for the object instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE ITypeDescriptor GetTypeDescriptor ():

        DEFINE VARIABLE oTypeDescriptor AS ITypeDescriptor NO-UNDO .

        oTypeDescriptor = SUPER:GetTypeDescriptor () .
        oTypeDescriptor:TypeName = cClassType.

        oTypeDescriptor:PropertyDescriptors:Add (cClassType,
                                                 "Value":U,
                                                 "Value to Compare"{&TRAN}) .

        RETURN oTypeDescriptor .

    END METHOD.

END CLASS.
