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
    File        : MinLengthValidationProperties
    Purpose     : Properties for MinLength Field Validation
    Syntax      :
    Description :
    Author(s)   : Mark Bartscherer / Consultingwerk Ltd.
    Created     : Wed Dec 21 14:17:16 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Util.* FROM PROPATH.
USING Consultingwerk.Exceptions.Exception.
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Validation.MinLengthValidationProperties
    INHERITS BaseValidationProperties
    IMPLEMENTS IHasTypeDescriptor
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Limit INTEGER "INIT 0"} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MinLengthValidationProperties class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MinLengthValidationProperties ():

        SUPER ().

        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

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

        RETURN SUBSTITUTE ("Consultingwerk.OERA.ValidationHelper:MinLength (BUFFER &1:HANDLE, &2:U, &3, &4).":U,
                           pcBufferName,
                           QUOTER(pcFieldName),
                           THIS-OBJECT:Limit,
                           cMessage) .

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Returns the Type Descriptor for the object instance
        Notes:
        @return The ITypeDescriptor for the object instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE ITypeDescriptor GetTypeDescriptor ():

        DEFINE VARIABLE oTypeDescriptor AS ITypeDescriptor NO-UNDO .

        oTypeDescriptor = SUPER:GetTypeDescriptor () .
        oTypeDescriptor:TypeName = "Consultingwerk.BusinessEntityDesigner.Validation.MinLengthValidationProperties":U .

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.MinLengthValidationProperties":U,
                                                 "Limit":U,
                                                 "Minimum Length"{&TRAN}) .

        RETURN oTypeDescriptor .

    END METHOD.

END CLASS.
