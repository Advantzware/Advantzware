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
    File        : BaseValidationProperties
    Purpose     : Base Properties for Field Validation
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 18 21:19:19 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Validation.* FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.*          FROM PROPATH .
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties
    INHERITS JsonSerializable
    IMPLEMENTS IHasTypeDescriptor
    {&SERIALIZABLE}
    ABSTRACT:

    {Consultingwerk/JsonSerializableProperty.i ValidationMessage CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SmartMessageGroup CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SmartMessageNumber INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BaseValidationProperties class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BaseValidationProperties ():
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
    METHOD PUBLIC ABSTRACT LONGCHAR GenerateCode (pcBufferName AS CHARACTER,
                                                  pcFieldName AS CHARACTER,
                                                  pcDataType AS CHARACTER,
                                                  piExtent AS INTEGER) .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Type Descriptor for the object instance
        Notes:
        @return The ITypeDescriptor for the object instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ITypeDescriptor GetTypeDescriptor ():

        DEFINE VARIABLE oTypeDescriptor AS TypeDescriptor NO-UNDO .

        oTypeDescriptor = NEW TypeDescriptor () .

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties":U,
                                                 "ValidationMessage":U,
                                                 "Validation Message"{&TRAN}) .

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties":U,
                                                 "SmartMessageGroup":U,
                                                 "Message Group"{&TRAN}) .

        oTypeDescriptor:PropertyDescriptors:Add ("Consultingwerk.BusinessEntityDesigner.Validation.BaseValidationProperties":U,
                                                 "SmartMessageNumber":U,
                                                 "Message Number"{&TRAN},
                                                 EditorTypeEnum:NumericInteger) .

        RETURN oTypeDescriptor .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Message Parameter for the validation routine
        Notes:   Either constant message string of call into MessageFormatter
        @param pcBufferName The name of the buffer to validate
        @param pcFieldName The name of the field to validate
        @param pcDataType The data type of the temp-table field
        @param piExtent The field extent
        @return The Message Parameter for the validation routine
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR GetMessage (pcBufferName AS CHARACTER,
                                       pcFieldName AS CHARACTER,
                                       pcDataType AS CHARACTER,
                                       piExtent AS INTEGER):

        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

        IF THIS-OBJECT:SmartMessageGroup > "":U AND
           THIS-OBJECT:SmartMessageNumber > 0 THEN DO:

            IF pcDataType = "CHARACTER":U THEN
                ASSIGN cMessage = SUBSTITUTE ("Consultingwerk.SmartFramework.MessageFormatter:GetMessage (&1:U, &2, ~"&3~":U, &4.&3)":U,
                                              QUOTER (THIS-OBJECT:SmartMessageGroup),
                                              THIS-OBJECT:SmartMessageNumber,
                                              pcFieldName,
                                              pcBufferName) .
            ELSE
                ASSIGN cMessage = SUBSTITUTE ("Consultingwerk.SmartFramework.MessageFormatter:GetMessage (&1:U, &2, ~"&3~":U, STRING (&4.&3))":U,
                                              QUOTER (THIS-OBJECT:SmartMessageGroup),
                                              THIS-OBJECT:SmartMessageNumber,
                                              pcFieldName,
                                              pcBufferName) .
        END.
        ELSE
            ASSIGN cMessage = QUOTER (THIS-OBJECT:ValidationMessage) + ":U":U.

        RETURN cMessage .

    END METHOD .

END CLASS.
