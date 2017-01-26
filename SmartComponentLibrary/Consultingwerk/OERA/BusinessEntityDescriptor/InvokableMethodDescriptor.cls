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
    File        : InvokableMethodDescriptor
    Purpose     : Describes an Invokable Method
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

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.InvokableMethodDescriptor
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IInvokableMethodDescriptor
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i DatasetInput LOGICAL "INITIAL TRUE"} .
    {Consultingwerk/JsonSerializableProperty.i DatasetOutput LOGICAL "INITIAL TRUE"} .
    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ParameterClassName CHARACTER} .

    /**
     * Purpose: Constructor for the InvokableMethodDescriptor class
     * Notes:
     */
    CONSTRUCTOR PUBLIC InvokableMethodDescriptor ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the InvokableMethodDescriptor class
     * Notes:
     * @param pcName The name of the invokable method
     * @param pcParameterClassName The name of the parameter class
     * @param plDatasetInput Receive dataset from client
     * @param plDatasetOutput Return dataset to client
     */
    CONSTRUCTOR PUBLIC InvokableMethodDescriptor (pcName AS CHARACTER,
                                                  pcParameterClassName AS CHARACTER,
                                                  plDatasetInput AS LOGICAL,
                                                  plDatasetOutput AS LOGICAL):
        THIS-OBJECT () .

        ASSIGN THIS-OBJECT:Name               = pcName
               THIS-OBJECT:ParameterClassName = pcParameterClassName
               THIS-OBJECT:DatasetInput       = plDatasetInput
               THIS-OBJECT:DatasetOutput      = plDatasetOutput .

    END METHOD .

END CLASS.
