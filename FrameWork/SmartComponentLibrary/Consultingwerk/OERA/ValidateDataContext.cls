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
    File        : ValidateDataContext
    Purpose     : Parameter object for the invokable version of the 
                  ValidateData method in the Business Entity 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Sep 23 02:09:39 CEST 2015
    Notes       : SCL-1030
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.ValidateDataContext 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS IValidateDataContext 
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the current field
        Notes:   Typically the field that caused the validation during a Leave event
                 May be empty
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i CurrentFieldName CHARACTER} . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma delimited list of fields that have been 
                 processed by the user and thus should be validated
        Notes:   May be empty
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i ProcessedFields CHARACTER} . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the comma delimited list of names of the tables to validate
        Notes:   CAN-DO Style syntax, e.g. *
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Tables CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValidateDataContext class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ValidateDataContext ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
