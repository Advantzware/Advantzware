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
    File        : IsRestrictedCheckParameter
    Purpose     : Parameter object for the IsRestrictedCheck method of 
                  the SecurityService
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 13 21:30:14 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.IsRestrictedCheckParameter 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i RealmCode CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i SecurityItemGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i UserGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i LoginCompanyGuid CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ReturnGroups LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i IsRestricted LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i Reason CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Runtime INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the IsRestrictedCheckParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC IsRestrictedCheckParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        ASSIGN THIS-OBJECT:ReturnGroups = FALSE 
               THIS-OBJECT:IsRestricted = ? 
               THIS-OBJECT:Reason       = ? . 

    END CONSTRUCTOR. 

END CLASS.
