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
    File        : SerializableLayoutElement
    Purpose     : JSON Serializable Layout Element
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 14:27:02 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Web2.Views.* FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Web2.Views.SerializableLayoutElement 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    IMPLEMENTS ISerializableLayoutElement:

    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Template CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i InstanceId CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Type Consultingwerk.Web2.Views.LayoutElementType} .
    {Consultingwerk/JsonSerializableProperty.i DataSourceId CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i NavigationId CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i TableIOSourceId CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Elements Consultingwerk.Web2.Views.ListSerializableLayoutElement} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SerializableLayoutElement class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SerializableLayoutElement ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:UseSerializedTypeInformation = FALSE . 
        
        /* New GUID for each instance - Id cannot contain hyphens (-) */
        THIS-OBJECT:InstanceId = REPLACE (GUID, "-":U, "":U) . 
        
        THIS-OBJECT:Elements = NEW ListSerializableLayoutElement () .

    END CONSTRUCTOR. 

END CLASS.
