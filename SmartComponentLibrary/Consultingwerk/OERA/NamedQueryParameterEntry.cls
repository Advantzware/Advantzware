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
    File        : NamedQueryParameterEntry
    Purpose     : Entry of the NamedQueryParameter list
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 13 13:09:53 CEST 2015
    Notes       : SCL-1018
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.NamedQueryParameterEntry 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Value Progress.Lang.Object} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NamedQueryParameterEntry class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NamedQueryParameterEntry ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NamedQueryParameterEntry class
        Notes:
        @param pcName The value for Name
        @param poValue The value for Value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NamedQueryParameterEntry (pcName AS CHARACTER,
                                                 poValue AS Progress.Lang.Object):
        THIS-OBJECT ().

        ASSIGN THIS-OBJECT:Name  = pcName 
               THIS-OBJECT:Value = poValue . 

    END CONSTRUCTOR. 

END CLASS.
