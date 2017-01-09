/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DateFormat
    Purpose     : Holder structure for SESSION date value settings
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Nov 15 23:11:47 CET 2012
    Notes       : Serializable class, so that it can be marshalled to an 
                  AppServer
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.* FROM PROPATH .  
USING Progress.Lang.*  FROM PROPATH .

CLASS Consultingwerk.DateFormat 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF   
    : 
    {Consultingwerk/JsonSerializableProperty.i DateFormat CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i YearOffset INTEGER} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DateFormat class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DateFormat ():
        SUPER ().

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .

    END CONSTRUCTOR.

END CLASS.
