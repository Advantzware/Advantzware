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
    File        : NamedQueryParameter
    Purpose     : Parameter class for named queries
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 13 12:38:18 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*      FROM PROPATH .
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.NamedQueryParameter 
&IF "{&FrameworkSerializationType}" EQ "XML" OR PROVERSION BEGINS "10.2":U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF    
    IMPLEMENTS INamedQueryParameter
    {&SERIALIZABLE}: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the query 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Name CHARACTER} .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Dictionary of Named Query Parameters 
        Notes:   
    ------------------------------------------------------------------------------*/
    {Consultingwerk/JsonSerializableProperty.i Parameters Consultingwerk.OERA.NamedQueryParameterList} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NamedQueryParameter class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NamedQueryParameter ():
        SUPER ().

        THIS-OBJECT:Parameters = NEW NamedQueryParameterList () .

        THIS-OBJECT:AddSerializableProperties ("{&SerializableProperties}":U) .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the NamedQueryParameter class
        Notes:
        @param pcName The value for the name property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC NamedQueryParameter (pcName AS CHARACTER):
        THIS-OBJECT ().
    
        ASSIGN THIS-OBJECT:Name = pcName . 
    
    END CONSTRUCTOR .

END CLASS.
