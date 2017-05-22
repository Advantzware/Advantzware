/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : RestServletRequest
    Purpose     : Structured holder class for the data of the Servlet 
                  Request
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 18 17:04:19 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.OERA.RestAdapter.*      FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.OERA.RestAdapter.RestServletRequest 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    :

    {Consultingwerk/JsonSerializableProperty.i AuthType CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ContextPath CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i Method CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i PathInfo CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i PathTranslated CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i QueryString CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i RemoteUser CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i RequestedSessionId CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i RequestURI CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i RequestURL CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ServletPath CHARACTER} .
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RestServletRequest class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RestServletRequest ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
