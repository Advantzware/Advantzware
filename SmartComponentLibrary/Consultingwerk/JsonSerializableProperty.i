&IF 1=0 &THEN
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
    File        : JsonSerializableProperty.i
    Purpose     : Defines a Serializable Property in a child class of 
                  Consultingwerk.JsonSerializable

    Syntax      : {Consultingwerk/JsonSerializableProperty.i Name Data-Type "Other Definition"}
    
                  Dynamic extent Array-Support, currently only for array's of objects, not primitives:
                  {Consultingwerk/JsonSerializableProperty.i Name "Data-Type EXTENT" "Other Definition"}
    
                  The third parameter (Other Definition) is optional

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 29 19:32:14 CEST 2012
    Notes       : You are not supposed to use the SerializableProperties 
                  for other purposes than using it in the JsonSerializable
                  base class - as we may be changing the format of this 
                  property without warning.
  ----------------------------------------------------------------------*/
&ENDIF

&IF "{&FrameworkSerializationType}" EQ "XML" &THEN
{Consultingwerk/SerializableProperty.i "{1}" "{2}"}
&ELSE


    DEFINE PUBLIC PROPERTY {1} AS {2} NO-UNDO {3} 
    GET.
    SET.
    
&IF "{&SerializableProperties}":U NE "":U &THEN
&GLOBAL-DEFINE SerializableProperties {&SerializableProperties},{1},{2}
&ELSE
&GLOBAL-DEFINE SerializableProperties {1},{2}
&ENDIF

&ENDIF