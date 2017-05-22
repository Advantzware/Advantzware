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
    File        : SerializableProperty.i
    Purpose     : Defines a Serializable Property in a child class of 
                  Consultingwerk.Serializable or Consultingwerk.XmlSerializable

    Syntax      : {Consultingwerk/SerializableProperty.i Name Data-Type "Other Definition"}
                  The third parameter (Other Definition) is optional

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 17 14:18:14 CEST 2010
    Notes       : The XmlSerializable class does also support nested objects, 
                  where the second parameter does not equal a built in ABL
                  primitive data type, the Consultingwerk.Serializable class
                  however does 
  ----------------------------------------------------------------------*/
&ENDIF
&IF "{2}" = "CHARACTER" OR
    "{2}" = "DATE" OR
    "{2}" = "DATETIME" OR
    "{2}" = "DATETIME-TZ" OR
    "{2}" = "DECIMAL" OR
    "{2}" = "INT64" OR
    "{2}" = "INTEGER" OR
    "{2}" = "LOGICAL" OR
    "{2}" = "LONGCHAR" OR 
    "{2}" = "RAW" OR 
    "{2}" = "ROWID" &THEN
    DEFINE PUBLIC PROPERTY {1} AS {2} NO-UNDO {3} 
    GET():
        RETURN THIS-OBJECT:Get{2}Property ("{1}":U) .
    END GET.
    SET(INPUT arg AS {2}):
        THIS-OBJECT:Set{2}Property ("{1}":U, arg) .                
    END SET.
&ELSE
    DEFINE PUBLIC PROPERTY {1} AS {2} NO-UNDO {3} 
    GET():
        RETURN CAST (THIS-OBJECT:GetObjectProperty ("{1}":U),
                     {2}) .
    END GET.
    SET(INPUT arg AS {2}):
        THIS-OBJECT:SetObjectProperty ("{1}":U, arg) .                
    END SET.
&ENDIF

&IF "{&SerializableProperties}":U NE "":U &THEN
&GLOBAL-DEFINE SerializableProperties {&SerializableProperties},{1},{2}
&ELSE
&GLOBAL-DEFINE SerializableProperties {1},{2}
&ENDIF
    