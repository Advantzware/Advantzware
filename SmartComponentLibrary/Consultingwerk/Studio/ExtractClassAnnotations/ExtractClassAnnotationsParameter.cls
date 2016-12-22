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
    File        : ExtractClassAnnotationsParameter
    Purpose     : Parameter class for the ExtractClassAnnotationsJob
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 01 18:04:31 CEST 2015
    Notes       : SCL-846
                  http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.Studio.ExtractClassAnnotations.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Studio.ExtractClassAnnotations.ExtractClassAnnotationsParameter 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i Directory CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i FileMask CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i ExcludeAnnotations CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i OverwriteWriteProtected LOGICAL} .
    {Consultingwerk/JsonSerializableProperty.i Verbose LOGICAL} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ExtractClassAnnotationsParameter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ExtractClassAnnotationsParameter ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

    END CONSTRUCTOR. 

END CLASS.
