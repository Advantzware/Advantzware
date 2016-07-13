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
    File        : ClassAnnotationContainer
    Purpose     : Holder class for the annotations of a class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 01 15:10:15 CEST 2015
    Notes       : Serializable class, purpose to be serialized to .annotations
                  files
                  http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                          FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Framework.TypeDescriptor.ClassAnnotationContainer 
&IF '{&FrameworkSerializationType}' EQ 'XML' OR PROVERSION BEGINS '10.2':U &THEN
    INHERITS XmlSerializable
&ELSE
    INHERITS JsonSerializable
&ENDIF
    {&SERIALIZABLE}:

    {Consultingwerk/JsonSerializableProperty.i ClassName CHARACTER} .
    {Consultingwerk/JsonSerializableProperty.i TimeStamp DATETIME-TZ} .
    {Consultingwerk/JsonSerializableProperty.i Annotations Consultingwerk.Framework.TypeDescriptor.ListSerializableAnnotation} .
    {Consultingwerk/JsonSerializableProperty.i Constructors Consultingwerk.Framework.TypeDescriptor.ListSerializableClassMemberAnnotation} .
    {Consultingwerk/JsonSerializableProperty.i Destructor Consultingwerk.Framework.TypeDescriptor.SerializableClassMemberAnnotation} .
    {Consultingwerk/JsonSerializableProperty.i Events Consultingwerk.Framework.TypeDescriptor.ListSerializableClassMemberAnnotation} .
    {Consultingwerk/JsonSerializableProperty.i Methods Consultingwerk.Framework.TypeDescriptor.ListSerializableClassMemberAnnotation} .
    {Consultingwerk/JsonSerializableProperty.i Properties Consultingwerk.Framework.TypeDescriptor.ListSerializableClassMemberAnnotation} .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassAnnotationContainer class
        Notes:   
        @param pcClassName The class name that is described by these annotations
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ClassAnnotationContainer (pcClassName AS CHARACTER):
		THIS-OBJECT ().
		
		ASSIGN THIS-OBJECT:ClassName = pcClassName
		       THIS-OBJECT:TimeStamp = NOW  . 
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassAnnotationContainer class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassAnnotationContainer ():
        SUPER ().
        THIS-OBJECT:AddSerializableProperties ('{&SerializableProperties}':U) .

        THIS-OBJECT:Annotations = NEW ListSerializableAnnotation () . 
        THIS-OBJECT:Constructors = NEW ListSerializableClassMemberAnnotation () . 
        THIS-OBJECT:Events = NEW ListSerializableClassMemberAnnotation () . 
        THIS-OBJECT:Methods = NEW ListSerializableClassMemberAnnotation () . 
        THIS-OBJECT:Properties = NEW ListSerializableClassMemberAnnotation () . 

    END CONSTRUCTOR. 

END CLASS.
