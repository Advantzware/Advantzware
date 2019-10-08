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
    File        : ListSerializableAnnotation
    Purpose     : Generic list of SerializableAnnotation instances
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 01 15:08:43 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/The+Annotation+based+Type+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Base.*           FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i} 

CLASS Consultingwerk.Framework.TypeDescriptor.ListSerializableAnnotation 
    INHERITS GenericList
    {&SERIALIZABLE}: 

    {Consultingwerk/Framework/Base/GenericList.i SerializableAnnotation}
 
    /*------------------------------------------------------------------------------
        Purpose: Finds an annotation by name
        Notes:   Returns the first matching annotation
        @param pcName The name of the annotation to return
        @return The SerializableAnnotation with the given name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SerializableAnnotation FindByName (pcName AS CHARACTER):
        
        {Consultingwerk/foreachABL.i SerializableAnnotation oAnnotation in THIS-OBJECT}
        
            IF oAnnotation:Name = pcName THEN 
                RETURN oAnnotation .
        END.

        RETURN ? . 

    END METHOD .
     
END CLASS.
