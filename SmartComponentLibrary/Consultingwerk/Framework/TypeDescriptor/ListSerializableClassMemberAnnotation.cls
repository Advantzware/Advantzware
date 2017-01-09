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
    File        : ListSerializableClassMemberAnnotation
    Purpose     : Generic list of SerializableClassMemberAnnotation instances
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

CLASS Consultingwerk.Framework.TypeDescriptor.ListSerializableClassMemberAnnotation 
    INHERITS GenericList
    {&SERIALIZABLE}: 

    {Consultingwerk/Framework/Base/GenericList.i SerializableClassMemberAnnotation}
 
    /*------------------------------------------------------------------------------
        Purpose: Finds a class member by name
        Notes:   Returns the first matching member
        @param pcName The name of the class member to return
        @return The SerializableClassMemberAnnotation with the given name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC SerializableClassMemberAnnotation FindByName (pcName AS CHARACTER):
        
        {Consultingwerk/foreachABL.i SerializableClassMemberAnnotation oAnnotation in THIS-OBJECT}
        
            IF oAnnotation:Name = pcName THEN 
                RETURN oAnnotation .
        END.

        RETURN ? . 

    END METHOD .

END CLASS.
