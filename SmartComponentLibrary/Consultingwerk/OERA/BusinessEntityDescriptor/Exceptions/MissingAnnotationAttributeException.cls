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
    File        : MissingAnnotationAttributeException
    Purpose     : Exception thrown when mantatory annotation attributes
                  are missing when building the Business Entity Descritor
                  from class annotations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 12 14:04:51 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.BusinessEntityDescriptor.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                                           FROM PROPATH .

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.Exceptions.MissingAnnotationAttributeException 
    INHERITS BusinessEntityDescriptorException: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the missing annotation attribute 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AnnotationAttributeName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BusinessEntityDescriptorException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcBusinessEntityName The name of the Business Entity associated with this error
        @param pcAttributeName The name of the missing annotation attribute
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MissingAnnotationAttributeException (pcErrorString AS CHARACTER, 
                                                            piMessageNumber AS INTEGER, 
                                                            pcBusinessEntityName AS CHARACTER,
                                                            pcAttributeName AS CHARACTER):
        
        SUPER (pcErrorString, piMessageNumber, pcBusinessEntityName).
        
        THIS-OBJECT:AnnotationAttributeName = pcAttributeName . 
    
    END CONSTRUCTOR.

END CLASS.
