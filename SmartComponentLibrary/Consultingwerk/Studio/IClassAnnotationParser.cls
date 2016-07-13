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
    File        : IClassAnnotationParser
    Purpose     : Interface for parsers for class and class member annotations
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 22 18:36:39 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Studio.* FROM PROPATH .  
USING Progress.Lang.*         FROM PROPATH .

INTERFACE Consultingwerk.Studio.IClassAnnotationParser:  
  
    /*------------------------------------------------------------------------------
        Purpose: Gets the List of Annotations of the ClassHeader
        Notes:   
        @return The ListAnnotation with the list of annotations from the class header 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListAnnotation GetClassAnnotations () .
  
    /*------------------------------------------------------------------------------
        Purpose: Gets the List of Class Members and their Annotations
        Notes:   It is only expected that members with annotations are returned
        @return The ListClassMember with the lsit of class members and their annotations
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC ListClassMember GetClassMemberAnnotations () .
  
  
  
END INTERFACE.