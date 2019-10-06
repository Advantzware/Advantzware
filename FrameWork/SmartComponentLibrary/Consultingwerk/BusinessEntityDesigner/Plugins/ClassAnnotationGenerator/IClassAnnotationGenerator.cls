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
    File        : IClassAnnotationGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 14 21:26:27 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.* FROM PROPATH .  
USING Progress.Lang.*                                                          FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.IClassAnnotationGenerator:  

    /*------------------------------------------------------------------------------
        Purpose: Generates file annotations
        Notes:   
        @param pcDirectory The directory to generate the class annotations from 
        @param pcFileMask The file mask of the .cls files to process
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GenerateClassAnnotations (pcDirectory AS CHARACTER, 
                                                 pcFileMask AS CHARACTER).

END INTERFACE.
