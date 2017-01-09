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
    File        : TemplateHelper
    Purpose     : Simplifies location of Templates for the Business Entity 
                  Designer related code generators
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 10 15:47:31 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.* FROM PROPATH .
USING Consultingwerk.Util.*                   FROM PROPATH .  
USING Progress.Lang.*                         FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.TemplateHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the TemplateHelper class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE TemplateHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Locates a template file either in the custom template folder or
                 the standard template folder
        Notes:   
        @param pcTemplate The relative name of the template file
        @return The absolute file name of the template
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER Find (pcTemplate AS CHARACTER):
        
        DEFINE VARIABLE cTemplate AS CHARACTER NO-UNDO.
        
        ASSIGN cTemplate = FileHelper:FindFile (SUBSTITUTE ("&1/&2":U, 
                                                            BusinessEntityDesignerSettings:TemplateFolder,
                                                            pcTemplate)) . 
        
        IF cTemplate <> ? THEN 
            RETURN cTemplate .

        ASSIGN cTemplate = FileHelper:FindFile (SUBSTITUTE ("Consultingwerk/BusinessEntityDesigner/Generator/Templates/&1":U, 
                                                            pcTemplate)) . 
        
        IF cTemplate <> ? THEN 
            RETURN cTemplate .

        UNDO, THROW NEW AppError (SUBSTITUTE ("The template &1 could not be found!"{&TRAN}, pcTemplate), 0) . 

    END METHOD .

END CLASS.
