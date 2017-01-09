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
    File        : ClassAnnotationGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 14 21:30:45 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.* FROM PROPATH .  
USING Consultingwerk.Exceptions.*                                              FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*                                      FROM PROPATH .
USING Consultingwerk.Util.*                                                    FROM PROPATH .
USING Progress.Lang.*                                                          FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.ClassAnnotationGenerator 
    IMPLEMENTS IClassAnnotationGenerator: 

    /*------------------------------------------------------------------------------
        Purpose: Generates file annotations
        Notes:   
        @param pcDirectory The directory to generate the class annotations from 
        @param pcFileMask The file mask of the .cls files to process
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GenerateClassAnnotations (pcDirectory AS CHARACTER, 
                                                 pcFileMask AS CHARACTER):
        
        DEFINE VARIABLE cEntityFileName AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cCommandLine    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cAntBat         AS CHARACTER NO-UNDO .

        DEFINE VARIABLE oExecuteDialog AS ExecuteShellCommandDialog NO-UNDO .

        ASSIGN cEntityFileName = SUBSTITUTE ("&1/&2":U,
                                             pcDirectory,
                                             pcFileMask) 
               cAntBat = BusinessEntityDesignerSettings:ApacheAntCommandLine .

        IF CharacterType:IsNullOrEmpty (cAntBat) THEN 
            ASSIGN cAntBat = "ant.bat":U .

        IF NOT FileHelper:Exists (cEntityFileName) THEN
            UNDO, THROW NEW ExceptionWithTitle ("Business Entity not found. Please generate source code first."{&TRAN},
                                                0,
                                                "Class Annotation Generator Plugin"{&TRAN}) .

        ASSIGN cCommandLine = SUBSTITUTE ('~"&1~" ExtractClassAnnotationsWithParameters -Ddirectory=~"&2~" -DfileMask=~"&3~"':U,
                                          cAntBat,
                                          pcDirectory,
                                          pcFileMask) .

        oExecuteDialog = NEW ExecuteShellCommandDialog () .
        oExecuteDialog:Execute (cCommandLine) .

    END METHOD .

END CLASS.
