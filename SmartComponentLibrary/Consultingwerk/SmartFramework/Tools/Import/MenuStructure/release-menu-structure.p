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
    File        : release-menu-structure.p
    Purpose     : Import menu structure during update process

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 12 13:48:13 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Tools.Import.MenuStructure.* FROM PROPATH . 
USING Consultingwerk.Util.*                                      FROM PROPATH . 

DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO FORMAT "x(80)":U LABEL "Source File":U                INIT "c:/temp/menustructure.xml":U .
DEFINE VARIABLE lOverwriteExisting AS LOGICAL   NO-UNDO                  LABEL "Overwrite existing records":U INIT TRUE.

DEFINE VARIABLE oImporter AS MenuStructureImporter NO-UNDO . 

/* ***************************  Main Block  *************************** */

ASSIGN lOverwriteExisting = DataTypeHelper:ToLogical (DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                                                        "Overwrite":U)) 
       cFileName    = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                        "SourceFile":U) .


FILE-INFORMATION:FILE-NAME = cFileName .
ASSIGN cFileName = FILE-INFORMATION:FULL-PATHNAME . 

PUT UNFORMATTED "Loading menu structure from: ":U cFileName SKIP (0). 
PUT UNFORMATTED "Overwrite existing data: ":U lOverwriteExisting SKIP (1). 

oImporter = NEW MenuStructureImporter () . 
oImporter:ImportMenuStructure (cFileName, lOverwriteExisting) .

RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled error:":U SKIP 
            ErrorHelper:FormattedErrorMessagesExt (err) . 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.       
       