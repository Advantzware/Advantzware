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
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Aug 12 12:29:53 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Tools.Export.MenuStructure.* FROM PROPATH . 
USING Consultingwerk.Util.*                                      FROM PROPATH . 

DEFINE VARIABLE cParentGuids AS CHARACTER NO-UNDO FORMAT "x(80)":U LABEL "Parent GUID's":U .
DEFINE VARIABLE cFileName    AS CHARACTER NO-UNDO FORMAT "x(80)":U LABEL "Target File":U   INIT "c:/temp/menustructure.xml":U .

DEFINE VARIABLE oExporter AS MenuStructureExporter NO-UNDO . 

/* ***************************  Main Block  *************************** */

ASSIGN cParentGuids = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                        "ParentGuids":U) 
       cFileName    = DYNAMIC-FUNCTION ("getParameter":U IN SOURCE-PROCEDURE, 
                                        "DestFile":U) .

PUT UNFORMATTED "Dumping menu structure to: ":U cFileName SKIP (1). 

oExporter = NEW MenuStructureExporter () . 
oExporter:ExportMenuStructure (cParentGuids, cFileName) .

RETURN "0":U .

CATCH err AS Progress.Lang.Error:
    MESSAGE "Unhandled error:":U SKIP 
            ErrorHelper:FormattedErrorMessagesExt (err) . 
    
    RETURN "1":U . /* signal erorr */   
END CATCH.       
       