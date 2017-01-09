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
    File        : export-menu-structure.p
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

DEFINE VARIABLE cParentGuids AS CHARACTER NO-UNDO FORMAT "x(800)":U LABEL "Parent GUID's":U VIEW-AS EDITOR SIZE 60 BY 10.
DEFINE VARIABLE cFileName    AS CHARACTER NO-UNDO FORMAT "x(80)":U  LABEL "Target File":U   INIT "c:/temp/menustructure.xml":U .

DEFINE VARIABLE oExporter AS MenuStructureExporter NO-UNDO . 

/* ***************************  Main Block  *************************** */

DEFAULT-WINDOW:WIDTH = 120 . 

UPDATE cParentGuids SKIP  
       cFileName 
       WITH WIDTH 100 1 DOWN . 

oExporter = NEW MenuStructureExporter () . 
oExporter:ExportMenuStructure (cParentGuids, cFileName) .

CATCH err AS Progress.Lang.Error:
    Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
END CATCH.       
       