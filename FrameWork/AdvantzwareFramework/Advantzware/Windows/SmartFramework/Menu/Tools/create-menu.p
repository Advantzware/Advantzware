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
    File        : create-menu.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Feb 09 16:53:17 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.SmartFramework.* FROM PROPATH.

DEFINE VARIABLE oCallParameter AS RunProcedureCallParameter NO-UNDO .  

DEFINE VARIABLE cRootMenuGuid    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParentMenuGuid  AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

/* find the module record */
FIND SmartModule WHERE SmartModule.ModuleName = "legacy" .

/* create menu function record */
ASSIGN 
    oCallParameter = NEW RunProcedureCallParameter () 
    oCallParameter:AllowMultiple                        = FALSE 
    oCallParameter:ProcedureName                        = "windows/cust.w"
    oCallParameter:RunPersistent                        = TRUE
    oCallParameter:InitializeInternalProcedure          = "dispatch"
    oCallParameter:InitializeInternalProcedureParameter = "initialize"
    oCallParameter:ReactivateInternalProcedure          = "dispatch"
    oCallParameter:ReactivateInternalProcedureParameter = "view"
    .

CREATE SmartFunction . 
ASSIGN SmartFunction.FunctionName          = "Sample function"
       SmartFunction.FunctionDescription   = "Imported menu function" 
       SmartFunction.FunctionModuleGuid    = SmartModule.ModuleGuid 
       SmartFunction.FunctionCallParameter = oCallParameter:Serialize() .

FIND CURRENT SmartFunction NO-LOCK . 

/* Create/find menu structure */       
       
/* root menu structrue */
FIND SmartMenu WHERE SmartMenu.ParentMenuGuid = ""
                 AND SmartMenu.MenuName       = "Advantzware" NO-LOCK .

ASSIGN cRootMenuGuid = SmartMenu.MenuGuid . 

/* Find or create "master data" menu */

FIND SmartMenu WHERE SmartMenu.ParentMenuGuid = cRootMenuGuid
                 AND SmartMenu.MenuName       = "Master data" NO-ERROR .
       
IF NOT AVAILABLE SmartMenu THEN DO: 
    CREATE SmartMenu.
    ASSIGN SmartMenu.ParentMenuGuid         = cRootMenuGuid
           SmartMenu.MenuName               = "Master data"
           SmartMenu.MenuStructureType      = "Menu" .
    FIND CURRENT SmartMenu NO-LOCK . 
END.           
           
ASSIGN cParentMenuGuid = SmartMenu.MenuGuid .
       
/* Create an item menu structure under master data */       
CREATE SmartMenu.
ASSIGN SmartMenu.ParentMenuGuid         = cParentMenuGuid
       SmartMenu.MenuName               = "Customer"
       SmartMenu.MenuStructureType      = "Item" 
       SmartMenu.FunctionGuid           = SmartFunction.FunctionGuid .
       
       
       
       