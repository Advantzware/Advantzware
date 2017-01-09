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
    File        : create-initial-smartuser-data.p
    Purpose     : Creates an initial admin user in the SmartDB.SmartUser 
                  table and ensures that the user is assigned to the Admin 
                  group 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu May 21 09:11:31 CEST 2015
    Notes       : Does also ensure that the default Admin group has the default
                  menu structure assigned 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*        FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .

DEFINE VARIABLE cMenuGuids AS CHARACTER NO-UNDO EXTENT 2 INIT 
        ["d5f16ac1-54ae-52b7-e211-391384a9e03a":U, 
         "fafd8128-fc6d-8eaf-e311-cd051a3c1fa6":U].

DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* *****************  Seek existing control data  ********************** */

FIND SmartLanguage WHERE SmartLanguage.LanguageIsoCode = "EN":U NO-LOCK . 

/* *********************  Default Admin User  ************************* */

FIND SmartUser WHERE SmartUser.UserName         = "admin":U 
                 AND SmartUser.LoginCompanyGuid = "":U EXCLUSIVE-LOCK NO-ERROR .
                 
IF NOT AVAILABLE SmartUser THEN DO:
    CREATE SmartUser . 
    ASSIGN SmartUser.UserName         = "admin":U 
           SmartUser.LoginCompanyGuid = "":U
           SmartUser.UserFullName     = "Default Admin User":U . 
END.    

ASSIGN SmartUser.LanguageGuid            = SmartLanguage.LanguageGuid 
       SmartUser.UserPassword            = ENCODE ("password":U)
       SmartUser.UserPasswordChangedDate = NOW . 

FIND CURRENT SmartUser NO-LOCK . 

/* ************************  Demo Company  **************************** */

FIND FIRST SmartLoginCompany WHERE SmartLoginCompany.LoginCompanyShort = "demo":U NO-LOCK NO-ERROR .

IF NOT AVAILABLE SmartLoginCompany THEN DO:
    CREATE SmartLoginCompany.
    ASSIGN SmartLoginCompany.LoginCompanyShort = "demo":U
           SmartLoginCompany.LoginCompanyName  = "Demo Company":U .
END.    

FIND CURRENT SmartLoginCompany NO-LOCK . 

/* ************  Include Admin User in default Admin Group ************* */

FIND SmartGroup WHERE SmartGroup.GroupName = "Admin":U NO-LOCK . /* this record is expected to be created by the SmartDB migration script */

FIND SmartUserGroup WHERE SmartUserGroup.UserGuid  = SmartUser.UserGuid 
                      AND SmartUserGroup.GroupGuid = SmartGroup.GroupGuid NO-LOCK NO-ERROR . 
                      
IF NOT AVAILABLE SmartUserGroup THEN DO:
    CREATE SmartUserGroup.
    ASSIGN SmartUserGroup.UserGuid  = SmartUser.UserGuid 
           SmartUserGroup.GroupGuid = SmartGroup.GroupGuid .
             
    FIND CURRENT SmartUserGroup NO-LOCK .     
END.                            

/* ****  Ensure Admin group is linked to default menu structures  ****** */

DO i = 1 TO EXTENT (cMenuGuids):
    
    IF NOT CAN-FIND (SmartMenuGroup WHERE SmartMenuGroup.GroupGuid = SmartGroup.GroupGuid
                                      AND SmartMenuGroup.MenuGuid  = cMenuGuids[i]) THEN DO: 
    
        CREATE SmartMenuGroup .
        ASSIGN SmartMenuGroup.GroupGuid = SmartGroup.GroupGuid
               SmartMenuGroup.MenuGuid  = cMenuGuids[i] .
               
        RELEASE SmartMenuGroup .                 
    END.
END.

CATCH err AS Progress.Lang.Error:
	ErrorHelper:ShowErrorMessage (err) .	
END CATCH.
                  
