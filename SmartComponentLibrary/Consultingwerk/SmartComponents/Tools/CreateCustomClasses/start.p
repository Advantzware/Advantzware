/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : start.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 23 22:49:28 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ***************************  Main Block  *************************** */

DEFINE VARIABLE cBaseFolder             AS CHARACTER NO-UNDO INIT "C:/Work/SmartComponents4NET/Trunk/ABL_Test":U .
DEFINE VARIABLE cOERAPackage            AS CHARACTER NO-UNDO INIT "Test.CustomClasses.OERA":U .
DEFINE VARIABLE cOERAPrefix             AS CHARACTER NO-UNDO INIT "Demo":U .

DEFINE VARIABLE cSmartComponentsPackage AS CHARACTER NO-UNDO INIT "Test.CustomClasses.SmartComponents":U .
DEFINE VARIABLE cSmartComponentsPrefix  AS CHARACTER NO-UNDO INIT "Demo":U .

DEFINE VARIABLE cAuthor                 AS CHARACTER NO-UNDO INIT "Customer Name":U .

DEFINE VARIABLE cOERA                   AS CHARACTER NO-UNDO INIT "OERA:":U                  FORMAT "x(30)":U .
DEFINE VARIABLE cSmartComponentLibrary  AS CHARACTER NO-UNDO INIT "SmartComponent Libray:":U FORMAT "x(30)":U .

DEFAULT-WINDOW:WIDTH = 150 . 

ASSIGN cBaseFolder = Consultingwerk.Util.SessionHelper:CurrentDirectory() . 

FORM cBaseFolder             LABEL "Base Folder":U FORMAT "x(80)":U SKIP (2) 
     cOERA                   NO-LABEL                               SKIP (0)
     cOERAPackage            LABEL "Package":U     FORMAT "x(80)":U SKIP         
     cOERAPrefix             LABEL "Prefix":U      FORMAT "x(20)":U SKIP (2)      
     cSmartComponentLibrary  NO-LABEL                               SKIP (0)
     cSmartComponentsPackage LABEL "Package":U     FORMAT "x(80)":U SKIP 
     cSmartComponentsPrefix  LABEL "Prefix":U      FORMAT "x(20)":U SKIP (2) 
       
     cAuthor                 LABEL "Author":U      FORMAT "x(80)":U
       
     WITH WIDTH 140 SIDE-LABEL 1 DOWN .

DISPL cOERA cSmartComponentLibrary .

UPDATE cBaseFolder             LABEL "Base Folder":U FORMAT "x(80)":U SKIP (2) 
       cOERAPackage            LABEL "Package":U     FORMAT "x(80)":U SKIP         
       cOERAPrefix             LABEL "Prefix":U      FORMAT "x(20)":U SKIP (2)      
       cSmartComponentsPackage LABEL "Package":U     FORMAT "x(80)":U SKIP 
       cSmartComponentsPrefix  LABEL "Prefix":U      FORMAT "x(20)":U SKIP (2) 
       
       cAuthor                 LABEL "Author":U      FORMAT "x(80)":U
       
       WITH WIDTH 140 SIDE-LABEL 1 DOWN .
       
       
NEW Consultingwerk.SmartComponents.Tools.CreateCustomClasses.CustomClassGenerator (cBaseFolder, 
                                                                                   cOERAPackage, 
                                                                                   cOERAPrefix, 
                                                                                   cSmartComponentsPackage, 
                                                                                   cSmartComponentsPrefix, 
                                                                                   cAuthor) . 
