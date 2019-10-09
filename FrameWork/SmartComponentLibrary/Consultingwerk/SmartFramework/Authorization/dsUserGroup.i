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
    File        : dsUserGroup.i
    Purpose     : Business Entity for UserGroup

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories
    Created     : 16.01.2013 15:47:57
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsUserGroup

{ Consultingwerk/SmartFramework/Authorization/ePossibleAssignments.i }
{ Consultingwerk/SmartFramework/Authorization/eSmartUserGroup.i }


DEFINE {&ACCESS} DATASET dsUserGroup {&REFERENCE-ONLY} FOR ePossibleAssignments, eSmartUserGroup 

    .    
