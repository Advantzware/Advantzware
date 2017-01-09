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
    File        : ttUserGoups.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 13 21:46:59 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{Consultingwerk/SmartFramework/Authorization/eSmartGroup.i &REFERENCE-ONLY=REFERENCE-ONLY &NO-BEFORE=NO-BEFORE}

DEFINE TEMP-TABLE ttUserGroups NO-UNDO 
    LIKE eSmartGroup
    FIELD Order AS INTEGER 
    INDEX Order IS PRIMARY UNIQUE Order . 