/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ttAuthorizationMaintenance.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 21.05.2014 11:17:03
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttAuthorizationMaintenance NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE ttAuthorizationMaintenanceBefore &ENDIF
    FIELD SecurityItemGuid AS CHARACTER FORMAT "x(36)":U LABEL "Security Item":T
    FIELD Restricted AS LOGICAL EXTENT 20 FORMAT "yes/no":U LABEL "Restricted":T
    FIELD DefaultLevel AS CHARACTER EXTENT 20 FORMAT "X(20)":U INIT "DEFUNRES":U LABEL "Default Level":T

    INDEX SecurityItemGuid AS UNIQUE PRIMARY SecurityItemGuid ASCENDING

    .
