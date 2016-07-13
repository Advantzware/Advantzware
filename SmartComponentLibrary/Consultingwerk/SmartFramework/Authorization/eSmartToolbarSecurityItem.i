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
    File        : eSmartToolbarSecurityItem.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.04.2014 11:44:41
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartToolbarSecurityItem NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartToolbarSecurityItemBefore &ENDIF
    FIELD ToolbarSecurityItemGuid AS CHARACTER FORMAT "x(36)":U LABEL "ToolbarSecurityItemGuid":T SERIALIZE-NAME "ToolbarSecurityItemGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ObjectName AS CHARACTER FORMAT "x(80)":U LABEL "Container / Object Name":T SERIALIZE-NAME "ObjectName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ToolbarItemKey AS CHARACTER FORMAT "x(80)":U LABEL "Toolbar Item":T SERIALIZE-NAME "ToolbarItemKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD Description AS CHARACTER FORMAT "x(80)":U LABEL "Description":T SERIALIZE-NAME "Description":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX ObjectNameToolbarItem AS UNIQUE ObjectName ASCENDING ToolbarItemKey ASCENDING
    INDEX ToolbarItemObjectName AS UNIQUE ToolbarItemKey ASCENDING ObjectName ASCENDING
    INDEX ToolbarSecurityItemGuid AS UNIQUE PRIMARY ToolbarSecurityItemGuid ASCENDING

    .
