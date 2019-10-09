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
    File        : eSmartValueList.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 08.03.2013 16:34:16
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartValueList NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartValueListBefore &ENDIF
    FIELD ValueListGuid AS CHARACTER FORMAT "x(36)":U LABEL "GUID":T SERIALIZE-NAME "ValueListGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ModuleGuid AS CHARACTER FORMAT "x(36)":U LABEL "ModuleGuid":T SERIALIZE-NAME "ModuleGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ValueListKey AS CHARACTER FORMAT "x(15)":U LABEL "Value List Key":T SERIALIZE-NAME "ValueListKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ValueListDescription AS CHARACTER FORMAT "x(80)":U LABEL "Description":T SERIALIZE-NAME "ValueListDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX ModuleGuid ModuleGuid ASCENDING
    INDEX ValueListDescription ValueListDescription ASCENDING
    INDEX ValueListGuid ValueListGuid ASCENDING
    INDEX ValueListKey AS UNIQUE PRIMARY ValueListKey ASCENDING

    .
