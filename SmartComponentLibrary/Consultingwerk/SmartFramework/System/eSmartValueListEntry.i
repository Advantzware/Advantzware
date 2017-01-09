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
    File        : eSmartValueListEntry.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 08.03.2013 16:34:16
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartValueListEntry NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartValueListEntryBefore &ENDIF
    FIELD ValueListEntryGuid AS CHARACTER FORMAT "x(36)":U LABEL "GUID":T SERIALIZE-NAME "ValueListEntryGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ValueListGuid AS CHARACTER FORMAT "x(36)":U LABEL "GUID":T SERIALIZE-NAME "ValueListGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ValueListEntryKey AS CHARACTER FORMAT "x(15)":U LABEL "Entry Key":T SERIALIZE-NAME "ValueListEntryKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ValueListEntryDescription AS CHARACTER FORMAT "x(80)":U LABEL "Description":T SERIALIZE-NAME "ValueListEntryDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ValueListEntryImageKey AS CHARACTER FORMAT "x(80)":U LABEL "Image Key":T SERIALIZE-NAME "ValueListEntryImageKey":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX ValueList AS UNIQUE ValueListGuid ASCENDING ValueListEntryDescription ASCENDING
    INDEX ValueListEntryGuid AS UNIQUE ValueListEntryGuid ASCENDING
    INDEX ValueListEntryKey AS UNIQUE PRIMARY ValueListGuid ASCENDING ValueListEntryKey ASCENDING

    .
