/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : eSmartLinkType.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.05.2016 23:33:50
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.LinkTypeBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartLinkType{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartLinkTypeBefore{&SUFFIX} &ENDIF
    FIELD LinkTypeGuid AS CHARACTER FORMAT "x(36)":U LABEL "LinkTypeGuid":T
    FIELD LinkName AS CHARACTER FORMAT "x(80)":U LABEL "Link Name":T
    FIELD SourcePropertyName AS CHARACTER FORMAT "x(80)":U LABEL "Source Property Name":T
    FIELD SourceHandshakeMethod AS CHARACTER FORMAT "x(80)":U LABEL "Source Handshake Method":T
    FIELD TargetPropertyName AS CHARACTER FORMAT "x(80)":U LABEL "Target Property Name":T
    FIELD TargetHandshakeMethod AS CHARACTER FORMAT "x(80)":U LABEL "Target Handshake Method":T

    INDEX LinkName AS UNIQUE LinkName ASCENDING
    INDEX LinkTypeGuid AS UNIQUE PRIMARY LinkTypeGuid ASCENDING

    .

    