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
    File        : eSmartContextStore.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 25.02.2016 08:38:31
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Context.ContextStoreBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartContextStore{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartContextStoreBefore{&SUFFIX} &ENDIF
    FIELD ContextStoreGUID AS CHARACTER FORMAT "x(36)":U LABEL "ContextStoreGUID":T
    FIELD SessionID AS CHARACTER FORMAT "x(40)":U LABEL "Session ID":T
    FIELD UserName AS CHARACTER FORMAT "x(20)":U LABEL "User Name":T
    FIELD DomainName AS CHARACTER FORMAT "x(20)":U LABEL "Domain Name":T
    FIELD Created AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "Date Time Created":T
    FIELD LastAccessed AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INIT ? LABEL "Date Time Last Accessed":T
    FIELD ContextDataset AS CLOB FORMAT "x(8)":U INIT ? LABEL "ContextDataset":T

    INDEX ContextStoreGUID AS UNIQUE PRIMARY ContextStoreGUID ASCENDING
    INDEX Created Created ASCENDING
    INDEX DomainUser DomainName ASCENDING UserName ASCENDING
    INDEX LastAccessed LastAccessed ASCENDING
    INDEX SessionID AS UNIQUE SessionID ASCENDING

    .

    