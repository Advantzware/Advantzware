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
    File        : eSessionContext.i
    Purpose     : General purpose Context Temp-Table

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.05.2016 14:13:09
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.OERA.Context.ContextBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSessionContext{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSessionContextBefore{&SUFFIX} &ENDIF
    FIELD UserName AS CHARACTER FORMAT "X(80)":U
    FIELD UserKey AS CHARACTER FORMAT "X(36)":U LABEL "User Key":T
    FIELD LoginCompanyName AS CHARACTER FORMAT "X(80)":U
    FIELD ClientPrincipal AS RAW
    FIELD LoginCompanyKey AS CHARACTER FORMAT "X(8)":U
    FIELD LoginCompanyShortName AS CHARACTER FORMAT "X(8)":U
    FIELD LoginCompanyReferenceChar AS CHARACTER FORMAT "X(8)":U
    FIELD LoginCompanyReferenceDecimal AS DECIMAL FORMAT ">>>,>>>,>>9.9999":U
    FIELD LoginCompanyReferenceInteger AS INTEGER FORMAT ">>>,>>>,>>9":U
    FIELD LanguageDisplayName AS CHARACTER FORMAT "X(8)":U
    FIELD LanguageIsoCode AS CHARACTER FORMAT "X(8)":U
    FIELD LanguageKey AS CHARACTER FORMAT "X(8)":U
    FIELD UserGroupKeys AS CHARACTER FORMAT "X(8)":U
    FIELD UserGroupNames AS CHARACTER FORMAT "X(8)":U
    FIELD ServerLogEntryTypes AS CHARACTER FORMAT "X(60)":U
    FIELD ServerCustomLogEntries AS CHARACTER FORMAT "X(60)":U
    FIELD ServerDebugMode AS LOGICAL FORMAT "yes/no":U LABEL "Debug Mode":T
    FIELD ClientProVersion AS CHARACTER FORMAT "X(8)":U LABEL "Client OpenEdge Version":T
    FIELD AppServerProVersion AS CHARACTER FORMAT "X(8)":U LABEL "AppServer OpenEdge Version":T
    FIELD ClientType AS CHARACTER FORMAT "X(8)":U LABEL "Client Type":T
    FIELD AppServerType AS CHARACTER FORMAT "X(8)":U LABEL "App Server Type":T


    .

    