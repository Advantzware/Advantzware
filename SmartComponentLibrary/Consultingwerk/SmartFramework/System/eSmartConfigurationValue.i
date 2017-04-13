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
    File        : eSmartConfigurationValue.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 07.01.2017 12:05:53
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.ConfigurationValueBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="ConfigurationValueGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartConfigurationValue{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartConfigurationValueBefore{&SUFFIX} &ENDIF
    FIELD ConfigurationValueGuid AS CHARACTER FORMAT "x(36)":U LABEL "ConfigurationValueGuid":T
    FIELD ConfigurationValueName AS CHARACTER FORMAT "x(20)":U LABEL "Name":T
    FIELD ConfigurationValueCharacter AS CHARACTER FORMAT "x(50)":U INITIAL ? LABEL "Value Character":T
    FIELD ConfigurationValueInteger AS INTEGER FORMAT "->,>>>,>>9":U INITIAL ? LABEL "ConfigurationValueInteger":T
    FIELD ConfigurationValueInt64 AS INT64 FORMAT "->,>>>,>>9":U INITIAL ? LABEL "ConfigurationValueInt64":T
    FIELD ConfigurationValueDecimal AS DECIMAL FORMAT "->>,>>9.99":U INITIAL ? LABEL "ConfigurationValueDecimal":T
    FIELD ConfiugrationValueData AS DATE FORMAT "99/99/99":U INITIAL ? LABEL "ConfiugrationValueData":T
    FIELD ConfigurationValueDateTime AS DATETIME FORMAT "99/99/9999 HH:MM:SS.SSS":U INITIAL ? LABEL "ConfigurationValueDateTime":T
    FIELD ConfigurationValueDateTimeTz AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U INITIAL ? LABEL "ConfigurationValueDateTimeTz":T
    FIELD ConfigurationValueLogical AS LOGICAL FORMAT "yes/no":U INITIAL "no":U LABEL "ConfigurationValueLogical":T

    INDEX ConfigurationValueGuid AS UNIQUE PRIMARY ConfigurationValueGuid ASCENDING
    INDEX ConfigurationValueName AS UNIQUE ConfigurationValueName ASCENDING

    .

    