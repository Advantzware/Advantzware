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
    File        : eContextProperties.i
    Purpose     : General purpose name/value pairs

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 31.05.2016 14:13:09
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.OERA.Context.ContextBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eContextProperties{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eContextPropertyBefore{&SUFFIX} &ENDIF
    FIELD PropertyName AS CHARACTER FORMAT "X(40)":U
    FIELD ValueCharacter AS CHARACTER FORMAT "X(80)":U
    FIELD ValueLongchar AS CLOB FORMAT "X(800)":U
    FIELD ValueInteger AS INTEGER FORMAT ">>>,>>>,>>9":U
    FIELD ValueInt64 AS INT64 FORMAT ">>>,>>>,>>9":U
    FIELD ValueDecimal AS DECIMAL FORMAT ">>>,>>>,>>9.999":U
    FIELD ValueLogical AS LOGICAL FORMAT "yes/no":U
    FIELD ValueDate AS DATE FORMAT "99.99.9999":U
    FIELD ValueDateTime AS DATETIME-TZ
    FIELD ValueRaw AS RAW

    INDEX PropertyName AS UNIQUE PRIMARY PropertyName ASCENDING

    .

    