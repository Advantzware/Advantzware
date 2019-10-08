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
    File        : eSource.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 23.03.2013 10:21:21
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSource NO-UNDO XML-NODE-NAME "Source":U {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN   &ENDIF
    FIELD FileName AS CHARACTER FORMAT "X(60)":U XML-NODE-TYPE "attribute":U XML-NODE-NAME "File-name":U
    FIELD SourceGuid AS CHARACTER FORMAT "X(8)":U XML-NODE-NAME "Source-guid":U
    FIELD FileNum AS INTEGER FORMAT ">>>,>>>,>>9":U XML-NODE-NAME "File-num":U

    INDEX SourceGUID AS PRIMARY SourceGuid ASCENDING
    INDEX FileName FileName ASCENDING

    .
