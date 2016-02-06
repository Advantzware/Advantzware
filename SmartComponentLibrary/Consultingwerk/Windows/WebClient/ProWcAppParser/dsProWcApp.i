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
    File        : dsProWcApp.i
    Purpose     : Business Entity for dsProWcApp

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 16.08.2015 20:52:42
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsProWcApp

/* ***************************  ttApplicationProperties  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttApplicationProperties{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} 
    FIELD Section AS CHARACTER FORMAT "X(40)":U
    FIELD EntryName AS CHARACTER FORMAT "X(40)":U
    FIELD EntryValue AS CHARACTER FORMAT "X(40)":U

    INDEX SectionEntryName AS UNIQUE PRIMARY Section ASCENDING EntryName ASCENDING

    .
/* ***************************  ttVersions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttVersions{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} 
    FIELD VersionNumber AS INTEGER FORMAT "->,>>>,>>9":U
    FIELD VersionName AS CHARACTER FORMAT "X(40)":U
    FIELD EndUserDescription AS CHARACTER FORMAT "X(40)":U

    INDEX VersionNumber AS UNIQUE PRIMARY VersionNumber ASCENDING

    .


@BusinessEntityGenerator (entityname="Consultingwerk.Windows.WebClient.ProWcAppParser.ProWcAppBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsProWcApp{&SUFFIX} {&REFERENCE-ONLY} FOR ttApplicationProperties{&SUFFIX}, ttVersions{&SUFFIX} 

    .    
