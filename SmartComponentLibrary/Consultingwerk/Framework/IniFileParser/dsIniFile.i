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
    File        : dsIniFile.i
    Purpose     : Business Entity for IniFile

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 15.08.2015 11:26:37
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsIniFile

/* ***************************  ttEntries  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttEntries{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} BEFORE-TABLE etEntriesBefore{&SUFFIX} 
    FIELD Section AS CHARACTER FORMAT "X(40)":U LABEL "Section":T
    FIELD LineNumber AS INTEGER FORMAT "->,>>>,>>9":U LABEL "Line Number":T
    FIELD EntryName AS CHARACTER FORMAT "X(40)":U LABEL "Entry Name":T
    FIELD EntryValue AS CHARACTER FORMAT "X(40)":U LABEL "Entry Value":T

    INDEX SectionEntry AS PRIMARY Section ASCENDING EntryName ASCENDING
    INDEX LineNumber LineNumber ASCENDING

    .
/* ***************************  ttSection  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttSection{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} 
    FIELD Section AS CHARACTER FORMAT "X(40)":U LABEL "Section":T

    INDEX Section AS PRIMARY Section ASCENDING

    .


@BusinessEntityGenerator (entityname="Consultingwerk.Framework.IniFileParser.IniFileBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsIniFile{&SUFFIX} {&REFERENCE-ONLY} FOR ttSection{&SUFFIX}, ttEntries{&SUFFIX} 
    DATA-RELATION ttSectionttEntriesRelation FOR ttSection{&SUFFIX}, ttEntries{&SUFFIX} 
        RELATION-FIELDS (Section,Section)

    .    
