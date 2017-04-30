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
    File        : eReference.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 23.03.2013 10:21:21
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eReference NO-UNDO XML-NODE-NAME "Reference":U {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN   &ENDIF
    FIELD ReferenceType AS CHARACTER FORMAT "X(20)":U XML-NODE-TYPE "attribute":U XML-NODE-NAME "Reference-type":U
    FIELD ObjectIdentifier AS CHARACTER FORMAT "X(60)":U XML-NODE-TYPE "attribute":U XML-NODE-NAME "Object-identifier":U
    FIELD SourceGuid AS CHARACTER FORMAT "X(8)":U XML-NODE-NAME "Source-guid":U
    FIELD FileNum AS INTEGER FORMAT ">>>,>>>,>>9":U XML-NODE-NAME "File-num":U
    FIELD RefSeq AS INTEGER FORMAT ">>>,>>>,>>9":U XML-NODE-NAME "Ref-seq":U
    FIELD LineNum AS INTEGER FORMAT ">>>,>>>,>>9":U XML-NODE-NAME "Line-num":U
    FIELD ObjectContext AS CHARACTER FORMAT "x(60)":U XML-NODE-NAME "Object-context":U
    FIELD AccessMode AS CHARACTER FORMAT "X(8)":U XML-NODE-NAME "Access-mode":U
    FIELD DataMemberRef AS CHARACTER FORMAT "X(60)":U XML-NODE-NAME "Data-member-ref":U
    FIELD TempRef AS CHARACTER FORMAT "X(60)":U XML-NODE-NAME "Temp-ref":U
    FIELD Detail AS CHARACTER FORMAT "X(60)":U XML-NODE-NAME "Detail":U
    FIELD IsStatic AS LOGICAL FORMAT "yes/no":U XML-NODE-NAME "Is-static":U
    FIELD IsAbstract AS LOGICAL FORMAT "yes/no":U XML-NODE-NAME "Is-abstract":U

    INDEX Reference SourceGUID ASCENDING FileNum ASCENDING RefSeq ASCENDING
    INDEX RefSeq AS PRIMARY RefSeq ASCENDING

    .
