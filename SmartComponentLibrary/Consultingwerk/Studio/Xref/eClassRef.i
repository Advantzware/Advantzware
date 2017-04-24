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
    File        : eClassRef.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 23.03.2013 10:21:21
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eClassRef NO-UNDO XML-NODE-NAME "Class-ref":U {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN   &ENDIF
    FIELD SourceGuid AS CHARACTER FORMAT "X(8)":U XML-NODE-NAME "Source-guid":U
    FIELD RefSeq AS INTEGER FORMAT ">>>,>>>,>>9":U XML-NODE-NAME "Ref-seq":U
    FIELD InheritedList AS CHARACTER FORMAT "X(60)":U XML-NODE-NAME "Inherited-list":U
    FIELD ImplementsList AS CHARACTER FORMAT "X(60)":U XML-NODE-NAME "Implements-list":U
    FIELD HasUsePool AS LOGICAL FORMAT "yes/no":U XML-NODE-NAME "Has-use-pool":U
    FIELD IsFinal AS LOGICAL FORMAT "yes/no":U XML-NODE-NAME "Is-final":U

    INDEX ClassRef SourceGUID ASCENDING RefSeq ASCENDING

    .
