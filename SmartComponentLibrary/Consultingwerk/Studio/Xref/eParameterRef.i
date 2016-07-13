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
    File        : eParameterRef.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 23.03.2013 10:21:21
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eParameterRef NO-UNDO XML-NODE-NAME "Parameter-ref":U {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eParameterRefBefore &ENDIF
    FIELD Order AS INTEGER FORMAT ">>>,>>>,>>9":U SERIALIZE-NAME "Order":U XML-NODE-TYPE "attribute":U XML-NODE-NAME "Order":U
    FIELD ParameterMode AS CHARACTER FORMAT "X(60)":U SERIALIZE-NAME "ParameterMode":U XML-NODE-TYPE "attribute":U XML-NODE-NAME "Parameter-mode":U
    FIELD ParameterName AS CHARACTER FORMAT "X(60)":U SERIALIZE-NAME "ParameterName":U XML-NODE-TYPE "attribute":U XML-NODE-NAME "Parameter-name":U
    FIELD ParameterType AS CHARACTER FORMAT "X(60)":U SERIALIZE-NAME "ParameterType":U XML-NODE-TYPE "attribute":U XML-NODE-NAME "Parameter-type":U
    FIELD SourceGuid AS CHARACTER FORMAT "X(8)":U SERIALIZE-NAME "SourceGuid":U XML-NODE-NAME "Source-guid":U
    FIELD RefSeq AS INTEGER FORMAT ">>>,>>>,>>9":U SERIALIZE-NAME "RefSeq":U XML-NODE-NAME "Ref-seq":U
    FIELD Dimension AS INTEGER FORMAT ">>>,>>>,>>9":U SERIALIZE-NAME "Dimension":U XML-NODE-NAME "Dimension":U
    FIELD IsAppend AS LOGICAL FORMAT "yes/no":U SERIALIZE-NAME "IsAppend":U XML-NODE-NAME "Is-append":U
    FIELD DatasetGuid AS CHARACTER FORMAT "x(60)":U SERIALIZE-NAME "DatasetGuid":U XML-NODE-NAME "DatasetGuid":U

    INDEX RefSeq AS PRIMARY RefSeq ASCENDING Order ASCENDING
    INDEX Reference SourceGUID ASCENDING RefSeq ASCENDING Order ASCENDING

    .
