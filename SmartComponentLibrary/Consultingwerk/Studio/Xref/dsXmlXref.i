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
    File        : dsXmlXref.i
    Purpose     : Business Entity for XmlXref

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 23.03.2013 10:21:21
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsXmlXref

{ Consultingwerk/Studio/Xref/eSource.i }
{ Consultingwerk/Studio/Xref/eReference.i }
{ Consultingwerk/Studio/Xref/eClassRef.i }
{ Consultingwerk/Studio/Xref/eParameterRef.i }


DEFINE {&ACCESS} DATASET dsXmlXref {&REFERENCE-ONLY} FOR eSource, eReference, eClassRef, eParameterRef 
    DATA-RELATION eReferenceeClassRefRelation FOR eReference, eClassRef 
        RELATION-FIELDS (SourceGuid,SourceGuid,RefSeq,RefSeq)
        NESTED 
    DATA-RELATION eReferenceeParameterRefRelation FOR eReference, eParameterRef 
        RELATION-FIELDS (SourceGuid,SourceGuid,RefSeq,RefSeq)
        NESTED 
    DATA-RELATION eSourceeReferenceRelation FOR eSource, eReference 
        RELATION-FIELDS (SourceGuid,SourceGuid)
        NESTED 

    .    
