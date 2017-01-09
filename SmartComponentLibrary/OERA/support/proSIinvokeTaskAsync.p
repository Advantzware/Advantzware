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
    File        : proSIinvokeTaskAsync.p
    Purpose     : Service Interface procedure for asynchronous invoke
                  task

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 20 21:10:58 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.* FROM PROPATH.
USING Consultingwerk.Util.*      FROM PROPATH.

DEFINE INPUT  PARAMETER pcTaskName    AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER pcMethodName  AS CHARACTER NO-UNDO .
DEFINE INPUT  PARAMETER piNumDatasets AS INTEGER   NO-UNDO .
DEFINE INPUT  PARAMETER plcDataSetIn1 AS LONGCHAR  NO-UNDO .
DEFINE INPUT  PARAMETER plcDataSetIn2 AS LONGCHAR  NO-UNDO .
DEFINE INPUT  PARAMETER plcDataSetIn3 AS LONGCHAR  NO-UNDO .
DEFINE INPUT  PARAMETER plcDataSetIn4 AS LONGCHAR  NO-UNDO .
DEFINE INPUT  PARAMETER plcDataSetIn5 AS LONGCHAR  NO-UNDO .
DEFINE INPUT  PARAMETER plcParameter AS LONGCHAR NO-UNDO .
DEFINE INPUT  PARAMETER plcContext    AS LONGCHAR  NO-UNDO .

DEFINE OUTPUT PARAMETER plcDataset1     AS LONGCHAR NO-UNDO . 
DEFINE OUTPUT PARAMETER plcDataset2     AS LONGCHAR NO-UNDO . 
DEFINE OUTPUT PARAMETER plcDataset3     AS LONGCHAR NO-UNDO . 
DEFINE OUTPUT PARAMETER plcDataset4     AS LONGCHAR NO-UNDO . 
DEFINE OUTPUT PARAMETER plcDataset5     AS LONGCHAR NO-UNDO . 
DEFINE OUTPUT PARAMETER plcParameterOut AS LONGCHAR NO-UNDO . 

DEFINE VARIABLE hDataset1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataset2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataset3 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataset4 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataset5 AS HANDLE NO-UNDO.

DEFINE VARIABLE hContextDataset       AS HANDLE NO-UNDO.
DEFINE VARIABLE hContextDatasetBefore AS HANDLE NO-UNDO.

{ Consultingwerk/products.i }

/* Mike Fechner, Consultingwerk Ltd. 08.05.2013
   Support for custom include files to the proSI... procedures. 
   This allows adding SHARED variable definitions that may be 
   required to execute legacy database triggers */
&IF "{&ProSIcustomIncludeDirectory}":U NE "":U &THEN
{ {&ProSIcustomIncludeDirectory}/proSIinvokeTaskCustom.i }
&ENDIF

/* ***************************  Main Block  *************************** */

IF plcContext > "":U THEN DO:
    CREATE DATASET hContextDataset . 
    
    hContextDataset:READ-XML ("LONGCHAR":U, plcContext, "EMPTY":U, ?, ?) .
    
    ASSIGN hContextDatasetBefore                                          = Consultingwerk.Framework.Session.SessionManager:ContextDataset  
           Consultingwerk.Framework.Session.SessionManager:ContextDataset = hContextDataset .
END.

IF plcDatasetIn1 > "":U THEN DO:
    CREATE DATASET hDataset1 .
    hDataset1:READ-XML ("LONGCHAR":U, plcDatasetIn1, "EMPTY":U, ?, ?) .
END.    

IF plcDatasetIn2 > "":U THEN DO:
    CREATE DATASET hDataset2 .
    hDataset2:READ-XML ("LONGCHAR":U, plcDatasetIn2, "EMPTY":U, ?, ?) .
END.    

IF plcDatasetIn3 > "":U THEN DO:
    CREATE DATASET hDataset3 .
    hDataset3:READ-XML ("LONGCHAR":U, plcDatasetIn3, "EMPTY":U, ?, ?) .
END.    

IF plcDatasetIn4 > "":U THEN DO:
    CREATE DATASET hDataset4 .
    hDataset4:READ-XML ("LONGCHAR":U, plcDatasetIn4, "EMPTY":U, ?, ?) .
END.    

IF plcDatasetIn5 > "":U THEN DO:
    CREATE DATASET hDataset5 .
    hDataset5:READ-XML ("LONGCHAR":U, plcDatasetIn5, "EMPTY":U, ?, ?) .
END.    

Consultingwerk.OERA.ServiceInterface:InvokeTask (pcTaskName,
                                                 pcMethodName,
                                                 piNumDatasets,
                                                 INPUT-OUTPUT DATASET-HANDLE hDataset1 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE hDataset2 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE hDataset3 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE hDataset4 BY-REFERENCE,
                                                 INPUT-OUTPUT DATASET-HANDLE hDataset5 BY-REFERENCE,
                                                 INPUT-OUTPUT plcParameter).
                 
IF VALID-HANDLE (hDataset1) THEN 
    hDataset1:WRITE-XML ("LONGCHAR":U, plcDataset1, FALSE, "utf-8":U, ?, TRUE) .
IF VALID-HANDLE (hDataset2) THEN 
    hDataset2:WRITE-XML ("LONGCHAR":U, plcDataset2, FALSE, "utf-8":U, ?, TRUE) .
IF VALID-HANDLE (hDataset3) THEN 
    hDataset3:WRITE-XML ("LONGCHAR":U, plcDataset3, FALSE, "utf-8":U, ?, TRUE) .
IF VALID-HANDLE (hDataset4) THEN 
    hDataset4:WRITE-XML ("LONGCHAR":U, plcDataset4, FALSE, "utf-8":U, ?, TRUE) .
IF VALID-HANDLE (hDataset5) THEN 
    hDataset5:WRITE-XML ("LONGCHAR":U, plcDataset5, FALSE, "utf-8":U, ?, TRUE) .
                 
ASSIGN plcParameterOut = plcParameter .                

CATCH err AS Progress.Lang.Error :
	LogManager:WriteError (err) .	
END CATCH.
                        
/*{ {&OERASI}/sicatch.i }*/
              
FINALLY:
    IF VALID-HANDLE (hDataset1) THEN                                                    
        DELETE OBJECT hDataset1 .
    IF VALID-HANDLE (hDataset2) THEN                                                    
        DELETE OBJECT hDataset2 .
    IF VALID-HANDLE (hDataset3) THEN                                                    
        DELETE OBJECT hDataset3 .
    IF VALID-HANDLE (hDataset4) THEN                                                    
        DELETE OBJECT hDataset4 .
    IF VALID-HANDLE (hDataset5) THEN                                                    
        DELETE OBJECT hDataset5 .
    IF VALID-HANDLE (hContextDataset) THEN 
        DELETE OBJECT hContextDataset NO-ERROR . 

    /* Mike Fechner, Consultingwerk Ltd. 23.10.2011
       Only reset the ContextDataset when it was set by this instance
       (avoid issues with call nesting) */
    Consultingwerk.Framework.Session.SessionManager:ContextDataset = hContextDatasetBefore .
END FINALLY.          
                                                          