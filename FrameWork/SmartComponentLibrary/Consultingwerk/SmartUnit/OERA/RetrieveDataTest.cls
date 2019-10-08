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
    File        : RetrieveDataTest
    Purpose     : Performs a basic RetrieveData test on a set of 
                  Business Entities
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 29 20:56:12 CET 2013
    Notes       : Business entities with the @TestIgnore. Annotation just
                  before the class block will be ignored.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH . 
USING Consultingwerk.OERA.Exceptions.*       FROM PROPATH . 
USING Consultingwerk.OERA.UnitTests.*        FROM PROPATH . 
USING Consultingwerk.SmartUnit.*             FROM PROPATH .
USING Consultingwerk.SmartUnit.TestRunner.*  FROM PROPATH .
USING Consultingwerk.Studio.*                FROM PROPATH . 
USING Consultingwerk.Util.*                  FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartUnit.OERA.RetrieveDataTest
    IMPLEMENTS ITestRunner: 

    {Consultingwerk/SmartUnit/dsTestsuites.i}

    DEFINE VARIABLE iStopAfter AS INTEGER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Executes the test suite described by the ITestSuite instance
        Notes:   
        @param poTestSuite The ITestSuite to execute
        @return Logical value, when set to false halt will be signalled to the caller (ANT script)  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Execute (INPUT poTestSuite AS Consultingwerk.SmartUnit.ITestSuite):
        
        CREATE eTestsuite . 
        ASSIGN eTestsuite.TestsuiteName = poTestSuite:TestSuite .
         
        ASSIGN iStopAfter = INTEGER (SmartUnit:GetTestSuiteParameter ("StopAfter":U)) NO-ERROR .

        THIS-OBJECT:ExecuteRetrieveDataTest () .

        RETURN TRUE .

        CATCH err AS Progress.Lang.Error:
            MESSAGE ErrorHelper:FormattedErrorMessagesExt (err) .
            
            RETURN FALSE .     
        END CATCH.

        FINALLY:
            SmartUnit:WriteTestResult (poTestSuite:Output,
                                       DATASET dsTestsuites BY-REFERENCE) .       
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs a RetrieveDataTest on every Business Entity in the 
                 BusinessEntityFileset 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ExecuteRetrieveDataTest ():
        
        DEFINE VARIABLE cFileset          AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cBaseFolder       AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE oFiles            AS CharacterList          NO-UNDO . 
        DEFINE VARIABLE oClass            AS Progress.Lang.Class    NO-UNDO . 
        DEFINE VARIABLE oAnnotations      AS ListAnnotation         NO-UNDO .
        DEFINE VARIABLE cFileName         AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE dtTestStart       AS DATETIME-TZ            NO-UNDO .
        DEFINE VARIABLE oFetchDataRequest AS IFetchDataRequest      NO-UNDO .
        DEFINE VARIABLE cBuffers          AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE hDataset          AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE oParser           AS IClassAnnotationParser NO-UNDO . 
        DEFINE VARIABLE lStopCondition    AS LOGICAL                NO-UNDO .
        DEFINE VARIABLE oError            AS Progress.Lang.Error    NO-UNDO . 
        DEFINE VARIABLE i                 AS INTEGER                NO-UNDO .

        ASSIGN cFileset    = SmartUnit:GetTestSuiteParameter ("BusinessEntityFileset":U) 
               cBaseFolder = SmartUnit:GetTestSuiteParameter ("BusinessEntityBaseFolder":U)
               oFiles      = SmartUnit:ParseFileset (cFileset, cBaseFolder) .

        MESSAGE "Business Entity Fileset:":U cFileset .

        {Consultingwerk/foreachPrimitiveList.i Character cClass in oFiles}
            
            oClass = Progress.Lang.Class:GetClass (cClass) .

            IF NOT oClass:IsA ("Consultingwerk.OERA.IBusinessEntity":U) THEN 
                NEXT . 

            IF oClass:IsAbstract() OR oClass:IsInterface() THEN 
                NEXT . 
    
            ASSIGN cFileName = ClassHelper:ClassNameToFile (cClass) .
    
            MESSAGE "Parsing Business Entity:":U cClass 
                    SUBSTITUTE ("(&1)":U, cFileName).

            FILE-INFORMATION:FILE-NAME = cFileName . 

            oParser = SmartUnit:GetAnnotationParser (FILE-INFORMATION:FULL-PATHNAME, 
                                                     FALSE) .

            oAnnotations = oParser:GetClassAnnotations () .
            
            IF VALID-OBJECT (oAnnotations:FindByName ("@TestIgnore":U)) THEN DO:
                IF SmartUnit:Verbose THEN
                    MESSAGE "Ignoring":U cClass. 
                NEXT . 
            END.

            MESSAGE "Processing Business Entity:":U cClass 
                    SUBSTITUTE ("(&1)":U, cFileName).

            CREATE eTestcase.
            ASSIGN eTestcase.TestsuiteName = eTestsuite.TestsuiteName 
                   eTestcase.ClassName     = cClass 
                   eTestcase.TestcaseName  = "RetrieveData":U .
   
            /* Mike Fechner, Consultingwerk Ltd. 04.04.2013
               STOP-AFTER only on OE11 */
            DO 
&IF PROVERSION NE "10.2B" &THEN    
            STOP-AFTER iStopAfter 
&ENDIF
            ON STOP UNDO, LEAVE:
   
                ASSIGN lStopCondition = TRUE
                       oError         = ? . 
                
                DO ON ERROR UNDO, THROW:
                    ASSIGN dtTestStart    = NOW . 
    
                    ASSIGN cBuffers = "":U . 
                
                    ServiceInterface:FetchDataset (cClass,
                                                   OUTPUT DATASET-HANDLE hDataset) .
    
                    DO i = 1 TO hDataset:NUM-BUFFERS:
                        cBuffers = cBuffers + 
                                   (IF i > 1 THEN ",":U ELSE "":U) +
                                   hDataset:GET-BUFFER-HANDLE (i):NAME .
                    END.
        
                    oFetchDataRequest = NEW FetchDataRequest (cBuffers, 1) .
        
                    ServiceInterface:FetchData (cClass, 
                                                oFetchDataRequest,
                                                OUTPUT DATASET-HANDLE hDataset) .
        
                    CATCH err AS Progress.Lang.Error:
                        /* Keep error */
                        oError = err .                      
                    END CATCH.
        
                    FINALLY:
                        ASSIGN lStopCondition          = FALSE 
                               eTestcase.ExecutionTime = INTERVAL (NOW, dtTestStart, "milliseconds":U) / 1000.                    
                    END FINALLY.
                END.
            END. /* STOP-AFTER */
  
            IF lStopCondition = TRUE THEN 
                ASSIGN eTestcase.ExecutionTime   = INTERVAL (NOW, dtTestStart, "milliseconds":U) / 1000
                       eTestcase.ErrorMessage    = "A STOP Condition has occurred.":U   
                       eTestcase.ErrorType       = "":U 
                       eTestcase.ErrorStacktrace = "":U .
            ELSE 
                IF VALID-OBJECT (oError) THEN 
                    ASSIGN eTestcase.ErrorMessage    = ErrorHelper:FormattedErrorMessages (err)
                           eTestcase.ErrorType       = ErrorHelper:ErrorTypeName (err)
                           eTestcase.ErrorStacktrace = ErrorHelper:StackTrace (err).
  
            FINALLY:
                IF VALID-HANDLE (hDataset) THEN 
                    DELETE OBJECT hDataset . 
                
                hDataset = ? .    
                
                DO ON ERROR UNDO, THROW:
                    ServiceInterface:StopBusinessEntity (cClass) .
                    
                    CATCH err2 AS BusinessServiceNotRunning:
                        /* ignore errors from StopBusinessEntity */    
                    END CATCH.
                END.
            END FINALLY.
        END. /* foreach.i */

    END METHOD .

END CLASS.
