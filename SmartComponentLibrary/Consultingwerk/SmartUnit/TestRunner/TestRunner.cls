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
    File        : TestRunner
    Purpose     : Default TestRunner implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Mar 24 16:12:17 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Assertion.*             FROM PROPATH . 
USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.SmartUnit.*             FROM PROPATH .
USING Consultingwerk.SmartUnit.TestRunner.*  FROM PROPATH .
USING Consultingwerk.Studio.*                FROM PROPATH .  
USING Consultingwerk.Util.*                  FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartUnit.TestRunner.TestRunner 
    IMPLEMENTS ITestRunner: 

    {Consultingwerk/SmartUnit/dsTestsuites.i}

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if Errors Messages should be shown on in an dialog
                 box  
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PROTECTED PROPERTY DialogOutput AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if Log-Messages should be written (MESSAGE to console)
                  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY LogMessages AS LOGICAL INITIAL TRUE NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Executes the test suite described by the ITestSuite instance
        Notes:   
        @param poTestSuite The ITestSuite to execute
        @return Logical value, when set to false halt will be signalled to the caller (ANT script)  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Execute (poTestSuite AS ITestSuite).

        DEFINE VARIABLE oClasses               AS CharacterList          NO-UNDO . 
        DEFINE VARIABLE oClassAnnotationParser AS IClassAnnotationParser NO-UNDO .   
        DEFINE VARIABLE oAnnotations           AS ListAnnotation         NO-UNDO . 
        DEFINE VARIABLE oListClassMember       AS ListClassMember        NO-UNDO . 
        DEFINE VARIABLE cFileName              AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE oClassTestAnnotation   AS Annotation             NO-UNDO .
        DEFINE VARIABLE oMemberTestAnnotation  AS Annotation             NO-UNDO .  
        DEFINE VARIABLE oTestInstance          AS Progress.Lang.Object   NO-UNDO . 
        DEFINE VARIABLE cExpected              AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE iStopAfter             AS INTEGER                NO-UNDO .
        DEFINE VARIABLE oResult                AS TestResult             NO-UNDO .
        DEFINE VARIABLE cLogFolder             AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE lcSystemErr            AS LONGCHAR               NO-UNDO . 
        DEFINE VARIABLE lcSystemOut            AS LONGCHAR               NO-UNDO . 
        
        DEFINE VARIABLE cSetupMethod           AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE cTearDownMethod        AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE cBeforeMethod          AS CHARACTER              NO-UNDO.
        DEFINE VARIABLE cAfterMethod           AS CHARACTER              NO-UNDO.
        
        IF CharacterType:IsNotNullOrEmpty (poTestSuite:Services) THEN  
            THIS-OBJECT:LoadServices (poTestSuite:Services) .
        
        CREATE eTestsuite . 
        ASSIGN eTestsuite.TestsuiteName = poTestSuite:TestSuite .

        ASSIGN cLogFolder = FileHelper:DirectoryName (poTestSuite:Output) .

        oClasses = SmartUnit:ParseFileset (poTestSuite:Tests) .

        IF THIS-OBJECT:LogMessages THEN 
            MESSAGE "Processing fileset:":U poTestSuite:Tests .

        {Consultingwerk/foreachPrimitiveList.i Character cClass in oClasses}

            ASSIGN cFileName = ClassHelper:ClassNameToFile (cClass) .
            FILE-INFO:FILE-NAME = cFileName .
            
            IF SmartUnit:Verbose AND THIS-OBJECT:LogMessages THEN 
                MESSAGE "Parsing filename:":U cFileName FILE-INFO:FULL-PATHNAME .

            IF FILE-INFO:FULL-PATHNAME > "":U THEN 
                oClassAnnotationParser = SmartUnit:GetAnnotationParser (FILE-INFO:FULL-PATHNAME,
                                                                        poTestSuite:ForceXmlXref) .
            ELSE 
                NEXT . 
        
            oAnnotations = oClassAnnotationParser:GetClassAnnotations() .
            oClassTestAnnotation = oAnnotations:FindByName ("@Test":U) .
        
            IF NOT VALID-OBJECT (oClassTestAnnotation) THEN 
                NEXT . 

            IF THIS-OBJECT:LogMessages THEN
                MESSAGE "Processing test:":U cClass .

            oTestInstance = DYNAMIC-NEW (cClass) () .

            oListClassMember = oClassAnnotationParser:GetClassMemberAnnotations () .

            ASSIGN  cSetupMethod    = "":U 
                    cTearDownMethod = "":U
                    cBeforeMethod   = "":U 
                    cAfterMethod    = "":U . 
            
            /* Check for setup/teardown/before/after methods */
            {Consultingwerk/foreachABL.i ClassMember oClassMember in oListClassMember}
                
                IF NOT oClassMember:MemberType = ClassMemberTypeEnum:METHOD THEN 
                    NEXT . 
                
                IF cSetupMethod = "":U AND VALID-OBJECT (oClassMember:Annotations:FindByName ("@Setup":U)) THEN 
                    ASSIGN cSetupMethod = oClassMember:Name .

                IF cTearDownMethod = "":U AND VALID-OBJECT (oClassMember:Annotations:FindByName ("@TearDown":U)) THEN 
                    ASSIGN cTearDownMethod = oClassMember:Name .

                IF cBeforeMethod = "":U AND VALID-OBJECT (oClassMember:Annotations:FindByName ("@Before":U)) THEN 
                    ASSIGN cBeforeMethod = oClassMember:Name .

                IF cAfterMethod = "":U AND VALID-OBJECT (oClassMember:Annotations:FindByName ("@After":U)) THEN 
                    ASSIGN cAfterMethod = oClassMember:Name .
            END.            

            IF cBeforeMethod > "":U THEN DO ON ERROR UNDO, THROW:
                THIS-OBJECT:ExecuteManagementMethod ("Before":U, oTestInstance, cBeforeMethod) . 
            
                CATCH oError AS Progress.Lang.Error:
        
                    CREATE eTestcase.
                    ASSIGN eTestcase.TestsuiteName   = eTestsuite.TestsuiteName 
                           eTestcase.ClassName       = oTestInstance:GetClass():TypeName 
                           eTestcase.TestcaseName    = cBeforeMethod
                           eTestcase.ErrorMessage    = SUBSTITUTE ("&1: &2":U,
                                                                   ErrorHelper:ErrorTypeName (oError),
                                                                   ErrorHelper:FormattedErrorMessages (oError))
                           eTestcase.ErrorStacktrace = ErrorHelper:StackTrace (oError) 
                           eTestcase.ErrorType       = ErrorHelper:ErrorTypeName (oError)
                           .
                    NEXT . 
        
                END CATCH.            
            END.
            
            /* Execute test methods */
            {Consultingwerk/foreachABL.i ClassMember oClassMember in oListClassMember nodefine}
    
                IF oClassMember:MemberType = ClassMemberTypeEnum:Method THEN DO ON ERROR UNDO, THROW: 
                    
                    oMemberTestAnnotation = oClassMember:Annotations:FindByName ("@Test":U) .
                    
                    IF VALID-OBJECT (oMemberTestAnnotation) THEN DO ON ERROR UNDO, THROW:
                    
                        ASSIGN cExpected = oMemberTestAnnotation:Parameters:GetValueByPair ("expected":U) . 
                        
                        IF VALID-OBJECT (oMemberTestAnnotation:Parameters:FindByName ("stop-after":U)) THEN 
                            iStopAfter = INTEGER (oMemberTestAnnotation:Parameters:GetValueByPair ("stop-after":U)) .
                        ELSE IF VALID-OBJECT (oMemberTestAnnotation:Parameters:FindByName ("stopafter":U)) THEN 
                            iStopAfter = INTEGER (oMemberTestAnnotation:Parameters:GetValueByPair ("stopafter":U)) .
                        ELSE iStopAfter = 0 . 

                        CREATE eTestcase.
                        ASSIGN eTestcase.TestsuiteName = eTestsuite.TestsuiteName 
                               eTestcase.ClassName     = cClass 
                               eTestcase.TestcaseName  = oClassMember:Name.
                                                
                        IF THIS-OBJECT:LogMessages THEN
                            MESSAGE "Executing Test:":U oClassMember:Name  .  
                
                        DO ON ERROR UNDO, THROW:

                            SmartUnit:CurrentTestParameters = oMemberTestAnnotation:Parameters .
    
                            oResult = THIS-OBJECT:ExecuteTest (poTestSuite,
                                                               cLogFolder,
                                                               cClass,
                                                               oTestInstance,
                                                               oClassMember:Name,
                                                               cSetupMethod,
                                                               cTearDownMethod,
                                                               cExpected,
                                                               iStopAfter) .

                            FINALLY:
                                ASSIGN SmartUnit:CurrentTestParameters = ? 
                                       eTestcase.ErrorMessage          = oResult:ErrorMessage
                                       eTestcase.ErrorStacktrace       = oResult:ErrorStacktrace
                                       eTestcase.ErrorType             = oResult:ErrorType 
                                       eTestcase.ExecutionTime         = oResult:ExecutionTime .       
                                       
                                IF oResult:SystemOut > "":U THEN                                         
                                    lcSystemOut = lcSystemOut + oResult:SystemOut + Consultingwerk.Environment:NewLine .
    
                                IF oResult:SystemErr > "":U THEN                                         
                                    lcSystemErr = lcSystemErr + oResult:SystemErr + Consultingwerk.Environment:NewLine .
                            END FINALLY.
                        END. 
                    END. /* @Test. Member Annotation */
                END. /* MemberType:Method */
            END .  /* foreach ClassMember */

            IF cAfterMethod > "":U THEN DO ON ERROR UNDO, THROW:
                THIS-OBJECT:ExecuteManagementMethod ("After":U, oTestInstance, cAfterMethod) . 
            
                CATCH oError AS Progress.Lang.Error:
        
                    CREATE eTestcase.
                    ASSIGN eTestcase.TestsuiteName   = eTestsuite.TestsuiteName 
                           eTestcase.ClassName       = oTestInstance:GetClass():TypeName 
                           eTestcase.TestcaseName    = cAfterMethod
                           eTestcase.ErrorMessage    = SUBSTITUTE ("&1: &2":U,
                                                                   ErrorHelper:ErrorTypeName (oError),
                                                                   ErrorHelper:FormattedErrorMessages (oError))
                           eTestcase.ErrorStacktrace = ErrorHelper:StackTrace (oError) 
                           eTestcase.ErrorType       = ErrorHelper:ErrorTypeName (oError)
                           .
                    NEXT . 
        
                END CATCH.            
            END.


            CATCH err AS Progress.Lang.Error:
                IF THIS-OBJECT:DialogOutput = TRUE THEN 
                    ErrorHelper:ShowErrorMessage (err, SUBSTITUTE ("Error processing file: &1":U, cFileName)) .                
                ELSE 
                    MESSAGE "Error processing file:":U cFileName SKIP 
                            ErrorHelper:FormattedErrorMessagesExt (err) . 
                        
                RETURN FALSE . 
            END CATCH.
            
            FINALLY:
                GarbageCollectorHelper:DeleteObject (oTestInstance) .       
            END FINALLY.
            
        END. /* foreach CHARACTER cClass */

        RETURN TRUE . 

        FINALLY:
            IF poTestSuite:ImportStdOut THEN 
                ASSIGN eTestsuite.SystemOut = lcSystemOut .
            IF poTestSuite:ImportClientLog THEN 
                ASSIGN eTestsuite.SystemErr = lcSystemErr . 
                   
            IF poTestSuite:Output > "":U THEN  
                SmartUnit:WriteTestResult (poTestSuite:Output,
                                           DATASET dsTestsuites BY-REFERENCE) .       
        END FINALLY.

    END METHOD . 

	/*------------------------------------------------------------------------------
	    Purpose: Executes a test management method (setup, teardown, before, after)
	    Notes:
	    @param pcLabel The label for the setup method
        @param poTestInstance The reference to the Test instance
	    @param pcMethodName The name of the method to execute
	------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID ExecuteManagementMethod (pcLabel AS CHARACTER,
                                                   poTestInstance AS Progress.Lang.Object,
	                                               pcMethodName AS CHARACTER):
		
		IF THIS-OBJECT:LogMessages THEN 
		    MESSAGE "Executing":U pcLabel "(":U + pcMethodName + ")":U .

        DYNAMIC-INVOKE (poTestInstance, pcMethodName) .

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Executes a single test
        Notes:  
        @param poTestSuite The reference to the current test suite 
        @param pcLogFolder The name of the folder to use for logfiles etc.
        @param pcClassName The name of the test class
        @param poTestInstance The reference to the Test instance
        @param pcTestName The name of the test method to execute
        @param pcSetupMethod The name of the setup method 
        @param pcTearDownMethod The name of the tear down method
        @param pcExpected The class of error that is expected to be thrown by the test method
        @param piStopAfter The number of seconds after which the test is supposed to be stopped
        @return The TestResult of the executed test
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED TestResult ExecuteTest (poTestSuite      AS ITestSuite,
                                             pcLogFolder      AS CHARACTER, 
                                             pcClassName      AS CHARACTER, 
                                             poTestInstance   AS Progress.Lang.Object,
                                             pcTestName       AS CHARACTER,
                                             pcSetupMethod    AS CHARACTER, 
                                             pcTearDownMethod AS CHARACTER,
                                             pcExpected       AS CHARACTER, 
                                             piStopAfter      AS INTEGER):
        
        DEFINE VARIABLE oResult         AS TestResult          NO-UNDO . 
        DEFINE VARIABLE cLogfileName    AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cOutputFileName AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE dtTestStart     AS DATETIME-TZ         NO-UNDO .
        DEFINE VARIABLE lStopCondition  AS LOGICAL             NO-UNDO INIT TRUE .
        DEFINE VARIABLE oError          AS Progress.Lang.Error NO-UNDO . 
        DEFINE VARIABLE lcLogFragment   AS LONGCHAR            NO-UNDO .
        
        ASSIGN cLogfileName    = SUBSTITUTE ("&1/&2 &3.log":U,
                                             pcLogFolder,
                                             pcClassName,
                                             pcTestName)
               cOutputFileName = SUBSTITUTE ("&1/&2 &3.out":U,
                                             pcLogFolder,
                                             pcClassName,
                                             pcTestName)  .
        
        oResult = NEW TestResult () .
        
        LOG-MANAGER:LOGFILE-NAME = cLogfileName .
        LOG-MANAGER:LOG-ENTRY-TYPES = "4GLTrace:5,4GLMessages,QryInfo":U .

        IF poTestSuite:ImportStdOut THEN 
            OUTPUT TO VALUE (cOutputFileName) KEEP-MESSAGES .
        
        /* When piStopAfter is ? or 0 the STOP-AFTER option is ignored */
        /* Mike Fechner, Consultingwerk Ltd. 04.04.2013
           STOP-AFTER only on OE11 */
        DO 
&IF PROVERSION NE "10.2B" &THEN    
           STOP-AFTER piStopAfter 
&ENDIF  
        ON STOP UNDO, LEAVE:
            
            DO ON ERROR UNDO, THROW:
                ASSIGN dtTestStart    = NOW . 
    
                IF pcSetupMethod > "":U THEN 
                    THIS-OBJECT:ExecuteManagementMethod ("Setup":U, poTestInstance, pcSetupMethod) . 
    
                DYNAMIC-INVOKE (poTestInstance, pcTestName) .

                IF pcTearDownMethod > "":U THEN 
                    THIS-OBJECT:ExecuteManagementMethod ("Tear-Down":U, poTestInstance, pcTearDownMethod) . 
    
                CATCH err AS Progress.Lang.Error:
                    /* Keep error */
                    oError = err .                      
                END CATCH.
    
                FINALLY:
                    IF poTestSuite:ImportStdOut THEN 
                        OUTPUT CLOSE .

                    LOG-MANAGER:CLOSE-LOG () .

                    ASSIGN lStopCondition        = FALSE 
                           oResult:ExecutionTime = INTERVAL (NOW, dtTestStart, "milliseconds":U) / 1000.                    
                END FINALLY.
            END.
        END .

        /* Test for Stop condition from the code block above */
        IF lStopCondition THEN DO:
            IF poTestSuite:ImportStdOut THEN 
                OUTPUT CLOSE .

            LOG-MANAGER:CLOSE-LOG () .

            ASSIGN oResult:ExecutionTime   = INTERVAL (NOW, dtTestStart, "milliseconds":U) / 1000
                   oResult:ErrorMessage    = "A STOP Condition has occured.":U   
                   oResult:ErrorType       = "":U 
                   oResult:ErrorStacktrace = "":U .
        END .                     
        ELSE DO:
            /* Mike Fechner, Consultingwerk Ltd. 25.03.2013
               When we were actually expecting an error, but no error was thrown,
               this is a problem! */
            IF pcExpected > "":U AND NOT VALID-OBJECT (oError) THEN DO:
                ASSIGN oResult:ErrorMessage    = SUBSTITUTE ("Expected error ~"&1~" was not thrown!":U,
                                                             pcExpected) 
                       oResult:ErrorType       = "":U 
                       oResult:ErrorStacktrace = "":U . 
            END. 
            ELSE DO:
                IF pcExpected > "":U THEN DO:
                    IF NOT oError:GetClass ():IsA (pcExpected) THEN DO:
                        ASSIGN oResult:ErrorMessage    = SUBSTITUTE ("Expected errror ~"&1~", but a different error was thrown!":U,
                                                                     pcExpected) + Consultingwerk.Environment:NewLine + 
                                                         SUBSTITUTE ("&1: &2":U,
                                                                     ErrorHelper:ErrorTypeName (oError),
                                                                     ErrorHelper:FormattedErrorMessages (oError)) 
                               oResult:ErrorType       = ErrorHelper:ErrorTypeName (oError) 
                               oResult:ErrorStacktrace = ErrorHelper:StackTrace (oError) .
                    END.
                    /* no else, because we got the expected error */
                END.
                ELSE 
                    ASSIGN oResult:ErrorMessage    = SUBSTITUTE ("&1: &2":U,
                                                                 ErrorHelper:ErrorTypeName (oError),
                                                                 ErrorHelper:FormattedErrorMessages (oError))
                           oResult:ErrorType       = ErrorHelper:ErrorTypeName (oError) 
                           oResult:ErrorStacktrace = ErrorHelper:StackTrace (oError) .
            END.
        END. 
        
        FINALLY:
            IF poTestSuite:ImportStdOut THEN DO: 
                FILE-INFO:FILE-NAME = cOutputFileName .
                IF FILE-INFO:FULL-PATHNAME > "":U THEN DO:
                    COPY-LOB FROM FILE cOutputFileName
                        TO lcLogFragment .
                        
                    oResult:SystemOut = StringHelper:PrefixLines (lcLogFragment, 
                                                                  SUBSTITUTE ("[&1] ":U, pcTestName)) .                     
                END.
            END.

            IF poTestSuite:ImportClientLog THEN DO:
                FILE-INFO:FILE-NAME = cLogFileName .
                IF FILE-INFO:FULL-PATHNAME > "":U THEN DO:
                    COPY-LOB FROM FILE cLogFileName
                        TO lcLogFragment .
    
                    oResult:SystemErr = StringHelper:PrefixLines (lcLogFragment, 
                                                                  SUBSTITUTE ("[&1] ":U, pcTestName)) .                     
                END.
            END.
                        
            RETURN oResult .
                    
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Loads services from a comma-delimited list of XML files
        Notes:   
        @param pcServices The comma delimited list of service xml files
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LoadServices (pcServices AS CHARACTER):
        
        DEFINE VARIABLE oLoader AS ServiceLoader NO-UNDO . 
        DEFINE VARIABLE i       AS INTEGER       NO-UNDO .
        
        IF CharacterType:IsNullOrEmpty (pcServices) THEN 
            RETURN . 

        oLoader = NEW ServiceLoader () .

        DO i = 1 TO NUM-ENTRIES (pcServices):
            MESSAGE "Loading services from:":U ENTRY (i, pcServices) .
            
            oLoader:Load (ENTRY (i, pcServices)) .
        END.        

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oLoader) .     
        END FINALLY.

    END METHOD .

END CLASS.
