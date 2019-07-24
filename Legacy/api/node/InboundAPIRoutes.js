const express = require('express')
const router = express.Router();
const fs = require('fs');
//this package is required for csv generation
const csvWriter = require('csv-write-stream');
//this sends headers as null
var writer = csvWriter({sendHeaders: false,separator: '|'});
// this is required for running java program
const jre = require('node-jre');
// this is the relative path (path from where node server is started) where java programs are stored on Node 
const javaProgramDir = "java";
// this is the relative path (path from where node server is started) where logs are stored on Node if APPServer is down
const logsDir = "java\\logs\\";
// this is the name of the java program which gets called by Node for processing a request
const javaProgram  = "RequestDispatcher";
// this is the URL to Progress AppServer. This needs update based on the targeted AppServer broker name
const appServerURL = "AppServer://localhost:5162/restbroker1";
// this should have \\ to escape backslash. DLC needs to change as per the server configuration 
const DLC = "C:\\Progress\\OE116_64\\" 
// username/password which others can use while making calls to inbound API enpoints. This needs to change accordingly
const authUsername = "user1";
const authPassword = "user1";

var requestUsername;
var requestPassword;
var requestDataType;
var requestData;
var errorMessage;
var csvFilename;

// this function separates username/password from request headers
function getCredentialsFromRequest(req){
    const authorizationData = (req.headers.authorization || '').split(' ')[1] || '';  
    requestUsername = Buffer.from(authorizationData,'base64').toString().split(':')[0];
    requestPassword = Buffer.from(authorizationData,'base64').toString().split(':')[1];
}

// this function returns the content-type of the request data
function getRequestDataType(req){
    var contentType= req.headers['content-type'];
    return contentType.split('/')[1];
}

// this function autheticates
function Authenticated(req){
    const authCredentials = {username: authUsername, password: authPassword};
    getCredentialsFromRequest(req);
    return (requestUsername && requestPassword && 
        requestUsername === authCredentials.username && 
        requestPassword === authCredentials.password);
}

// this function calls Java program
function callJavaProgram(req){
    requestDataType = getRequestDataType(req);
    // this will convert the nested JSON to flat JSON
    const convertToFlatJSON = (obj, prefix = '') =>
        Object.keys(obj).reduce((resultJSON, itemNum) => {
            const prefixChar = prefix.length ? prefix + '.' : '';
            if (typeof obj[itemNum] === 'object') 
                Object.assign(resultJSON, convertToFlatJSON(obj[itemNum], prefixChar + itemNum));
            else 
                resultJSON[prefixChar + itemNum] = obj[itemNum];
            return resultJSON;
        }, {});
    requestData = convertToFlatJSON(req.body);  
    requestData = JSON.stringify(requestData);
    // calling the Java program     
    const output = jre.spawnSync( 
            [javaProgramDir],    //relative directory - should be on Node server
             javaProgram,        //java program
            [appServerURL,       //ipAppServerURL
             req.originalUrl,    //ipRoute
             req.method,         //ipVerb
             requestUsername,    //ipUsername
             requestPassword,    //ipPassword
             requestDataType,    //ipRequestDataType
             requestData,        //ipRequestData
             DLC
            ],      
            { encoding: 'utf8' } // encode output as string
          ).stdout.trim();       // take output from stdout as trimmed String
    return output;
}

// this function initiates API Route call
function handleRoute(req,res){
    try {
        if (!Authenticated(req)){
            res.set('WWW-Authenticate', 'Basic realm="401"');
            res.status(401).send({'response_code':401,'response_message': 'Invalid Username/Password supplied'});
            res.end();
        }
        else {
            var output = JSON.parse(callJavaProgram(req));
            if (output.response_code === 500){
                var date = getDateString();
                var time = getTimeString();
                var dateTime = date + ' ' + time;
                csvFilename  = logsDir + "APIInboundJavaServer_" + date + ".csv";
                errorMessage = output.exception;
                output = {'response_code':output.response_code,'response_message': output.response_message};
                // writes the request data to csv file in case AppServer is down
                CSVFileDataCreate(req,output,dateTime,errorMessage,csvFilename);
            }
                        
            res.setHeader('Content-Type', 'text/json');
            res.status(output.response_code).send(output);
            res.end();
        }
    }
    catch ( e ) {
        output = {'response_code':400,'response_message': 'Bad response' + e,'exception':e};
        res.setHeader('Content-Type', 'text/json');
        res.status(400).send(output);
        res.end();      
    }   
}

function getDateString(){
    var today = new Date();
    var date = today.getFullYear() + '-' + (today.getMonth() + 1) + '-' + today.getDate();
    
    return date;
}

function getTimeString(){
    var today = new Date();
    var time = today.getHours() + ":" + today.getMinutes() + ":" + today.getSeconds();
    
    return time;
}

// This will handle if any invalid route is supplied    
function routeNotFound(res){
   res.setHeader('Content-Type', 'text/json');
   res.status(404).send({"response_code":404,"response_message":"route not found!"});
   res.end();
}

//this creates CSV log data if AppServer is down
function CSVFileDataCreate(req,output,dateTime,errorMessage,csvFilename){
    writer = csvWriter({sendHeaders: false,separator: '|'});
    writer.pipe(fs.createWriteStream(csvFilename, {flags: 'a'}));
    writer.write({
      APIRoute        : req.originalUrl,
      RequestVerb     : req.method,
      RequestData     : requestData,
      ResponseData    : JSON.stringify(output),
      RequestDataType : requestDataType,
      AppServerURL    : appServerURL,
      ErrorMessage    : errorMessage,
      RequestDateTime : dateTime,
    });
    writer.end();
}

/* ----------------------------- routes handlers ---------------------------------*/

//This route is for fetching the inventory details
router.get('/getInventory', (req,res) => {
    handleRoute(req, res)
});

//This will create an inventory transfer record
router.post('/createInventoryTransfer', (req,res) => {
    handleRoute(req, res)
});

// validation for undefined routes for POST
router.post('*', function(req, res) {
   routeNotFound(res);
});

// validation for undefined routes for GET
router.get('*', function(req, res) {
   routeNotFound(res);
});

module.exports = router;
