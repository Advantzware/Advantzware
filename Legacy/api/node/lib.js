const {config} = require("./config.js");
//this package is required for file i/o
const fs = require('fs');
//this package is required for csv generation
const csvWriter = require('csv-write-stream');
// this is required for running java program
const jre = require('node-jre');
//this package is required for XML handling
const xml = require('xml');
const DOMParser = require("xmldom").DOMParser;

const self = {
	
	getCompactXML: function(strXML){
		var compactXML = strXML.replace(/>\s*/g, '>');  // Remove space after >
		compactXML = compactXML.replace(/\s*</g, '<');      // Remove space before 
		compactXML = compactXML.replace(new RegExp( "\\n", "g" ),""); // remove new lines
		return compactXML;
	}, 
	
	// This will handle if any invalid route is supplied    
	routeNotFoundJSON: function (res){
		res.setHeader('Content-Type', 'text/json');
		res.status(404).send(self.JSONResponse(404,"route not found!"));
		res.end();
	},
	
	// This will handle if any invalid route is supplied    
	routeNotFoundXML: function (res){
		res.setHeader('Content-Type', 'text/xml');
		res.status(404).send(self.XMLResponse(404,"route not found!"));
		res.end();
	},

	isBadXML:function(data){
		var error=false;
		// this is to be updated
		return (error);
	},
	
	// this function initiates JSON API Route call
    handleRouteJSON:function(req,res){
		var responseCode = "";
		var response = "";
		var responseExceptionMessage = "";
	
		try {
			if (self.getRequestDataType(req) != "json"){
				responseCode = 400;
				response = self.JSONResponse(responseCode,"This API accepts JSON request");
			}			 
			else {
				var JSONRequestData = self.getFlatJSON(req.body);  
				JSONRequestData = JSON.stringify(JSONRequestData);
				
				response = JSON.parse(self.callJavaProgram(req,config,JSONRequestData));
				responseCode = response.response_code;
				responseExceptionMessage = response.exception;
				
				if (responseCode === 500 || response.response_code === 404){
					// writes the request data to csv file in case AppServer is down
					self.CSVFileDataCreate(req,JSONRequestData,JSON.stringify(response),responseExceptionMessage);
					response = self.JSONResponse(response.response_code,response.response_message);
				}   
			}
			
			res.setHeader('Content-Type', 'text/json');
			res.status(responseCode).send(response);
			res.end();
		}
		catch ( e ) {
			res.setHeader('Content-Type', 'text/json');
			response = self.JSONResponse(400,"Bad request - " + e);
			res.status(400).send(response);
			res.end();      
		}   
    },
	// this function initiates XML API Route call
    handleRouteXML:function(req,res){
		var responseCode = "";
		var response = "";
		var responseMessage = "";
		var responseExceptionMessage = "";
		var responseWithException = "";
		
		try {
			if (self.getRequestDataType(req) != "plain"){
				responseCode = 400
				response = self.XMLResponse(responseCode,"This API accepts Text/plain request with XML data");
			}
			else{
				const XMLRequestData = self.getCompactXML(req.body); 
				if(self.isBadXML(XMLRequestData)){
					responseCode = 400
					response = self.XMLResponse(responseCode,"Invalid XML Request Data");
				}
				else{
					response = self.callJavaProgram(req,config,XMLRequestData);
					if (response.length > 0) {
						response = JSON.parse(response);
						if (!response) {
							response = {"response_code":500,"response_message":"Internal Server Error"};
						}
						responseCode = response.response_code;	
						responseMessage = response.response_message;
						responseExceptionMessage = response.exception;
						
						response = self.XMLResponse(response.response_code,response.response_message);
						
						if (responseCode === 500 || responseCode === 404){
							// writes the request data to csv file in case AppServer is down
							responseWithException = self.XMLResponseWithException(responseCode,responseMessage,responseExceptionMessage);
							self.CSVFileDataCreate(req,XMLRequestData,responseWithException,responseExceptionMessage);
						}
					}
					else {
						responseCode = 500;
						response = self.XMLResponse(responseCode,"Internal Server Error");						
					}
				}
			}
			
			res.setHeader('Content-Type', 'text/xml');
			if (responseMessage.includes("cXML")) {
				response = responseMessage;
			}
			
			res.status(responseCode).send(response);
			res.end();
		}
		catch ( e ) {
			res.setHeader('Content-Type', 'text/xml');
			response = self.XMLResponse(400,"Bad request - " + e);
			res.status(400).send(response);
			res.end();      
		}   
	},


	XMLResponse: function(responseCode,responseMessage){
		return xml({"response":[{"response_code":responseCode},{"response_message":responseMessage}]});
	},
	
	XMLResponseWithException: function(responseCode,responseMessage,responseExceptionMessage){
		return xml({"response":[{"response_code":responseCode},{"response_message":responseMessage},{"exception":responseExceptionMessage}]});
	},
	
	JSONResponse: function(responseCode,responseMessage){
		return {'response_code':responseCode,'response_message': responseMessage};
	},
	
	getFlatJSON: function(data){
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
		return convertToFlatJSON(data);
	},
	
	// this function returns the content-type of the request data
	getRequestDataType: function(req){
		var contentType= req.headers['content-type'];
		return contentType.split('/')[1];
	},
	
	// this function calls Java program
	callJavaProgram: function(req,config,ipRequestData){
		reqCredentials = self.getCredentialsFromRequest(req);
		// calling the Java program     
		const output = jre.spawnSync( 
				[config.javaProgramDir], //relative directory - should be on Node server
				 config.javaProgram,  //java program
				[config.appServerURL, //ipAppServerURL
				 req.originalUrl,    //ipRoute
				 req.method,         //ipVerb
				 reqCredentials.requestUsername,    //ipUsername
				 reqCredentials.requestPassword,    //ipPassword
				 self.getRequestDataType(req),      //ipRequestDataType
				 ipRequestData,      //ipRequestData
				 config.appServerRequestRouter          // ipAppServerRequestRouter
				],      
				{ encoding: 'utf8' } // encode output as string
			  ).stdout.trim();       // take output from stdout as trimmed String
		return output;
	},
        // this function separates username/password from request headers
	getCredentialsFromRequest: function(req){
		const authorizationData = (req.headers.authorization || '').split(' ')[1] || '';  
		const requestUsername = Buffer.from(authorizationData,'base64').toString().split(':')[0];
		const requestPassword = Buffer.from(authorizationData,'base64').toString().split(':')[1];
		return {"requestUsername":requestUsername,"requestPassword":requestPassword};
	},
	getDateString: function(){
		var today = new Date();
		var date = today.getFullYear() + '-' + (today.getMonth() + 1) + '-' + today.getDate();
		
		return date;
	},

	getTimeString: function(){
		var today = new Date();
		var time = today.getHours() + ":" + today.getMinutes() + ":" + today.getSeconds();
		
		return time;
	},
	
	fillSpaces: function(num){
		var spaces = "";
		for(counter = 1; counter <= num; counter++){
			spaces += " ";
		}
		return spaces;
	},

	//this creates CSV log data if AppServer is down
	CSVFileDataCreate: function(req,requestData,responseData,responseDataExceptionMessage){
		const date = self.getDateString();
		const dateTime = date + ' ' + self.getTimeString();	
		const backSlash = config.javaLogsDir.substring(config.javaLogsDir.length,1) === "\\" ? "":"\\";
		const csvFilename  = config.javaLogsDir + backSlash + "PendingRequests_" + date + ".csv";
		const writer = csvWriter({sendHeaders: false,separator: '|'});
		writer.pipe(fs.createWriteStream(csvFilename, {flags: 'a'}));
		writer.write({
		  APIRoute        : req.originalUrl,
		  RequestVerb     : req.method,
		  RequestData     : requestData,
		  ResponseData    : responseData,
		  RequestDataType : self.getRequestDataType(req),
		  AppServerURL    : config.appServerURL,
		  ErrorMessage    : responseDataExceptionMessage,
		  RequestDateTime : dateTime,
		});
		writer.end();
	}
};

module.exports = self;