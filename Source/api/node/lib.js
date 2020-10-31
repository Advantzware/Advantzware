//this is common variables library 
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
	// This will compact the XML by removing spaces and new line characters
	getCompactXML: function(strXML){
		var compactXML = strXML.replace(/>\s*/g, '>');  // Remove space after >
		compactXML = compactXML.replace(/\s*</g, '<');  // Remove space before <
		compactXML = compactXML.replace(new RegExp( "\\n", "g" ),""); // Remove new lines
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
	
	// this function initiates JSON API Route call
    handleRouteJSON: async function(req,res){
		var responseCode = "";
		var response = "";
		var responseExceptionMessage = "";
		
		res.setHeader('Content-Type', 'text/json');
		
		try {
			if (self.getRequestDataType(req) != "json"){
				responseCode = 400;
				response = self.JSONResponse(responseCode,"This API accepts JSON based request");
			}			 
			else {
				var JSONRequestData = self.getFlatJSON(req.body);  
				JSONRequestData = JSON.stringify(JSONRequestData);
				if (JSONRequestData === "{}") { 
					response = self.JSONResponse(400,"Empty request");
					res.status(400).send(response);
					res.end(); 
					return;
				}
				response = await self.callJavaProgram(req,config,JSONRequestData);
				if (response.length > 0) {
					try{
						response = JSON.parse(response);
					}
					catch ( e ) {
						self.CSVFileDataCreate(req,JSONRequestData,response,e.message);
					}
					if (!response) {
						response = {"response_code":500,"response_message":"Internal Server Error at Node (#4) - JSON response is Invalid"};
					}
					responseCode = response.response_code;
					if (response.exception) {
						responseExceptionMessage = " - " + response.exception;
					}
					if (responseCode === 500 || response.response_code === 404){
						// writes the request data to csv file in case AppServer is down
						self.CSVFileDataCreate(req,JSONRequestData,JSON.stringify(response),responseExceptionMessage);
						response = self.JSONResponse(response.response_code,response.response_message + responseExceptionMessage);
					}   
				}
				else {
					responseCode = 500;
					response = self.JSONResponse(responseCode,"Internal Server Error at Node (#5) - JSON response is empty");						
				}
			}

			res.status(responseCode).send(response);
			res.end();
		}
		catch ( e ) {
			console.log(self.getDateTimeString() + " " + e);
			response = self.JSONResponse(400,"Internal Server Error at Node (#6) - " +e.message);
			res.status(400).send(response);
			res.end();      
		}   
    },
	
	// this function initiates cXML API Route call - cXML specific
    handleRoutecXML: async function(req,res){
		var responseCode = "";
		var response = "";
		var responseMessage = "";
		var responseExceptionMessage = "";
		var responseWithException = "";
		
		res.setHeader('Content-Type', 'text/xml');
		
		try {
			if (self.getRequestDataType(req) != "xml"){
				responseCode = 400
				response = self.cXMLResponse(responseCode,"This API accepts cXML based requests");
			}
			else{
				const XMLRequestData = self.getCompactXML(req.rawBody);
				if (XMLRequestData.length == 0) { 
					response = self.cXMLResponse(400,"Empty request");
					res.status(400).send(response);
					res.end(); 
					return;
				}

				response = await self.callJavaProgram(req,config,XMLRequestData);
				if (response.length > 0) {
					try{
						response = JSON.parse(response);
					}
					catch ( e ) {
						self.CSVFileDataCreate(req,XMLRequestData,response,e.message);
					}
					if (!response) {
						response = {"response_code":500,"response_message":"Internal Server Error at Node (#1) - XML issue with the response format"};
					}
					responseCode = response.response_code;	
					responseMessage = response.response_message;
					
					if (response.exception) {
						responseExceptionMessage = " - " + response.exception;
					}
					
					response = self.cXMLResponse(response.response_code,response.response_message + responseExceptionMessage);
					
					if (responseCode === 500 || responseCode === 404){
						// writes the request data to csv file in case AppServer is down
						responseWithException = self.XMLResponseWithException(responseCode,responseMessage,responseExceptionMessage);
						self.CSVFileDataCreate(req,XMLRequestData,responseWithException,responseExceptionMessage);
					}
				}
				else {
					responseCode = 500;
					response = self.cXMLResponse(responseCode,"Internal Server Error at Node (#2) - XML response is empty");						
				}
			}

			if (responseMessage.includes("cXML")) {
				response = responseMessage;
			}
			
			res.status(responseCode).send(response);
			res.end();
		}
		catch ( e ) {
			console.log(self.getDateTimeString() + " " + e);
			response = self.cXMLResponse(500,"Internal Server Error at Node (#3) - "+e.message);
			res.status(500).send(response);
			res.end();      
		}   
	},
	
	// this function prepares XML response
	XMLResponse: function(responseCode,responseMessage){
		return '<?xml version="1.0" encoding="UTF-8"?>' + 
			   xml({"response":[{"response_code":responseCode},{"response_message":responseMessage}]});
	},
	
	// this function prepares XML response to write in CSV file
	XMLResponseWithException: function(responseCode,responseMessage,responseExceptionMessage){
		return xml({"response":[{"response_code":responseCode},{"response_message":responseMessage},{"exception":responseExceptionMessage}]});
	},
	
	// this function prepares JSON response
	JSONResponse: function(responseCode,responseMessage){
		return {'response_code':responseCode,'response_message': responseMessage};
	},
	
	// this will convert the nested JSON to flat JSON
	getFlatJSON: function(data){
		const convertToFlatJSON = (obj, prefix = '') =>
			Object.keys(obj).reduce((resultJSON, itemNum) => {
				const prefixChar = prefix.length ? prefix + '.' : '';
				if (typeof obj[itemNum] === 'object') 
					Object.assign(resultJSON, convertToFlatJSON(obj[itemNum], prefixChar + itemNum.toString().replace(/,/g,'#comma#').replace(/:/g,'#colon#').replace(/\./g,'#period#')));
				else 
					resultJSON[prefixChar + itemNum.toString().replace(/,/g,'#comma#').replace(/:/g,'#colon#').replace(/\./g,'#period#')] = obj[itemNum].toString().replace(/,/g,'#comma#').replace(/:/g,'#colon#'); // replace any comma, colon or period in the key and value, and handle in Progress				
				return resultJSON;
			}, {});
		return convertToFlatJSON(data);
	},
	// this function returns the content-type of the request data
	getRequestDataType: function(req){
		var contentType= req.headers['content-type'];
		if (contentType) 
			return contentType.split('/')[1];
		else
			return "plain";
	},

	asyncSpawn: function (classPath, className, args, options) {
	  return new Promise((resolve, reject) => {
		const handle = jre.spawn(classPath, className, args, options);
		const stdouts = [];
		const stderrs = [];

		handle.stdout.on("data", (data) => {
		  stdouts.push(data);
		});

		handle.stderr.on("data", (data) => {
		  stderrs.push(data);
		});

		handle.on("exit", (status, signal) => {
		  resolve({
			output: stdouts,
			stdout: Buffer.concat(stdouts).toString(),
			stderr: Buffer.concat(stderrs).toString(),
			status,
			signal,
		  });
		});
	  });
	},
	
	// this function calls Java program
	callJavaProgram: async function(req,config,ipRequestData){
		reqCredentials = self.getCredentialsFromRequest(req);
		// calling the Java program     
		const { stdout } = await self.asyncSpawn( 
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
			  );       // take output from stdout as trimmed String
		const output = stdout.trim();
		return output;
	},
    // this function separates username/password from request headers
	getCredentialsFromRequest: function(req){
		const authorizationData = (req.headers.authorization || '').split(' ')[1] || '';  
		const requestUsername = Buffer.from(authorizationData,'base64').toString().split(':')[0];
		const requestPassword = Buffer.from(authorizationData,'base64').toString().split(':')[1];
		return {"requestUsername":requestUsername,"requestPassword":requestPassword};
	},
	
	// this function returns date
	getDateString: function(){
		var today = new Date();
		var currentMonth=('0'+(today.getMonth()+1)).slice(-2);
		var currentDay=('0'+(today.getDate())).slice(-2);
		var date = today.getFullYear() + '-' + currentMonth + '-' + currentDay;
		
		return date;
	},

	// this function returns time
	getTimeString: function(){
		var today = new Date();
		var hours=('0'+today.getHours()).slice(-2);
		var minutes=('0'+today.getMinutes()).slice(-2);
		var seconds=('0'+today.getSeconds()).slice(-2);
		var time = hours + ":" + minutes + ":" + seconds;
		
		return time;
	},
	
	// this function returns current time with milliseconds
	getTimeStringWithMilliseconds: function(){
		var today = new Date();
		var hours=('0'+today.getHours()).slice(-2);
		var minutes=('0'+today.getMinutes()).slice(-2);
		var seconds=('0'+today.getSeconds()).slice(-2);
		var milliSeconds=('0'+today.getMilliseconds()).slice(-3);
		var time = hours + ":" + minutes + ":" + seconds + "." + milliSeconds;
		
		return time;
	},
		
	// this function returns date time
	getDateTimeString: function(){
		return self.getDateString() + ' ' + self.getTimeString();
	},
	
	// this function returns time zone
	getTimezone: function(){
		var timezone_offset_min = new Date().getTimezoneOffset(),
			offset_hrs = parseInt(Math.abs(timezone_offset_min/60)),
			offset_min = Math.abs(timezone_offset_min%60),
			timezone_standard;

		if(offset_hrs < 10)
			offset_hrs = '0' + offset_hrs;

		if(offset_min < 10)
			offset_min = '0' + offset_min;

		// Add an opposite sign to the offset
		// If offset is 0, it means timezone is UTC
		if(timezone_offset_min < 0)
			timezone_standard = '+' + offset_hrs + ':' + offset_min;
		else if(timezone_offset_min > 0)
			timezone_standard = '-' + offset_hrs + ':' + offset_min;
		else if(timezone_offset_min == 0)
			timezone_standard = 'Z';

		// Timezone difference in hours and minutes
		// String such as +5:30 or -6:00 or Z
		return timezone_standard; 
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
	},
	
	// this function checks whether API route is valid or not
	routeNotFound: function(req,res){
		switch(self.getRequestDataType(req)) {
			case "json":
				self.routeNotFoundJSON(res);
				break;
			case "xml":
				self.routeNotFoundXML(res);
				break;
			default:
				self.routeNotFoundJSON(res);
		}
	},
	
	// this function prepares cXML response
	cXMLResponse: function(statusCode, responseMessage){
		const cXMLDateTimeString = self.getDateString() + "T" + self.getTimeStringWithMilliseconds() + self.getTimezone(); 			
		const response = '<?xml version="1.0" encoding="UTF-8"?>' +
		                 '<cXML xml:lang="en" payloadID="" timestamp="' + cXMLDateTimeString +  ' ">' +
			             '<Response>' +
				         '<Status code="' + statusCode + '" text="' + responseMessage + '"/>' + 
			             '</Response>' + 
		                 '</cXML>';
		return response;
	},
	
	// this function checks whether a directory exists, given a path
	isDir: function(path) {
		try {
			var stat = fs.lstatSync(path);
			return stat.isDirectory();
		} catch (e) {
			// lstatSync throws an error if path doesn't exist
			return false;
		}
	} 
};

module.exports = self;