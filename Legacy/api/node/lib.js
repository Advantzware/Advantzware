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
	
	isBadXML:function(data){
		var error=false;
		// this is to be updated
		return (error);
	},
	
	XMLResponse: function(responseCode,responseMessage){
		return xml({"response":[{"response_code":responseCode},{"response_message":responseMessage}]});
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

	// this function autheticates
	Authenticated: function(req){
		const authCredentials = {username: config.authUsername, password: config.authPassword};
		reqCredentials = self.getCredentialsFromRequest(req);
		return (reqCredentials.requestUsername && reqCredentials.requestPassword && 
			reqCredentials.requestUsername === authCredentials.username && 
			reqCredentials.requestPassword === authCredentials.password);
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
				 self.getRequestDataType(req), //ipRequestDataType
				 ipRequestData,      //ipRequestData
				 config.DLC          // Progress Install Dir (DLC)
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

	//this creates CSV log data if AppServer is down
	CSVFileDataCreate: function(req,requestData,responseData){
		const date = self.getDateString();
		const dateTime = date + ' ' + self.getTimeString();;
		const csvFilename  = config.logsDir + "APIInboundJavaServer_" + date + ".csv";
		const writer = csvWriter({sendHeaders: false,separator: '|'});

		writer.pipe(fs.createWriteStream(csvFilename, {flags: 'a'}));
		writer.write({
		  APIRoute        : req.originalUrl,
		  RequestVerb     : req.method,
		  RequestData     : requestData,
		  ResponseData    : responseData,
		  RequestDataType : self.getRequestDataType(req),
		  AppServerURL    : config.appServerURL,
		  ErrorMessage    : responseData.exception,
		  RequestDateTime : dateTime,
		});
		writer.end();
	}
};

module.exports = self;