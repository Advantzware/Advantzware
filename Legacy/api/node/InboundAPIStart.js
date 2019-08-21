const express = require('express');
const app = express();

// this should be test to production
app.set("env", "production");

//this is common functions library 
const lib = require("./lib.js");

//this creates datetime
const dateTime = lib.getDateString() + ' ' + lib.getTimeString();

//this is common variables library 
const {config} = require("./config.js");

//this verifies whether environment variables set properly/not
if (config.javaProgramDir === undefined         || 
    config.javaLogsDir === undefined            || 
	config.javaProgram === undefined            || 
	config.appServerURL === undefined   	    || 
	config.appServerRequestRouter === undefined || 
	config.serverIP === undefined               || 
	config.serverPort === undefined) {
	if (config.javaProgramDir === undefined) {
		console.log(`${dateTime} Error: Environment variable API_JAVA_PROGRAMS_DIR is not set`);
	}
	if (config.javaLogsDir === undefined) {
		console.log(`${dateTime} Error: Environment variable API_JAVA_LOGS_DIR is not set`);
	}
	if (config.javaProgram === undefined) {
		console.log(`${dateTime} Error: Environment variable API_JAVA_PROCESSOR is not set`);
	}
	if (config.appServerURL === undefined) {
		console.log(`${dateTime} Error: Environment variable API_APPSERVER_URL is not set`);
	}
	if (config.appServerRequestRouter === undefined) {
		console.log(`${dateTime} Error: Environment variable API_APPSERVER_REQUEST_ROUTER is not set`);
	}
	if (config.serverIP === undefined) {
		console.log(`${dateTime} Error: Environment variable API_IP_ADDRESS is not set`);
	};
	if (config.serverPort === undefined) {
		console.log(`${dateTime} Error: Environment variable API_PORT is not set`);
	};
	console.log(`${dateTime} Error: Node could not be started!`);
	process.exit(1);
}

// InboundAPIRoutesJSON.js has all JSON based routes definitions
const JSONRoutes = require('./InboundAPIRoutesJSON.js');
app.use('/api', JSONRoutes);

// InboundAPIRoutesXML.js has all XML based routes definitions
const XMLRoutes = require('./InboundAPIRoutesXML.js');
app.use('/api', XMLRoutes);

// this makes Node listen on the port 
app.listen(config.serverPort, config.serverIP, () => console.log(`${dateTime} Node server started with the following configuration:\n${lib.fillSpaces(20)}javaProgramDir="${config.javaProgramDir}",
${lib.fillSpaces(20)}javaLogsDir="${config.javaLogsDir}",
${lib.fillSpaces(20)}javaProgram="${config.javaProgram}",
${lib.fillSpaces(20)}appServerURL="${config.appServerURL}",
${lib.fillSpaces(20)}appServerRequestRouter="${config.appServerRequestRouter}",
${lib.fillSpaces(20)}serverIP="${config.serverIP}",
${lib.fillSpaces(20)}serverPort="${config.serverPort}"`));
