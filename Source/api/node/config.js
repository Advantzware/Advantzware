/* config.js contain common config information as below: */
// javaProgramDir : this is the relative path (path from where node server is started) where java programs are stored on Node 
// javaLogsDir : this is the relative path (path from where node server is started) where logs are stored on Node if APPServer is down
// javaProgram: this is the name of the java program which gets called by Node for processing a request
// appServerURL: this is the URL to Progress AppServer. This needs update based on the targeted AppServer broker name
// *** if AppServer and Node are running on the same machine then update "appServerURL" to "AppServer://localhost:<port>/Advantzware_API"
// *** if AppServer and Node are running on different machines then "AppServer://<ip>:<port>/Advantzware_API"
// appServerRequestRouter: this is Progress layer common program
// serverIP: This is IP address for the server on which node server runs.This needs to be updated based on targeted server.
// serverPort: This is port number on which node server starts

const config = {"javaProgramDir":process.env.API_JAVA_PROGRAMS_DIR, 
				"javaLogsDir":process.env.API_JAVA_LOGS_DIR,
				"javaProgram":process.env.API_JAVA_PROCESSOR,
				"appServerURL":process.env.API_APPSERVER_URL,
				"appServerRequestRouter":process.env.API_APPSERVER_REQUEST_ROUTER,
				"serverIP":process.env.API_IP_ADDRESS,
				"serverPort":process.env.API_PORT
			   };
			   
module.exports = {
    config
}