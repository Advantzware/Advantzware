/* config.js contain common config information as below: */
// javaProgramDir : this is the relative path (path from where node server is started) where java programs are stored on Node 
// logsDir : this is the relative path (path from where node server is started) where logs are stored on Node if APPServer is down
// javaProgram: this is the name of the java program which gets called by Node for processing a request
// appServerURL: this is the URL to Progress AppServer. This needs update based on the targeted AppServer broker name
// *** if AppServer and Node are running on the same machine then update "appServerURL" to "AppServer://localhost:<port>/Advantzware_API"
// *** if AppServer and Node are running on different machines then "AppServer://<ip>:<port>/Advantzware_API"
// DLC: this should have \\ to escape backslash. DLC needs to change as per the server configuration 
// authUsername and authPassword: this is to be used while making calls to inbound API enpoints. This needs to change accordingly
// IPJSON: This is IP address for the server on which JSON node server runs.This needs to be updated based on targeted server.
// IPXML: This is IP address for the server on which XML node server runs.This needs to be updated based on targeted server.
// portJSON: This is port number for JSON node server 
// portXML: This is port number for XML node server  
const config = {"javaProgramDir":"java", 
				"logsDir":"java\\logs\\",
				"javaProgram":"RequestDispatcher",
				"appServerURL":"AppServer://localhost:5162/Advantzware_API",
				"DLC":"C:\\Progress\\OE116_64\\",
				"authUsername":"user1",
				"authPassword":"user1",
				"IPJSON":"172.31.19.207",
				"IPXML":"172.31.19.207",
				"portJSON":8444,
				"portXML":8443
			   };
			   
module.exports = {
    config
}