using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.IO;
using Progress.Open4GL.Proxy;
using ASINET;
using ASIDataNS;



/// <summary>
/// Summary description for AppServerConnect
/// </summary>
namespace AppServerConnect
{
    public class AppServer
    {
        private Connection conn;
        protected asinet aoObject;
        private string ConnString;

        public AppServer()
        {
            TextReader tr = new StreamReader("AppServerShip.txt");          

            ConnString = tr.ReadLine();

            tr.Close();

        }

        public void AppServerConnect()
        {
            string strHold;
            conn = new Connection(ConnString, "", "", "");
            try
            {
                aoObject = new asinet(conn);
            }
            catch (Progress.Open4GL.Exceptions.ConnectException ex)
            {
                strHold = ex.ToString();
            }
            catch (Progress.Open4GL.DynamicAPI.SessionPool.NoAvailableSessionsException ex)
            {
                strHold = ex.ToString();
            }
            
        }

        public void AppServerDisconnect()
        {
            try
            {
                conn.ReleaseConnection();
                conn.Dispose();
                aoObject = null;
                conn = null;
            }
            catch { }
        }

        ~AppServer()
        {
            if (aoObject != null)
            {
                aoObject = null;
                conn = null;
            }
        }
    }
}
