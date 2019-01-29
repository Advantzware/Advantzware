using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.IO;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class print_download_list : System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {        
        try
        {

            string att1 = Request.QueryString["filename"];           
            string str2 = Path.GetFileName(att1);

            string path = str2;
            if (path != "")
            {
                if (!Request.Browser.Browser.Contains("Safari"))
                {

                    string fileName = str2;
                    System.IO.FileStream fs = null;
                    fs = System.IO.File.Open(att1, System.IO.FileMode.Open); ;
                    byte[] btFile = new byte[fs.Length];
                    fs.Read(btFile, 0, Convert.ToInt32(fs.Length));
                    fs.Close();
                    Response.AddHeader("Content-disposition", "attachment; filename=" + fileName);
                    Response.ContentType = "application/octet-stream";
                    Response.BinaryWrite(btFile);
                    Response.End();
                    fs = null;
                }
            }
        }
        catch { }
    }
}
