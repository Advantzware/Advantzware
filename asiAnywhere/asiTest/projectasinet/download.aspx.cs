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
using System.Threading;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class print_download_list : System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
        Response.Write("download");
        try
        {

            // string att1 = Request.QueryString["filename"];       
            string att1 = "O:/wwwroot/pdfs/backlog20110505.txt";
            string str2 = Path.GetFileName(att1);
            //if (System.IO.File.Exists(@"//192.168.77.20" + @"O:/wwwroot/pdfs/backlog20110505.txt"))
            //if (System.IO.File.Exists(@"E:/backlog20110503.txt"))
            //DriveInfo.GetDrives("O:/wwwroot/pdfs/");
            if (System.IO.File.Exists("O:/wwwroot/pdfs/backlog20110505.txt"))
            {
                Response.Write("hello");

            }


            string path = str2;
            if (path != "")
            {
                if (!Request.Browser.Browser.Contains("Safari"))
                {

                    string fileName = str2;
                    System.IO.FileStream fs = null;
                    fs = System.IO.File.Open((att1), System.IO.FileMode.Open); ;
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
        catch(Exception g)
        {
            Response.Write(g);
        }
    }

    //protected void Page_Load(object sender, EventArgs e)
    //{
    //   // string filename = Request["file"].ToString();
    //    string filename = "arrow down.jpeg";
    //   // fileDownload(filename, Server.MapPath("O:/wwwroot/pdfs/" + filename));
    //    fileDownload(filename, Server.MapPath("~/Images/" + filename));
    //}
    //private void fileDownload(string fileName, string fileUrl)
    //{
    //    Page.Response.Clear();
    //    bool success = ResponseFile(Page.Request, Page.Response, fileName, fileUrl, 1024000);
    //    if (!success)
    //        Response.Write("Downloading Error!");
    //    Page.Response.End();

    //}
    //public static bool ResponseFile(HttpRequest _Request, HttpResponse _Response, string _fileName, string _fullPath, long _speed)
    //{
    //    try
    //    {
    //        FileStream myFile = new FileStream(_fullPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
    //        BinaryReader br = new BinaryReader(myFile);
    //        try
    //        {
    //            _Response.AddHeader("Accept-Ranges", "bytes");
    //            _Response.Buffer = false;
    //            long fileLength = myFile.Length;
    //            long startBytes = 0;

    //            int pack = 10240; //10K bytes 
    //            int sleep = (int)Math.Floor((double)(1000 * pack / _speed)) + 1;
    //            if (_Request.Headers["Range"] != null)
    //            {
    //                _Response.StatusCode = 206;
    //                string[] range = _Request.Headers["Range"].Split(new char[] { '=', '-' });
    //                startBytes = Convert.ToInt64(range[1]);
    //            }
    //            _Response.AddHeader("Content-Length", (fileLength - startBytes).ToString());
    //            if (startBytes != 0)
    //            {
    //                _Response.AddHeader("Content-Range", string.Format(" bytes {0}-{1}/{2}", startBytes, fileLength - 1, fileLength));
    //            }
    //            _Response.AddHeader("Connection", "Keep-Alive");
    //            _Response.ContentType = "application/octet-stream";
    //            _Response.AddHeader("Content-Disposition", "attachment;filename=" + HttpUtility.UrlEncode(_fileName, System.Text.Encoding.UTF8));

    //            br.BaseStream.Seek(startBytes, SeekOrigin.Begin);
    //            int maxCount = (int)Math.Floor((double)((fileLength - startBytes) / pack)) + 1;

    //            for (int i = 0; i < maxCount; i++)
    //            {
    //                if (_Response.IsClientConnected)
    //                {
    //                    _Response.BinaryWrite(br.ReadBytes(pack));
    //                    Thread.Sleep(sleep);
    //                }
    //                else
    //                {
    //                    i = maxCount;
    //                }
    //            }
    //        }
    //        catch
    //        {
    //            return false;
    //        }
    //        finally
    //        {
    //            br.Close();
    //            myFile.Close();
    //        }
    //    }
    //    catch
    //    {
    //        return false;
    //    }
    //    return true;
    //}


}
