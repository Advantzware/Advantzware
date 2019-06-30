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

public partial class printreport : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
       
        try
        {
            string path = Convert.ToString(Request.QueryString["file"]);


            if (path != "")
            {                
                string path2 = @"/pdfs/" + path;
                //Response.Write(path2);
                Response.Redirect(path2);
            }
            else
            {
                showlabel.Text = "No PDF Exists";
                showlabel.Visible = true;
            }
        }
        catch
        {
            showlabel.Text = "No PDF Exists";
            showlabel.Visible = true;
        }

    }
}