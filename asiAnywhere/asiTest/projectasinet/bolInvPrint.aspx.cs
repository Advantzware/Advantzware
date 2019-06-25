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

public partial class bolInvPrint : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {

        UserClass UserLogin = (UserClass)Session["User"];

         ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        try
        {
            Label print = (Label)FormView1.FindControl("aFileLabel");
            if (print.Text != "")
            {
                string path = print.Text;

                string path2 = @"/pdfs/" + path;
                ////Response.Write(path2);
                Response.Redirect(path2);
            }
            else
            {
                Response.Write("<script>alert('There has not been an Bol created for this order yet'); self.close();</script>");
                //showlabel.Text = "No PDF Exists";
                //showlabel.Visible = true;
            }
        }
        catch
        {
            Response.Write("<script>alert('There has not been an Bol created for this order yet'); self.close();</script>");
            //showlabel.Text = "No PDF Exists";
            //showlabel.Visible = true;
        }

    }
}