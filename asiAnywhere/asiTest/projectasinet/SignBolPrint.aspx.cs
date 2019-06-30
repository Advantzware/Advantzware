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

public partial class SignBolPrint : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        showlabel.Visible = false;
        UserClass.CheckLogin(Page);
        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];

            string vUserId = UserLogin.UserName;
            string vPage = "SignBolPrint.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //lblComp.Text = PrmComp;
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        try
        {
            Label print = (Label)FormView1.FindControl("vSignBolFileLabel");


            if (print.Text != "")
            {
                string path = print.Text;

                string path2 = @"/pdfs/" + path;
                //Response.Write(path2);
                Response.Redirect(path2);
            }
            else
            {
                Response.Write("<script>alert('There is no Signed Bol on file')</script>");
                //showlabel.Text = "There is no Signed BOL on file";
                //showlabel.Visible = true;
            }
        }
        catch
        {
            Response.Write("<script>alert('There is no Signed Bol on file'); self.close();</script>");
            //showlabel.Text = "There is no Signed BOL on file";
            //showlabel.Visible = true;
        }
        //Label print = (Label)FormView1.FindControl("aFileLabel");

        //string path = print.Text;
        //Response.Write(path);
        ////if (path != null)
        ////{
        ////string[] radioval = path.Split(new char[] { '\\' });
        ////string a = radioval[0];
        ////string b = radioval[1];
        ////string c= radioval[2];
        ////string d = radioval[3];


        ////Response.Write(a);
        ////Response.Write(b);
        ////Response.Write(c);
        ////Response.Write(d);
        ////Response.Write(path);

        //string path2 = @"/pdfs/" + path;
        //Response.Write(path2);
        ////Response.Redirect(path2);
        ////}
    }
}