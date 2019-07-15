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

public partial class materialinfo : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {

        if (Session["view_order_entry_pages"] == null)
        {
            LinkButton2.Visible = false;
        }
        if (Session["view_order_entry_pages"] != null)
        {
            LinkButton1.Visible = false;
        }

        if (Session["view_order_entry_pages_with_estimate"] != null)
        {
            LinkButton1.Visible = false;
            LinkButton2.Visible = false;
        }
        if (Session["view_order_entry_pages_with_estimate"] == null)
        {
            LinkButton3.Visible = false;
        }

        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            UserClass.CheckLogin(Page);

            MatInfoDataSource.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            string vUserId = UserLogin.UserName;
            string vPage = "materialinfo.aspx";
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
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Material Info";
        //ImageButton matinfo = (ImageButton)Master.FindControl("matlinfo");
        //matinfo.ImageUrl = "~/img/matlinfo1.jpg";
    }
    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_entry.aspx");
    }
    protected void LinkButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");
    }

    protected void LinkButton3_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
}

