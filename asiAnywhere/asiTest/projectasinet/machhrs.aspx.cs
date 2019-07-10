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

public partial class machhrs : System.Web.UI.Page
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
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource4.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            UserClass.CheckLogin(Page);
            string vUserId = UserLogin.UserName;
            string vPage = "machhrs.aspx";
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
        Session["machhr"] = Session["machhr"];
        Session["line"] = Session["line"];
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Mach Hrs";
        //ImageButton machhrs = (ImageButton)Master.FindControl("machhrs");
        //machhrs.ImageUrl = "~/img/machhrs1.jpg";

    }

    //protected void LinkButton1_Click(object sender, EventArgs e)
    //{
    //    string swhere = Request.Form["radio2"];

    //    if (swhere != "" && swhere != null)
    //    {

    //        string[] radioval = swhere.Split(new char[] { ',' });

    //        ObjectDataSource3.SelectParameters["vFormNo"].DefaultValue = radioval[0];
    //        ObjectDataSource3.SelectParameters["vBlankNo"].DefaultValue = radioval[1];
    //        ObjectDataSource3.SelectParameters["prmItem"].DefaultValue = radioval[2];
    //        ObjectDataSource4.SelectParameters["vFormNo"].DefaultValue = radioval[0];
    //        ObjectDataSource4.SelectParameters["vBlankNo"].DefaultValue = radioval[1];
    //        ObjectDataSource4.SelectParameters["prmItem"].DefaultValue = radioval[2];


    //    }
    //}
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["vFormNo"] = GridView1.SelectedRow.Cells[1].Text;
        Session["vBlankNo"] = GridView1.SelectedRow.Cells[2].Text;
        Session["prmItem"] = GridView1.SelectedRow.Cells[3].Text;
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
