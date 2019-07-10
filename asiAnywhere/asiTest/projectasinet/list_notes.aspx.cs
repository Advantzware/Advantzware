#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class list_notes : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        int count = GridView1.Rows.Count;
        Session["contact_rec_key10"] = Convert.ToInt64(Session["contact_rec_key"]) + count;
        if (Session["view_contact_list"] == null)
        {
            lnk_listcontact.Visible = false;
            lilistcnd.Visible = false;
            lnk_viewcontact.Visible = false;
            liviewcnd.Visible = false;
            lnk_MailList.Visible = false;
            limail.Visible = false;
            lnk_calendar.Visible = false;
            licalender.Visible = false;
        }
        if (Session["view_comp_supplier"] == null)
        {
            lnk_listsupplier.Visible = false;
            lilistsp.Visible = false;
            lnk_viewsupplier.Visible = false;
            liviewsp.Visible = false;
        }
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "list_notes.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }


        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {

                lblUser.Text = UserLogin.UserName;

            }
        }
        GridView1.SelectedIndex = Convert.ToInt32(Session["list_notes_index"]);
        try
        {
            if (Session["list_notes_index"] == null)
            {
                GridView1.SelectedIndex = 0;                              

            }
            Session["list_notes_time"] = GridView1.SelectedRow.Cells[2].Text; 
            Session["list_notes_date"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
        }
        catch
        {
            return;
        }

    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void lnk_viewsnotes_click(object sender, EventArgs e)
    {
        Response.Redirect("view_notes.aspx");
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["list_notes_index"] = GridView1.SelectedIndex;
        Session["list_notes_date"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
        Session["list_notes_time"] = GridView1.SelectedRow.Cells[2].Text;

    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }



    protected void lnk_listcontacts_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_list.aspx");
    }
    protected void lnk_viewcontacts_click(object sender, EventArgs e)
    {
        Response.Redirect("view_contacts.aspx");
    }

    protected void lnk_listsupplier_Click(object sender, EventArgs e)
    {
        Response.Redirect("comp_suppliers_list.aspx");
    }
    protected void lnk_viewsupplier_Click(object sender, EventArgs e)
    {
        Response.Redirect("comp_suppliers_viewlist.aspx");
    }
    protected void lnk_MailList_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_maillist.aspx");
    }
    protected void lnk_calendar_click(object sender, EventArgs e)
    {
        Response.Redirect("appointment.aspx");
    }

}