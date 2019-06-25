#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class View_notes : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        

       try
       {
           SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

           conn.Open();

           string cmd = "select max(convert(bigint,d_rec_key)) from notes where rec_key = '" + Convert.ToString(Session["contact_rec_key"]) + "' ";
           SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
           DataSet ds = new DataSet();
           da.Fill(ds);
           
           Session["contact_rec_key2"] = Convert.ToInt64(ds.Tables[0].Rows[0][0].ToString()) + 1;
           

           conn.Close();
       }
       catch
       {
           Session["contact_rec_key2"] = Convert.ToInt64(Session["contact_rec_key10"]) + 1;
           
       }


        if (Session["view_contact_list"] == null)
        {
            lnk_listcontact.Visible = false;
            lilistcnd.Visible = false;
            lnk_viewcontact.Visible = false;
            liviewcnd.Visible = false;
            lnk_MailList.Visible = false;
            limail.Visible = false;
            lnk_calendar.Visible = false;
            licalendar.Visible = false;
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
            string vPage = "view_notes.aspx";
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
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
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
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["logged_user"] = lblUser.Text;
            }
            if (Session["list_notes_time"] == null)
            {
                AddButton.Visible = true;
            }
            else
            {
                AddButton.Visible = false;
            }

        }






    }
    //protected void Page_Unload(object sender, EventArgs e)
    //{
    //    Session["list_notes_time"] = null;
    //}
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
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_brwsnotes_click(object sender, EventArgs e)
    {
        Response.Redirect("list_notes.aspx");
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            try
            {
                TextBox date = (TextBox)FormView1.FindControl("note_dateTextBox");
                Label time = (Label)FormView1.FindControl("note_timeTextBox");
                Label userid = (Label)FormView1.FindControl("user_idTextBox");
                date.Text = DateTime.Now.ToString("MM/dd/yyyy");
                time.Text = DateTime.Now.ToLongTimeString();
                userid.Text = Convert.ToString(Session["logged_user"]);
            }
            catch
            {
                return;
            }
        }
    }
    protected void AddButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        AddButton.Visible = false;
    }
    //protected void comp_supplier_click(object sender, EventArgs e)
    //{
    //    Response.Redirect("comp_suppliers_list.aspx");
    //}
    //protected void contact_list_click(object sender, EventArgs e)
    //{
    //    Response.Redirect("contact_list.aspx");
    //}

    protected void DeleteButton_Click(object sender, EventArgs e)
    {


        try
        {
            SqlConnection myConn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            myConn.Open();
            SqlCommand cmd = new SqlCommand("delete from notes where [rec_key]='" + Session["contact_rec_key"] + "' and note_time='" + Session["list_notes_time"] + "' and note_date='" + Session["list_notes_date"] + "'", myConn);

            cmd.ExecuteNonQuery();
            myConn.Close();

            Response.Redirect("list_notes.aspx");
        }
        catch
        {
            return;
        }


    }

    protected void Sqldatasource1_Inserted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record updated" + "</b><p>";
            Label time = (Label)FormView1.FindControl("note_timeTextBox");
            Session["list_notes_time"] = time.Text.Trim();
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
    }
    protected void Sqldatasource1_update(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record updated" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
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