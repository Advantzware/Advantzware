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
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Data.SqlClient;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class appointment : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        //Session["Current_date"] = Session["Current_date"];
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "appointment.aspx";
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
        complabel.Text = Convert.ToString(Session["contact_list_cust_name"]);
        custlabel.Text = Convert.ToString(Session["contact_list_first_name"]) + " " + Convert.ToString(Session["contact_list_last_name"]);

        if (!Page.IsPostBack)
        {
            if (Session["Current_date"] == null)
            {
                Session["Current_date"] = DateTime.Now.ToString("MM/dd/yyyy");
                Session["Current_date1"] = DateTime.Now.ToLongDateString();
                datelabel.Text = Convert.ToString(Session["Current_date1"]);
            }
            else
            {
                Session["Current_date"] = Session["Current_date"];
                //Session["Current_date1"] =Session["Current_date"];
                DateTime td = Convert.ToDateTime(Session["Current_date"]);

                datelabel.Text = Convert.ToString(td.ToLongDateString());
            }
            if (Session["User"] != null)
            {
                //    UserClass UserLogin = (UserClass)Session["User"]; 
                lblUser.Text = UserLogin.UserName;
            }
        }
        try
        {
            dbGrid_status.SelectedIndex = Convert.ToInt32(Session["Calendar_grid_index"]);
            if (Session["Calendar_grid_index"] == null)
            {
                dbGrid_status.SelectedIndex = 0;
                Session["status_list_index"] = dbGrid_status.SelectedIndex;
                Session["status_list_code"] = ((Label)dbGrid_status.SelectedRow.FindControl("Label1")).Text;
            }
        }
        catch
        {
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
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void lnk_viewcontacts_click(object sender, EventArgs e)
    {
        Response.Redirect("view_contacts.aspx");
    }
    protected void lnk_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("list_notes.aspx");
    }
    protected void lnk_MailList_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_maillist.aspx");
    }

    protected void lnk_calendar_click(object sender, EventArgs e)
    {
        Response.Redirect("appointment.aspx");
    }
    protected void lnk_listcontact_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_list.aspx");
    }
    //protected void DeleteButton_Click(object sender, EventArgs e)
    //{
    //    Label date = (Label)FormView1.FindControl("date_1Label");
    //    Label time = (Label)FormView1.FindControl("time_1Label");
    //    TextBox reckey = (TextBox)FormView1.FindControl("TextBox1");

    //    Response.Write(date.Text);
    //    Response.Write(time.Text);
    //    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
    //    try
    //    {
    //        conn.Open();
    //        SqlCommand cmd = new SqlCommand("delete from appointment where (date_1='" + date.Text + "') and (time_1='" + time.Text + "')", conn);
    //        cmd.ExecuteNonQuery();
    //        conn.Close();
    //    }
    //    catch
    //    {
    //        conn.Close();
    //    }
    //    finally
    //    {
    //        conn.Close();
    //    }
    //    Response.Write("<script>window.location.href='appointment.aspx'</script>");
    //}

    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        AddNewButton.Visible = true;
        TextBox date = (TextBox)FormView1.FindControl("date_1TextBox");
        TextBox time = (TextBox)FormView1.FindControl("time_1TextBox");
        TextBox reckey = (TextBox)FormView1.FindControl("TextBox1");
        TextBox length_appoint = (TextBox)FormView1.FindControl("length_of_appointmentTextBox");
        TextBox salesrepcode = (TextBox)FormView1.FindControl("sales_rep_codeTextBox");
        TextBox salesrepname = (TextBox)FormView1.FindControl("sales_rep_nameTextBox");
        TextBox meetingname = (TextBox)FormView1.FindControl("meeting_nameTextBox");
        TextBox meetingdesc = (TextBox)FormView1.FindControl("meeting_descriptionTextBox");

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

        try
        {
            conn.Open();
            SqlCommand cmd = new SqlCommand("update appointment set date_1= '" + date.Text + "', time_1= '" + time.Text + "', length_of_appointment='" + length_appoint.Text + "',sales_rep_code='" + salesrepcode.Text + "', sales_rep_name='" + salesrepname.Text + "', meeting_name='" + meetingname.Text + "', meeting_description='" + meetingdesc.Text + "' where  rec_key= '" + reckey.Text + "'", conn);
            cmd.ExecuteNonQuery();
            conn.Close();
        }
        catch
        {
            conn.Close();
        }
        finally
        {
            conn.Close();
        }
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='appointment.aspx'</script>");
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        Response.Write("<script>window.location.href='appointment.aspx'</script>");
        AddNewButton.Visible = true;
    }
    protected void CancelButton_Click(object sender, EventArgs e)
    {
        AddNewButton.Visible = true;
    }
    protected void AddNewButton_Click(object sender, EventArgs e)
    {
        AddNewButton.Visible = false;
        FormView1.ChangeMode(FormViewMode.Insert);

    }
    protected void formview_update_Click(object sender, EventArgs e)
    {
        //AddNewButton.Visible = false;

    }
    protected void FormView_dataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                TextBox salesname = (TextBox)FormView1.FindControl("sales_rep_nameTextBox");
                salesname.Text = Convert.ToString(Session["contact_list_last_name"]);
            }
        }
        catch
        {
        }
    }

    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {

            if (FormView1.DataItemCount == 0)
            {
                if (FormView1.CurrentMode == FormViewMode.Insert)
                {
                    //AddNewButton.Visible = false;
                }
                else
                {
                    //AddNewButton.Visible = true;
                }
            }
            else
            {
                // AddNewButton.Visible = false;
            }
        }
        catch { }
    }


    protected void dbGrid_industry_sic_PageIndexChanging(object source, GridViewPageEventArgs e)
    {
        dbGrid_status.PageIndex = e.NewPageIndex;
        //BuildDataSource();

    }
    protected void dbGrid_status_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["Calendar_grid_index"] = dbGrid_status.SelectedIndex;
        Session["status_list_index"] = dbGrid_status.SelectedIndex;
        Session["status_list_code"] = ((Label)dbGrid_status.SelectedRow.FindControl("Label1")).Text;

    }



    //private void BuildDataSource()
    //{
    //    string date1 = Calendar1.SelectedDate.ToString();
    //    Response.Write(date1);

    //    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
    //    try
    //    {
    //        conn.Open();
    //        string cmd = "select meeting_description as 'Meeting Description' , date_1 as 'Date' from appointment  ";
    //        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
    //        DataSet ds = new DataSet();
    //        da.Fill(ds);

    //        dbGrid_status.DataSource = ds;
    //        dbGrid_status.DataBind();

    //        conn.Close();
    //    }
    //    catch
    //    { return; }
    //    finally
    //    {
    //        conn.Close();
    //    }
    //}



    protected void dbGrid_status_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView rowData;
            rowData = (DataRowView)e.Row.DataItem;

        }

    }



    protected void FormView1_ItemUpdated(object sender, FormViewUpdatedEventArgs e)
    {
        this.dbGrid_status.DataBind();
    }


    protected void Calendar1_SelectionChanged(object sender, EventArgs e)
    {
        datelabel.Text = Calendar1.SelectedDate.ToLongDateString();
        Session["Calendar_grid_index"] = null;
        FormView1.Visible = true;
        GridView1.Visible = false;
        dbGrid_status.Visible = true;
        AddNewButton.Visible = true;
        Session["Current_date"] = Calendar1.SelectedDate.ToString("MM/dd/yyyy");

    }
    protected void FirstUpdateButton_Click(object sender, EventArgs e)
    {
        AddNewButton.Visible = false;
    }
    protected void CancelEdit_Click(object sender, EventArgs e)
    {
        AddNewButton.Visible = true;
    }
    protected void WeekButton_Click(object sender, EventArgs e)
    {
        ToLabel.Text = "To:";
        FromLabel.Text = "From:";
        DayReportLabel.Text = DateTime.Now.ToLongDateString();
        DayReportLabel.BackColor = System.Drawing.Color.Turquoise;
        DayReportLabel2.BackColor = System.Drawing.Color.Turquoise;
        DayReportLabel2.Text = DateTime.Now.AddDays(6).ToLongDateString();
        string date1 = DateTime.Now.ToString("MM/dd/yyyy");

        string date4 = DateTime.Now.AddDays(6).ToString("MM/dd/yyyy");
        Session["Current_date"] = null;
        AddNewButton.Visible = false;
        FormView1.Visible = false;
        datelabel.Visible = false;
        datelabel.Text = "Current Week";
        dbGrid_status.Visible = false;
        GridView1.Visible = true;
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select * from appointment where (sales_rep_name='" + Convert.ToString(Session["contact_list_last_name"]) + "' and (date_1>='" + date1 + "' and date_1<='" + date4 + "'))  ";
            DataSet ds = new DataSet();
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            da.Fill(ds);
            GridView1.DataSource = ds;
            GridView1.DataBind();

        }
        catch
        {
            conn.Close();
        }
        finally
        {
            conn.Close();
        }
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["status_list_code"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;

        AddNewButton.Visible = false;
    }
    protected void DailyButton_Click(object sender, EventArgs e)
    {
        Session["Current_date"] = DateTime.Now.ToString("MM/dd/yyyy");
        FromLabel.Text = "";
        ToLabel.Text = "";
        DayReportLabel.Text = "";
        DayReportLabel2.Text = "";
        Response.Redirect("appointment.aspx");
    }
}
